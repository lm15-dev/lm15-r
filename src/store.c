#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include <string.h>
#include <stdlib.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <sys/file.h>
#endif

#ifdef _WIN32
static wchar_t *wide_path(SEXP path) {
    const char *s = Rf_translateCharUTF8(STRING_ELT(path, 0));
    int n = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, s, -1, NULL, 0);
    if (!n) Rf_error("Invalid credential file path.");
    wchar_t *out = (wchar_t *) R_alloc(n, sizeof(wchar_t));
    MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, s, -1, out, n);
    return out;
}
#endif

/* The caller holds the canonical-path advisory lock. The temporary file is
   exclusively created beside the destination, so the replacement is atomic. */
static SEXP atomic_write(SEXP destination, SEXP temporary, SEXP bytes) {
    if (TYPEOF(destination) != STRSXP || XLENGTH(destination) != 1 ||
        TYPEOF(temporary) != STRSXP || XLENGTH(temporary) != 1 || TYPEOF(bytes) != RAWSXP)
        Rf_error("Invalid credential write arguments.");
#ifdef _WIN32
    wchar_t *target = wide_path(destination), *temp = wide_path(temporary);
    HANDLE file = CreateFileW(temp, GENERIC_WRITE, 0, NULL, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, NULL);
    if (file == INVALID_HANDLE_VALUE) Rf_error("Cannot create credential temporary file.");
    R_xlen_t offset = 0;
    int ok = 1;
    while (offset < XLENGTH(bytes)) {
        DWORD count = (DWORD)((XLENGTH(bytes) - offset) > 1048576 ? 1048576 : XLENGTH(bytes) - offset);
        DWORD written = 0;
        if (!WriteFile(file, RAW(bytes) + offset, count, &written, NULL) || written == 0) { ok = 0; break; }
        offset += written;
    }
    if (ok) ok = FlushFileBuffers(file);
    if (!CloseHandle(file)) ok = 0;
    if (ok) ok = MoveFileExW(temp, target, MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH);
    if (!ok) { DeleteFileW(temp); Rf_error("Cannot durably replace credential file."); }
#else
    const char *target = Rf_translateChar(STRING_ELT(destination, 0));
    const char *temp = Rf_translateChar(STRING_ELT(temporary, 0));
    char *directory = (char *) R_alloc(strlen(target) + 2, sizeof(char));
    strcpy(directory, target);
    char *slash = strrchr(directory, '/');
    if (slash == directory) slash[1] = '\0';
    else if (slash) *slash = '\0';
    else strcpy(directory, ".");
    int fd = open(temp, O_WRONLY | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
    if (fd < 0) Rf_error("Cannot create private credential temporary file.");
    R_xlen_t offset = 0;
    int ok = 1;
    while (offset < XLENGTH(bytes)) {
        size_t count = (size_t)((XLENGTH(bytes) - offset) > 1048576 ? 1048576 : XLENGTH(bytes) - offset);
        ssize_t written = write(fd, RAW(bytes) + offset, count);
        if (written < 0 && errno == EINTR) continue;
        if (written <= 0) { ok = 0; break; }
        offset += written;
    }
    if (ok && fsync(fd) != 0) ok = 0;
    if (close(fd) != 0) ok = 0;
    if (ok && rename(temp, target) != 0) ok = 0;
    if (!ok) { unlink(temp); Rf_error("Cannot durably replace credential file."); }
    /* Persist the rename as well as the contents across a power failure. */
    int parent = open(directory, O_RDONLY);
    if (parent < 0) Rf_error("Cannot open credential directory for synchronization.");
    int synced = fsync(parent);
    close(parent);
    if (synced != 0) Rf_error("Cannot synchronize credential directory.");
#endif
    return R_NilValue;
}


/* AUTH-4 advisory lock on the canonical path's lock file, the same primitive
   every lm15 SDK uses so that processes of different languages exclude each
   other: flock(2) on POSIX (not fcntl record locks, which Linux keeps
   independent of flock), and an exclusive lock on byte 0 on Windows (what
   msvcrt.locking and LockFileEx both take). Non-blocking: R polls, so a wait
   stays interruptible and bounded by the caller's timeout. */
typedef struct {
#ifdef _WIN32
    HANDLE handle;
#else
    int fd;
#endif
} lm15_lock;

static void lock_release(lm15_lock *lock) {
    if (!lock) return;
#ifdef _WIN32
    if (lock->handle != INVALID_HANDLE_VALUE) {
        OVERLAPPED ov; memset(&ov, 0, sizeof ov);
        UnlockFileEx(lock->handle, 0, 1, 0, &ov);
        CloseHandle(lock->handle);
        lock->handle = INVALID_HANDLE_VALUE;
    }
#else
    if (lock->fd >= 0) {
        flock(lock->fd, LOCK_UN);
        close(lock->fd);
        lock->fd = -1;
    }
#endif
}

static void lock_finalize(SEXP pointer) {
    lm15_lock *lock = (lm15_lock *) R_ExternalPtrAddr(pointer);
    if (!lock) return;
    lock_release(lock);
    free(lock);
    R_ClearExternalPtr(pointer);
}

static SEXP lock_try(SEXP path) {
    if (TYPEOF(path) != STRSXP || XLENGTH(path) != 1) Rf_error("Invalid lock path.");
    lm15_lock *lock = (lm15_lock *) malloc(sizeof(lm15_lock));
    if (!lock) Rf_error("Cannot allocate a credential lock.");
#ifdef _WIN32
    wchar_t *target = wide_path(path);
    lock->handle = CreateFileW(target, GENERIC_READ | GENERIC_WRITE, FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
                               NULL, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    if (lock->handle == INVALID_HANDLE_VALUE) { free(lock); Rf_error("Cannot open the credential lock file."); }
    OVERLAPPED ov; memset(&ov, 0, sizeof ov);
    if (!LockFileEx(lock->handle, LOCKFILE_EXCLUSIVE_LOCK | LOCKFILE_FAIL_IMMEDIATELY, 0, 1, 0, &ov)) {
        DWORD code = GetLastError();
        CloseHandle(lock->handle); free(lock);
        if (code == ERROR_LOCK_VIOLATION || code == ERROR_IO_PENDING) return R_NilValue;
        Rf_error("The filesystem could not acquire a credential lock.");
    }
#else
    const char *target = Rf_translateChar(STRING_ELT(path, 0));
    int flags = O_RDWR | O_CREAT;
#ifdef O_CLOEXEC
    flags |= O_CLOEXEC;
#endif
    lock->fd = open(target, flags, S_IRUSR | S_IWUSR);
    if (lock->fd < 0) { free(lock); Rf_error("Cannot open the credential lock file."); }
    if (flock(lock->fd, LOCK_EX | LOCK_NB) != 0) {
        int code = errno;
        close(lock->fd); free(lock);
        if (code == EWOULDBLOCK || code == EAGAIN || code == EINTR) return R_NilValue;
        Rf_error("The filesystem could not acquire a credential lock; use a local locking-capable filesystem or an explicit credential.");
    }
#endif
    SEXP pointer = PROTECT(R_MakeExternalPtr(lock, Rf_install("lm15_lock"), R_NilValue));
    R_RegisterCFinalizerEx(pointer, lock_finalize, TRUE);
    UNPROTECT(1);
    return pointer;
}

static SEXP lock_release_call(SEXP pointer) {
    if (TYPEOF(pointer) != EXTPTRSXP || R_ExternalPtrTag(pointer) != Rf_install("lm15_lock")) Rf_error("Invalid credential lock.");
    lock_finalize(pointer);
    return R_NilValue;
}

extern SEXP lm15_ws_open(SEXP, SEXP, SEXP, SEXP);
extern SEXP lm15_ws_send(SEXP, SEXP, SEXP);
extern SEXP lm15_ws_recv(SEXP, SEXP);
extern SEXP lm15_ws_close(SEXP);

static const R_CallMethodDef calls[] = {
    {"C_lm15_ws_open", (DL_FUNC) &lm15_ws_open, 4},
    {"C_lm15_ws_send", (DL_FUNC) &lm15_ws_send, 3},
    {"C_lm15_ws_recv", (DL_FUNC) &lm15_ws_recv, 2},
    {"C_lm15_ws_close", (DL_FUNC) &lm15_ws_close, 1},
    {"C_lm15_atomic_write", (DL_FUNC) &atomic_write, 3},
    {"C_lm15_lock_try", (DL_FUNC) &lock_try, 1},
    {"C_lm15_lock_release", (DL_FUNC) &lock_release_call, 1},
    {NULL, NULL, 0}
};
void attribute_visible R_init_lm15(DllInfo *dll) {
    R_registerRoutines(dll, NULL, calls, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
