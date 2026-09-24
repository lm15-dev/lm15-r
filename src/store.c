#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include <string.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
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
    {NULL, NULL, 0}
};
void attribute_visible R_init_lm15(DllInfo *dll) {
    R_registerRoutines(dll, NULL, calls, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
