#include <R.h>
#include <Rinternals.h>
#include <string.h>

#ifdef __EMSCRIPTEN__
SEXP lm15_ws_open(SEXP url, SEXP headers, SEXP timeout, SEXP ca_bundle) { Rf_error("Use the browser WebSocket bridge in webR."); return R_NilValue; }
SEXP lm15_ws_send(SEXP pointer, SEXP bytes, SEXP offset) { Rf_error("Use the browser WebSocket bridge in webR."); return R_NilValue; }
SEXP lm15_ws_recv(SEXP pointer, SEXP limit) { Rf_error("Use the browser WebSocket bridge in webR."); return R_NilValue; }
SEXP lm15_ws_close(SEXP pointer) { return R_NilValue; }
#else
#include <curl/curl.h>
#if LIBCURL_VERSION_NUM < 0x075600
#error libcurl 7.86.0 or newer is required for verified WebSocket connections
#endif

typedef struct { CURL *handle; struct curl_slist *headers; } connection;
static int initialized = 0;
static size_t discard_body(char *data, size_t size, size_t count, void *opaque) { return size * count; }
static void finalize(SEXP pointer) {
    connection *state = R_ExternalPtrAddr(pointer);
    if (state) {
        if (state->handle) curl_easy_cleanup(state->handle);
        if (state->headers) curl_slist_free_all(state->headers);
        R_Free(state);
        R_ClearExternalPtr(pointer);
    }
}
static connection *get_connection(SEXP pointer) {
    if (TYPEOF(pointer) != EXTPTRSXP || R_ExternalPtrTag(pointer) != Rf_install("lm15_websocket") || !R_ExternalPtrAddr(pointer)) Rf_error("WebSocket connection is closed or invalid.");
    return (connection *) R_ExternalPtrAddr(pointer);
}
SEXP lm15_ws_open(SEXP url, SEXP headers, SEXP timeout, SEXP ca_bundle) {
    if (TYPEOF(url) != STRSXP || XLENGTH(url) != 1 || STRING_ELT(url, 0) == NA_STRING || TYPEOF(headers) != STRSXP) Rf_error("Invalid WebSocket connection arguments.");
    const char *address = Rf_translateCharUTF8(STRING_ELT(url, 0));
    if (strncmp(address, "ws://", 5) && strncmp(address, "wss://", 6)) Rf_error("Expected a WebSocket URL.");
    double seconds = Rf_asReal(timeout);
    if (!R_FINITE(seconds) || seconds <= 0 || seconds > 3600) Rf_error("Invalid WebSocket connection timeout.");
    if (!initialized) {
        if (curl_global_init(CURL_GLOBAL_DEFAULT) != CURLE_OK) Rf_error("Cannot initialize verified WebSocket transport.");
        initialized = 1;
    }
    const curl_version_info_data *version = curl_version_info(CURLVERSION_NOW);
    int supports_ws = 0, supports_wss = 0;
    for (const char * const *protocol = version->protocols; protocol && *protocol; protocol++) {
        if (!strcmp(*protocol, "ws")) supports_ws = 1;
        if (!strcmp(*protocol, "wss")) supports_wss = 1;
    }
    if (!supports_ws || !supports_wss) Rf_error("Installed libcurl lacks WebSocket support; use a WebSocket-enabled build of libcurl.");
    connection *state = R_Calloc(1, connection);
    SEXP pointer = PROTECT(R_MakeExternalPtr(state, Rf_install("lm15_websocket"), R_NilValue));
    R_RegisterCFinalizerEx(pointer, finalize, TRUE);
    state->handle = curl_easy_init();
    if (!state->handle) { finalize(pointer); Rf_error("Cannot allocate WebSocket connection."); }
    for (R_xlen_t i = 0; i < XLENGTH(headers); i++) {
        if (STRING_ELT(headers, i) == NA_STRING) { finalize(pointer); Rf_error("Invalid WebSocket header."); }
        const char *header = Rf_translateCharUTF8(STRING_ELT(headers, i));
        if (strchr(header, '\r') || strchr(header, '\n')) { finalize(pointer); Rf_error("Invalid WebSocket header."); }
        struct curl_slist *next = curl_slist_append(state->headers, header);
        if (!next) { finalize(pointer); Rf_error("Cannot allocate WebSocket headers."); }
        state->headers = next;
    }
#define OPTION(name, value) do { if (curl_easy_setopt(state->handle, name, value) != CURLE_OK) { finalize(pointer); Rf_error("Cannot configure verified WebSocket transport."); } } while (0)
    OPTION(CURLOPT_URL, address);
    OPTION(CURLOPT_PROTOCOLS_STR, "ws,wss");
    OPTION(CURLOPT_CONNECT_ONLY, 2L);
    OPTION(CURLOPT_HTTPHEADER, state->headers);
    OPTION(CURLOPT_SSLVERSION, (long) CURL_SSLVERSION_TLSv1_2);
    OPTION(CURLOPT_SSL_VERIFYPEER, 1L);
    OPTION(CURLOPT_SSL_VERIFYHOST, 2L);
    if (TYPEOF(ca_bundle) == STRSXP && XLENGTH(ca_bundle) == 1 && STRING_ELT(ca_bundle, 0) != NA_STRING) OPTION(CURLOPT_CAINFO, Rf_translateCharUTF8(STRING_ELT(ca_bundle, 0)));
    OPTION(CURLOPT_FOLLOWLOCATION, 0L);
    OPTION(CURLOPT_NOSIGNAL, 1L);
    OPTION(CURLOPT_TIMEOUT_MS, (long) (seconds * 1000));
    OPTION(CURLOPT_HTTP_VERSION, (long) CURL_HTTP_VERSION_1_1);
    OPTION(CURLOPT_WRITEFUNCTION, discard_body);
#undef OPTION
    CURLcode result = curl_easy_perform(state->handle);
    long status = 0;
    curl_easy_getinfo(state->handle, CURLINFO_RESPONSE_CODE, &status);
    if (result != CURLE_OK || status != 101) {
        finalize(pointer);
        SEXP failure = PROTECT(Rf_allocVector(VECSXP, 2));
        SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
        SET_STRING_ELT(names, 0, Rf_mkChar("status"));
        SET_STRING_ELT(names, 1, Rf_mkChar("curl_code"));
        SET_VECTOR_ELT(failure, 0, Rf_ScalarInteger((int) status));
        SET_VECTOR_ELT(failure, 1, Rf_ScalarInteger((int) result));
        Rf_setAttrib(failure, R_NamesSymbol, names);
        SEXP class_name = PROTECT(Rf_mkString("lm15_ws_failure"));
        Rf_setAttrib(failure, R_ClassSymbol, class_name);
        UNPROTECT(4);
        return failure;
    }
    UNPROTECT(1);
    return pointer;
}
SEXP lm15_ws_send(SEXP pointer, SEXP bytes, SEXP offset) {
    connection *state = get_connection(pointer);
    double position = Rf_asReal(offset);
    if (TYPEOF(bytes) != RAWSXP || !R_FINITE(position) || position < 0 || position > XLENGTH(bytes) || position != (R_xlen_t) position) Rf_error("Invalid WebSocket send buffer.");
    size_t sent = 0;
    CURLcode result = curl_ws_send(state->handle, RAW(bytes) + (R_xlen_t) position, (size_t)(XLENGTH(bytes) - position), &sent, 0, CURLWS_TEXT);
    if (result != CURLE_OK && result != CURLE_AGAIN) Rf_error("WebSocket send failed (curl %d); diagnostics suppressed.", (int) result);
    return Rf_ScalarReal((double) sent);
}
SEXP lm15_ws_recv(SEXP pointer, SEXP limit) {
    connection *state = get_connection(pointer);
    unsigned char buffer[65536]; size_t received = 0;
    const struct curl_ws_frame *meta = NULL;
    CURLcode result = curl_ws_recv(state->handle, buffer, sizeof(buffer), &received, &meta);
    if (result == CURLE_AGAIN) return R_NilValue;
    if (result == CURLE_GOT_NOTHING) return Rf_ScalarLogical(FALSE);
    if (result != CURLE_OK || !meta) Rf_error("WebSocket receive failed (curl %d); diagnostics suppressed.", (int) result);
    if (meta->flags & CURLWS_CLOSE) return Rf_ScalarLogical(FALSE);
    if (meta->flags & (CURLWS_PING | CURLWS_PONG)) return R_NilValue;
    if ((double) meta->offset + (double) received + (double) meta->bytesleft > Rf_asReal(limit)) Rf_error("WebSocket frame exceeds the configured limit.");
    SEXP value = PROTECT(Rf_allocVector(RAWSXP, received));
    if (received) memcpy(RAW(value), buffer, received);
    SEXP complete = PROTECT(Rf_ScalarLogical(meta->bytesleft == 0 && !(meta->flags & CURLWS_CONT)));
    Rf_setAttrib(value, Rf_install("complete"), complete);
    UNPROTECT(2);
    return value;
}
SEXP lm15_ws_close(SEXP pointer) {
    if (TYPEOF(pointer) != EXTPTRSXP || R_ExternalPtrTag(pointer) != Rf_install("lm15_websocket")) Rf_error("Invalid WebSocket connection.");
    connection *state = R_ExternalPtrAddr(pointer);
    if (state && state->handle) {
        const unsigned char normal[] = {3, 232}; size_t sent = 0;
        curl_ws_send(state->handle, normal, sizeof(normal), &sent, 0, CURLWS_CLOSE);
    }
    finalize(pointer);
    return R_NilValue;
}
#endif
