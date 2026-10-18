// Runtime library for ChocoPy programs compiled to WebAssembly.
//
// Freestanding: no libc is linked, so this file provides the C functions the
// generated code calls (puts, printf, malloc, strcmp) along with the ChocoPy
// support functions. -fno-builtin keeps clang from turning its loops into
// memcpy/memset calls, which nothing here defines. The only imports are WASI's fd_write and proc_exit, so
// programs run under any WASI host (wasmtime, Node's node:wasi) or in the
// browser with a small shim.

#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>

typedef struct {
  const char* buf;
  size_t len;
} ciovec;

__attribute__((import_module("wasi_snapshot_preview1"), import_name("fd_write")))
int32_t wasi_fd_write(int32_t fd, const ciovec* iovs, size_t iovsLen,
                      size_t* written);

__attribute__((import_module("wasi_snapshot_preview1"), import_name("proc_exit"),
               noreturn))
void wasi_proc_exit(int32_t code);

// The program's top-level code. The compiler emits it under this name on wasm
// since clang renames any C function called main.
int chocopy_main(void);

// ---- Memory ----

// Bump allocator over linear memory, starting at the end of the data and
// stack (__heap_base is defined by wasm-ld). ChocoPy has no free.
extern unsigned char __heap_base;
static uintptr_t heapTop = (uintptr_t)&__heap_base;

#define WASM_PAGE_SIZE 65536

void* malloc(size_t size) {
  uintptr_t start = (heapTop + 7) & ~(uintptr_t)7;
  uintptr_t end = start + size;
  size_t memoryEnd = __builtin_wasm_memory_size(0) * WASM_PAGE_SIZE;
  if (end > memoryEnd) {
    size_t pages = (end - memoryEnd + WASM_PAGE_SIZE - 1) / WASM_PAGE_SIZE;
    if (__builtin_wasm_memory_grow(0, pages) == (size_t)-1)
      return NULL;
  }
  heapTop = end;
  return (void*)start;
}

// ---- Strings ----

static size_t length(const char* str) {
  const char* end = str;
  while (*end)
    ++end;
  return end - str;
}

int strcmp(const char* str1, const char* str2) {
  while (*str1 && *str1 == *str2) {
    ++str1;
    ++str2;
  }
  return (unsigned char)*str1 - (unsigned char)*str2;
}

char* strconcat(const char* str1, const char* str2) {
  char* result = malloc(length(str1) + length(str2) + 1);
  if (!result)
    return NULL;
  char* p = result;
  while (*str1)
    *p++ = *str1++;
  while ((*p++ = *str2++))
    ;
  return result;
}

char* stridx(const char* str, int index) {
  size_t len = length(str);
  if (index < 0 || (size_t)index >= len)
    return NULL;

  char* strchar = malloc(2);
  if (!strchar)
    return NULL;

  strchar[0] = str[index];
  strchar[1] = '\0';
  return strchar;
}

int strlength(const char* str) { return length(str); }

// ---- Output ----

static void write(const char* buf, size_t len) {
  ciovec iov = {buf, len};
  size_t written;
  wasi_fd_write(1, &iov, 1, &written);
}

static void writeStr(const char* str) { write(str, length(str)); }

static void writeInt(int value) {
  char buf[12];
  char* p = buf + sizeof(buf);
  // Negate as unsigned so INT_MIN does not overflow
  unsigned magnitude = value < 0 ? -(unsigned)value : (unsigned)value;
  do {
    *--p = '0' + magnitude % 10;
    magnitude /= 10;
  } while (magnitude);
  if (value < 0)
    *--p = '-';
  write(p, buf + sizeof(buf) - p);
}

int puts(const char* str) {
  writeStr(str);
  write("\n", 1);
  return 0;
}

// Supports the conversions the generated code uses: %d, %s and %%.
int printf(const char* fmt, ...) {
  va_list args;
  va_start(args, fmt);
  const char* literal = fmt;
  for (const char* p = fmt; *p; ++p) {
    if (*p != '%')
      continue;
    write(literal, p - literal);
    switch (*++p) {
    case 'd':
      writeInt(va_arg(args, int));
      break;
    case 's':
      writeStr(va_arg(args, const char*));
      break;
    case '%':
      write("%", 1);
      break;
    case '\0':
      --p;
      break;
    }
    literal = p + 1;
  }
  write(literal, length(literal));
  va_end(args);
  return 0;
}

void runtime_check(int cond, const char* msg, const char* file, int line,
                   int col) {
  if (!cond) {
    printf("%s:%d:%d: %s\n", file, line, col, msg);
    wasi_proc_exit(1);
  }
}

// ---- Entry point ----

// The WASI command entry point
void _start(void) {
  int code = chocopy_main();
  if (code != 0)
    wasi_proc_exit(code);
}
