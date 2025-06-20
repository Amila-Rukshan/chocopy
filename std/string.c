#include <stdlib.h>
#include <string.h>

char* strconcat(const char* str1, const char* str2) {
  size_t len1 = strlen(str1);
  size_t len2 = strlen(str2);
  char* result = (char*)malloc(len1 + len2 + 1);
  if (!result)
    return NULL;
  strcpy(result, str1);
  strcat(result, str2);
  return result;
}

char* stridx(const char* str, int index) {
  size_t len = strlen(str);
  if (index < 0 || index >= len) {
    return NULL;
  }

  char* strchar = (char*)malloc(2);
  if (!strchar)
    return NULL;

  strchar[0] = str[index];
  strchar[1] = '\0';
  return strchar;
}

int strlength(const char* str) { return strlen(str); }
