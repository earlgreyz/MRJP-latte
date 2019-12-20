#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printInt(int n) {
  printf("%d\n", n);
}

void printString(const char *s) {
  puts(s);
}

int readInt() {
  int n;
  scanf("%d ", &n);
  return n;
}

const char* readString() {
  char *buffer = NULL;
  size_t bufferSize = 0;
  size_t n = getline(&buffer, &bufferSize, stdin);
  if (n > 0 && buffer[n - 1] == '\n') {
    buffer[n - 1] = '\0';
  }
  return buffer;
}

bool stringsEqual(const char *s, const char *t) {
  return strcmp(s, t) == 0;
}

const char *stringsConcat(const char *s, const char *t) {
  int n = strlen(s);
  int m = strlen(t);
  char *buffer = malloc(sizeof(char) * (n + m + 1));
  strcpy(buffer, s);
  strcat(buffer, t);
  return buffer;
}
