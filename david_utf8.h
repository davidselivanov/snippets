#ifndef DAVID_UTF8_H
#define DAVID_UTF8_H

#include <sys/types.h>
#include <stdlib.h>
#include <memory.h>

//bytes in a first char of c
size_t utf8_bytes(char* c);

//pointer to a next char
char* utf8_next_char(char* c);

size_t utf8_strlen(char* c);

//first char as a c-string
char* utf8_get_char(char* c);

//pointer to nth char
char* utf8_nth_char(char* c, int n);

//nth char as a c-string
char* utf8_get_nth_char(char* c, int n);

#endif
