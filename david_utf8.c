#include "david_utf8.h"

//bytes in a first char of c
size_t utf8_bytes(char* c)
{
    if(*c == 0)
	return 0;

    if(!(((*c)>>7) & 1))
	return 1;
    
    int i = 8;
    for(;(((*c)>>(--i))&1););
    return 7-i;
}

//pointer to a next char
char* utf8_next_char(char* c)
{
    size_t nc = utf8_bytes(c);
    if(nc == 0)
	return NULL;
    return (c+nc);
}

size_t utf8_strlen(char* c)
{
    int rv = 0;
    for(char* r = c; utf8_next_char(r)!=NULL; r = utf8_next_char(r))
	rv++;
    return rv;
}

//first char as a c string
char* utf8_get_char(char* c)
{
    size_t char_size = utf8_bytes(c);
    char* utf8_char = malloc(char_size);
    strncpy(utf8_char, c, char_size);
    return utf8_char;
}

//pointer to nth char
char* utf8_nth_char(char* c, int n)
{
    char *r = c;
    for(int i = 0; (i < n) && (c !=NULL) ; i++)
	r = utf8_next_char(r);

    return r;
}

//nth char as a c string
char* utf8_get_nth_char(char* c, int n)
{
    return utf8_get_char(utf8_nth_char(c, n));
}
