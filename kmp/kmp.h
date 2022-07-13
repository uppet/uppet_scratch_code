#ifndef __KMP_H__
#define __KMP_H__
int kmp_index(const char * s, const char * pat);
void kmp_make_next(const char *pat, int* next);

#endif //  __KMP_H__
