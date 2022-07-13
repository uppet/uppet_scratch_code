#include "kmp.h"

#define KMP_TRACE_LOG 1
#if KMP_TRACE_LOG
#include "stdio.h"
#endif

#define MAX_LEN  1024
int kmp_index(const char * s, const char * pat) {
    int next[MAX_LEN];
    next[0] = 0;
    kmp_make_next(pat, next);

    int index_s = 0;
    int index_pat = 0;
    int found = -1;

#if KMP_TRACE_LOG
    printf("SEARCH [%s] for [%s]\n", s, pat);
#endif

    while (s[index_s] != 0 && pat[index_pat] != 0) {
	if (s[index_s] == pat[index_pat]) { // match head
	    if (pat[index_pat+1] == 0) {
		found = index_s - index_pat;
#if KMP_TRACE_LOG
		printf("FOUND at:%d, %d\n", index_s, index_pat);
#endif
		break;
	    }
#if KMP_TRACE_LOG
	    printf("back continue _ at:%d, %d\n", index_s, index_pat);
#endif
	    index_s++;
	    index_pat++;
	} else {
	    int next_idx = index_pat - 1;
	    if (next_idx < 0)
		next_idx = 0;
	    //int back_pat = next[index_pat];
	    int back_pat = next[next_idx];
	    if (back_pat == 0) {
		index_pat = 0;
		index_s++;
#if KMP_TRACE_LOG
		printf("back continue A at:%d, %d\n", index_s, index_pat);
#endif
	    } else {
		index_pat = back_pat;
#if KMP_TRACE_LOG
		printf("back continue B at:%d, %d\n", index_s, index_pat);
#endif
	    }
	}
    }
    return found;
}

void kmp_make_next(const char *pat, int* next) {
    if (pat[0] == 0) {
	return;
    }
    next[0] = 0;
    int next_len = 0;
    int i = 1;
    while (pat[i] != 0) {
	int back = next[i-1];
	if (pat[back] == pat[i]) {
	    next_len++;
	    next[i] = next_len;
	    i++;
	} else {
	    int next_len_idx = next_len - 1;
	    if (next_len_idx < 0)
		next_len_idx = 0;
	    next_len = next[next_len_idx];
	    if (next_len == 0) {
		next[i] = next_len;
		i++;
	    }
	}
    }
}

