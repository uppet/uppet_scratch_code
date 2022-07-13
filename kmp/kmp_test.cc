#include<stdio.h>
#include "kmp.h"

#define KMP_EXPECT(v) test_expect(v, #v, __FILE__, __LINE__)

#define KMP_EXPECT_INT_ARRAY(arr, arr_expected, len, info) test_expect_array(arr, arr_expected, len, info, __FILE__, __LINE__)

#define KMP_EXPECT_INT_EQ(ia, ie, info) test_expect_int_eq(ia, ie, info, __FILE__, __LINE__)

static int test_fail_count = 0;

void test_expect(bool expected_true, const char * cond, const char * fn, int fl) {
  if (!expected_true) {
    test_fail_count ++;
    printf("FAIL: (%*d) expected %s as true at %s:%d\n", 4, test_fail_count, cond, fn, fl);
  }
}

void test_expect_array(int* arr, int* arr_expected, int len, const char *info, const char * fn, int fl) {
  for (int idx=0; idx<len; idx++) {
    if (arr[idx] != arr_expected[idx]) {
      test_fail_count ++;
      printf("FAIL: (%*d) expected array mismatch at IDX[%d] as true at %s:%d\n", 4, test_fail_count, idx, fn, fl);
      printf(" %d SEEN but %d EXPECTED\n", arr[idx], arr_expected[idx]);
      return;
    }
  }
}

void test_expect_int_eq(int ia, int ie, const char *info, const char * fn, int fl) {
  if (ia != ie) {
    test_fail_count ++;
    printf("FAIL: (%*d) expected %d as %d at %s:%d\n", 4, test_fail_count, ia, ie, fn, fl);
  }
}

void test_build_next() {
  const char* c1 = "ABBA";
  int cn1[] = {0, 1, 0, 1};
  int cn1e[] = {0, 0, 0, 1};
  kmp_make_next(c1, cn1);
  KMP_EXPECT_INT_ARRAY(cn1, cn1e, 4, c1);

  const char* c2 = "AABA";
  int cn2[] = {1, 0, 0, 1};
  int cn2e[] = {0, 1, 0, 1};
  kmp_make_next(c2, cn2);
  KMP_EXPECT_INT_ARRAY(cn2, cn2e, 4, c2);

  const char* c3 = "ABACABAB";
  int c3n[] = {0, 0, 0, 1, 0, 0, 0, 1};
  int c3ne[] = {0, 0, 1, 0, 1, 2, 3, 2};
  kmp_make_next(c3, c3n);
  KMP_EXPECT_INT_ARRAY(c3n, c3ne, 4, c3);
}

void test_kmp_index() {
  const char* t1s = "good morning";
  const char* t1p = "good";
  KMP_EXPECT_INT_EQ(kmp_index(t1s, t1p), 0, "findt1");

  const char* t2s = "good morning";
  const char* t2p = "morning";
  KMP_EXPECT_INT_EQ(kmp_index(t2s, t2p), 5, "findt2");


  const char* t3s = "goo ABACABAD ABACABABgood morning";
  const char* t3p = "ABACABAB";
  KMP_EXPECT_INT_EQ(kmp_index(t3s, t3p), 13, "findt3");

  const char* t4s = "goo ABACABAD ABACABABgood morning";
  const char* t4p = "ABAD";
  KMP_EXPECT_INT_EQ(kmp_index(t4s, t4p), 8, "findt4");

  const char* t5s = "goo ABACABAD ABACABABgood morning";
  const char* t5p = "ABADX";
  KMP_EXPECT_INT_EQ(kmp_index(t5s, t5p), -1, "findt5");


}

void test_total_report() {
    printf("%*d test(s) failed.\n", 4, test_fail_count);
    if (test_fail_count == 0) {
	printf("All passed.\n");
    }
}

int main() {
  int v[] = {1,2,3};
  int ve[] = {1,2,3};
  KMP_EXPECT_INT_ARRAY(v, ve, 3, "simple int array");
  KMP_EXPECT(true);
  test_build_next();
  test_kmp_index();
  test_total_report();
  return 0;
}
