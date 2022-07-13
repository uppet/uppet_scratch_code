
DBGFLAG=-g
CXX=g++
LD=g++
.PHONY: all test clean
all:kmp test
	echo all done

run:all
	./kmp

test:kmp_test

run_test:kmp_test
	./kmp_test

debug_test:kmp_test
	gdb ./kmp_test


kmp:kmp.o kmp_impl.o
	${LD} -o $@ $^

kmp_test:kmp_test.o kmp_impl.o
	${LD} -o $@  $^

.cc.o:
	${CXX} ${DBGFLAG} -c $<

clean:
	rm -f *.o *~ kmp kmp_test
