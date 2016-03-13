c:
	gcc crb.c -o crb -std=c11 -O3 -lm -D_XOPEN_SOURCE=600

all: c
