c:
	gcc crb.c -o crb -std=c11 -O3 -lm -D_XOPEN_SOURCE=600
	#gcc -Ofast -std=c11 -lm -o crb crb.c -D_XOPEN_SOURCE=600 -march=native -fomit-frame-pointer

all: c
