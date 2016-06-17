#gcc -Ofast -std=c11 -lm -o crb crb.c -D_XOPEN_SOURCE=600 -march=native -fomit-frame-pointer
all: crb crb.omp ocamlrb hsrb

.PHONY: crb
crb:
	gcc crb.c -o crb -std=c11 -O3 -lm -D_XOPEN_SOURCE=600

.PHONY: crb.omp
crb.omp:
	gcc crb.omp.c -o crb.omp -std=c11 -O3 -lm -fopenmp -D_XOPEN_SOURCE=600

.PHONY: ocamlrb
ocamlrb:
	ocamlopt -inline 9 -unsafe -ccopt -O9 -o ocamlrb ocamlrb.ml

.PHONY: hsrb
hsrb:
	ghc -O3 -o hsrb hsrb.hs

