# raybench

A simple implementation of a path tracer in different programming languages with some restrictions.

Checkout my [blog posts](http://eccentricdevelopments.com/programming-language-comparison-summary/) about the development of this project.

## Restrictions
**Code should be simple,** as in, easy to read and understand.

**A single file** per implementation, whenever possible.

**Intersection finding algorithms** must be the same in all implementations.

**Brute force** algorithm, no acceleration structures.

**If the language allows** vectorization, parallel or distributed processing, those must be used in different implementations, i.e. crb (C single threaded), cmprb (C using OpenMP, not yet implemented).

**Code must follow best practices** in formating and structure, according to the language in use.

### Technical details
* Output resolution: 1280x720
* Max Depth: 5
* Samples: 50
* Only spheres
* Image format: PPM

## Other Info
The C language implementation will be used as baseline, crb.c. 

This implementation takes around 2 minutes to render the scene on a Basic A1 Microsoft Azure VM instance. That means 1 Core, 1.75 GB RAM, Ubuntu Server 14.04 LTS.

```
processor       : 0
vendor_id       : GenuineIntel
cpu family      : 6
model           : 63
model name      : Intel(R) Xeon(R) CPU E5-2673 v3 @ 2.40GHz
stepping        : 2
microcode       : 0xffffffff
cpu MHz         : 2394.403
cache size      : 30720 KB
physical id     : 0
siblings        : 1
core id         : 0
cpu cores       : 1
apicid          : 0
initial apicid  : 0
fpu             : yes
fpu_exception   : yes
cpuid level     : 13
wp              : yes
flags           : fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca cmov pat pse36 clflush mmx fxsr sse sse2 ss syscall nx lm constant_tsc rep_good nopl eagerfpu pni pclmulqdq ssse3 fma cx16 sse4_1 sse4_2 movbe popcnt aes xsave avx f16c rdrand hypervisor lahf_lm abm fsgsbase bmi1 avx2 smep bmi2 erms xsaveopt
bugs            :
bogomips        : 4788.80
clflush size    : 64
cache_alignment : 64
address sizes   : 42 bits physical, 48 bits virtual
power management:
```

## Implementations

C (crb.c) (gcc (Ubuntu 7.4.0-1ubuntu1~18.04) 7.4.0)

```
$ gcc crb.c -o crb -std=c11 -O3 -lm -D_XOPEN_SOURCE=600
```

```
$ time ./crb

real    1m32.039s
user    1m30.197s
sys     0m1.039s
```

C with vector extensions (crb-vec.c)

```
$ gcc crb-vec.c -o crb-vec -std=c11 -O3 -lm -D_XOPEN_SOURCE=600
```

```
$ time ./crb-vec

real    1m15.193s
user    1m13.722s
sys     0m0.893s
```

C Double Precision (crb-dbl.c)

```
$ gcc crb-dbl.c -o crb-dbl -std=c11 -O3 -lm -D_XOPEN_SOURCE=600
```

```
$ time ./crb-dbl 

real	2m26.546s
user	2m25.856s
sys	  0m0.104s
```

Lua (luarb.lua)
```
$ time lua luarb.lua 

real    611m38.925s
user    605m54.136s
sys     0m37.716s
```
```
$time luajit luarb.lua 

real    225m58.621s
user    224m2.140s
sys     0m14.044s
```
JavaScript (jsrb.js)

*node.js v5.7.1*
```
$ time node jsrb.js 

real    8m49.170s
user    8m40.620s
sys     0m4.488s
```

*node.js v6.2.1*
```
$ time node jsrb.js 

real    7m59.041s
user    7m50.412s
sys     0m5.300s
```

C# (csrb.js)
```
$ mcs csrb.cs
```

```
$ time mono csrb.exe 

real    12m18.463s
user    12m0.392s
sys     0m11.488s
```

Haskell (hsrb.hs)
```
$ ghc -O3 -o hsrb hsrb.hs
```
```
$ time ./hsrb

real    26m34.955s
user    26m5.968s
sys     0m21.108s
```

Python (pyrb.py)
```
$ time python pyrb.py 

real    348m35.965s
user    345m51.776s
sys     0m22.880s
```
```
$ time pypy pyrb.py

real    14m2.406s
user    13m55.292s
sys     0m1.416s
```

Lisp (lisprb.lisp)
```
$ time sbcl --script lisprb.lisp 

real    24m43.216s
user    24m27.772s
sys     0m4.972s
```

OCaml (ocamlrb.ml)

```
$ ocamlopt -inline 9 -unsafe -ccopt -O9 -o ocamlrb ocamlrb.ml
```

```
$ time ./ocamlrb 

real	3m59.597s
user	3m58.036s
sys	  0m0.432s
```

Elixir
```
$ time elixir elixirrb.exs

real    123m59.025s
user    118m2.504s
sys     4m16.504s
```

Elixir MP
```
$ time elixir elixirrb.mp.exs

real    138m48.241s
user    132m20.264s
sys     4m54.372s
```

Go (version 1.6)
```
$ time ./gorb

real    6m44.151s
user    6m35.732s
sys     0m4.144s
```

Nim (version 0.14.2)
```
$ nim c --boundChecks:off --floatChecks:off --opt:speed -d:release nimrb.nim
```
```
$ time ./nimrb

real    1m53.320s
user    1m52.236s
sys     0m0.144s
```

Nim Double Precision (version 0.14.2)
```
$ nim c --boundChecks:off --floatChecks:off --opt:speed -d:release nimrb_dbl.nim
```
```
$ time ./nimrb_dbl 

real	3m19.547s
user	3m18.432s
sys	  0m0.268s
```

Crystal (version 0.18.7)
```
$ crystal build --release crrb.cr
```

```
$ time ./crrb 

real	2m1.735s
user	2m1.116s
sys	0m0.140s
```

Rust (rustc 1.34.1 (fc50f328b 2019-04-24)), thanks to [tilpner](https://github.com/tilpner)
```
$ time cargo script rsrb.rs -d rand=0.3.14
Progress: 99%
real    1m12.964s
user    1m10.887s
sys     0m1.410s
```

Rust Alt (rustc 1.34.1 (fc50f328b 2019-04-24))
```
$ time cargo script rsrb_alt.rs -d rand=0.3.14

real    1m15.509s
user    1m13.896s
sys     0m0.962s
```

Java (1.7.0_111, OpenJDK IcedTea 2.6.7)
```
$ time java javarb 

real	2m36.949s
user	2m34.344s
sys	  0m1.832s
```
Julia (version 0.4.6), thanks to [pierroelmito](https://github.com/pierroelmito)
```
$ time julia jlrb.jl 
118.540627 seconds (286.21 k allocations: 22.626 MB, 0.08% gc time)

real	2m1.166s
user	2m0.324s
sys	  0m0.260s
```
```
