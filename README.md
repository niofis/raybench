# raybench

A simple implementation of a path tracer in different programming languages with some restrictions.

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

C (crb.c)

```
$ gcc crb.c -o crb -std=c11 -O3 -lm -D_XOPEN_SOURCE=600
```

```
$ time ./crb

real    1m59.116s
user    1m58.216s
sys     0m0.112s
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

```
$ time node jsrb.js 

real    8m49.170s
user    8m40.620s
sys     0m4.488s
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
$ ocamlopt -o ocamlrb -unsafe ocamlrb.ml
```

```
$ time ./ocamlrb

real    5m38.974s
user    5m35.960s
sys     0m0.532s
```
