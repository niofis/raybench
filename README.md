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
gcc crb.c -o crb -std=c11 -O3 -lm -D_XOPEN_SOURCE=600
```

```
$time ./crb

real	2m1.940s
user	2m1.212s
sys	0m0.180s
```
Lua (luarb.lua)

```
$time luajit luarb.lua 

real    225m58.621s
user    224m2.140s
sys     0m14.044s
```