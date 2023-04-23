USING: kernel math math.functions sequences assocs prettyprint classes.tuple accessors math.functions math.bitwise arrays sequences.repeating generalizations formatting math.order compiler ;
IN: factorrb
USING: factorrb ;

: v-add ( seq1 seq2 -- seq ) zip [ first2 + ] map ;
: v-sub ( seq1 seq2 -- seq ) zip [ first2 - ] map ;
: v-mul ( seq1 seq2 -- seq ) zip [ first2 * ] map ;
: v-scale ( seq1 n -- seq ) swap [ * ] with map ;
: v-div ( seq1 n -- seq ) 1 swap /f v-scale ;
: dot ( seq1 seq2 -- n ) v-mul 0 [ + ] reduce ;
: norm ( seq -- n ) dup dot sqrt ;
: unit ( seq -- seq ) dup norm 1 swap / v-scale ;

TUPLE: ray 
    { origin sequence read-only initial: { 0.0 0.0 0.0 } }
    { direction sequence read-only initial: { 0.0 0.0 0.0 } } ;

: <ray> ( origin direction -- ray ) ray boa ;

: ray-point ( ray distance -- seq )
    [ [ origin>> ] keep ] dip
    [ direction>> ] dip
    v-scale
    v-add ;

TUPLE: sphere
    { center sequence read-only initial: { 0.0 0.0 0.0 } }
    { radius float read-only initial: 1.0 } 
    { color sequence read-only initial: { 1.0 .0 0.0 } } 
    { is-light boolean read-only initial: f } ;

: <sphere> ( center radius color is-light -- sphere ) sphere boa ;

TUPLE: hit
    { sphere object }
    { distance float read-only initial: 3.4028237e28 }
    { point sequence read-only initial: { 0.0 0.0 0.0 } }
    { normal sequence read-only initial: { 0.0 0.0 0.0 } } 
    { did-hit boolean read-only initial: f } ;

: <hit> ( object distance seq seq did-hit -- hit ) hit boa ;

: sphere-hit ( ray sphere distance -- hit )
    dup
    [ rot ] dip
    ray-point
    [ [ center>> ] keep swap ] 2dip
    rot
    [ dup ] dip
    v-sub
    unit
    t
    <hit> ;

TUPLE: camera
    { eye sequence read-only initial: { 0.0 4.5 75.0 } }
    { lt sequence read-only initial: { -8.0 9.0 50.0 } }
    { rt sequence read-only initial: { 8.0 9.0 50.0 } }
    { lb sequence read-only initial: { -8.0 0.0 50.0 } } ;

: <camera> ( -- camera ) camera new ;



TUPLE: world
    { camera camera read-only }
    { spheres sequence read-only } ;

: <world> ( -- world )
    <camera>
    { 
        { { 0.0 -10002.0 0.0 } 9999.0 { 1.0 1.0 1.0 } f }
        { { -10012.0 0.0 0.0 } 9999.0 { 1.0 0.0 0.0 } f }
        { { 10012.0 0.0 0.0 } 9999.0 { 0.0 1.0 0.0 } f }
        { { 0.0 0.0 -10012.0 } 9999.0 { 1.0 1.0 1.0 } f }
        { { 0.0 10012.0 0.0 } 9999.0 { 1.0 1.0 1.0 } t }
        { { -5.0 0.0 2.0 } 2.0 { 1.0 1.0 0.0 } f }
        { { 0.0 5.0 -1.0 } 4.0 { 1.0 0.0 0.0 } f }
        { { 8.0 5.0 -1.0 } 2.0 { 0.0 0.0 1.0 } f }
    } [ first4 <sphere> ] map
    world
    boa ;

TUPLE: random
    { state sequence initial: { 123456789 362436069 521288629 88675123 } } ;

: next-random ( random -- random n )
    dup
    state>>
    unclip
    [ dup ] dip
    dup 11 shift bitxor
    32 bits
    dup -8 shift bitxor
    [ last ] dip dup -19 shift bitxor
    bitxor
    32 bits
    dup
    4294967295.0 /
    [ suffix ] dip
    [ >>state ] dip ;

: next-random-v ( random -- random seq )
    next-random
    [ next-random ] dip
    [ next-random ] 2dip
    3array
    [ 2 * 1 - ] map ;

: random-dome ( random normal -- random vector )
    { 0 0 0 } ! what you have to do to keep the invariant >.<
    [
        drop
        [ next-random-v ] dip swap
        unit
        [ dup dot ] keep swap
        0.0 <
    ] loop swap drop ;

: sphere-intersect ( ray sphere -- hit )
    [ [ origin>> ] keep swap ] dip swap ! origin is at the top of the stack
    [ [ center>> ] keep swap ] dip ! center is at the top of the stack
    swap v-sub ! oc
    [ [ direction>> ] keep swap ] 2dip [ swap ] dip swap ! direction is at the top of the stack
    dup [ dot ] keep ! a is second place in the stack
    [ swap ] dip ! oc is second place in the stack direction is on top
    swap [ dot ] keep ! b is second place in the stack oc is on top
    dup dot
    [ [ radius>> ] keep ] 3dip
    [ rot ] 2dip rot ! radius is top of the stack
    dup * - ! c is top of the stack
    [ [ dup * ] keep swap ] dip ! b * b is second place in the stack
    [ dup ] 3dip [ swap ] 2dip rot ! a is top of the stack
    * ! a * c
    - ! dis is top of the stack
    dup 0 >
    [
        ! true
        sqrt ! e
        [ neg ] dip
        [ + swap / ] 3keep ! t2 is second place in the stack
        - swap / ! t1 is top of the stack
        dup 0.007 >
        [
            ! true
            nip
            sphere-hit
        ]
        [
            drop
            dup 0.007 >
            [
                ! true
                sphere-hit
            ]
            [
                3drop
                hit new
            ] if
        ] if
    ]
    [
        ! else
        5drop
        hit new
    ] if ;

: trace ( random ray spheres depth -- random color )
    dup
    5 >=
    [
        ! max dept reached
        3drop
        { 0 0 0 }
    ]
    [
        ! get sphere hits
        -rot
        [ [ [ dup ] dip sphere-intersect ] map ] keep swap
        hit new [
           [ [ distance>> ] keep swap ] bi@
           [ swap ] dip
           <
           [
               drop
           ]
           [
               nip
           ] if
        ] reduce ! closest hit is now on top of the stack
        
        ! [ normal>> ] [ sphere>> [ color>> ] [ is-light>> ] bi ] [ did-hit>> ] tri
        [ did-hit>> ] keep swap
        [
            ! it was a hit
            [ sphere>> is-light>> ] keep swap
            [
                ! it is a light
                sphere>> color>>
                [ 3drop ] dip
            ]
            [
                [ normal>> ] keep swap
                6 nrot swap
                random-dome
                [ [ point>> ] keep ] 2dip
                [ rot ] dip
                dup 4 -nrot
                <ray>
                [ 4 nrot ] dip
                7 nrot
                1 +
                [ swap ] dip
                trace
                [ [ normal>> ] keep swap ] 3dip
                [ dot ] 2dip
                rot v-scale
                [ sphere>> color>> ] 2dip
                rot v-mul
                [ drop ] 2dip
            ] if
        ]
        [
            ! no hit
            4drop
            { 0 0 0 }
        ] if
        
    ] if ;

: pixels ( width height -- pixels )
    [ dup ] dip
    *
    { 0 } swap cycle
    [ 
        swap drop
        [ dup ] dip
        swap /mod
        swap
        2array
    ] map-index [ drop ] dip ;

: write-ppm ( width height seq -- )
    [ "P3\n%d %d\n255\n" printf ] dip
    [
        [ 255.99 * 255 min 0 max "%d " printf ] each
    ] each
    ;

: main ( -- )
    enable-optimizer
    1280 ! width
    dup
    720 ! height
    dup
    [ swap ] dip
    50 ! samples
    <world>
    [ camera>> rt>> ] keep
    [ camera>> lt>> ] keep
    [ v-sub ] dip ! vdu { 0.0125 0.0 0.0 }
    [ dup ] 4 ndip
    [ 4 nrot v-div ] dip
    [ camera>> lb>> ] keep
    [ camera>> lt>> ] keep
    [ v-sub ] dip ! vdv { 0.0 -0.0125 0.0 }
    [ dup ] 4 ndip
    [ 4 nrot v-div ] dip
    -rot ! world vdu vdv
    random new 
    7 nrot 7 nrot
    pixels
    [ 5 nrot ] dip
    [
        [ dup ] dip
        swap
        { 0 } swap cycle
        [
            drop
            [ [ camera>> eye>> ] keep ] 5 ndip
            7 nrot
            [ [ camera>> lt>> ] keep ] 6 ndip
            8 nrot
            [ [ first ] keep ] 2 ndip
            4 nrot ! x is top of the stack
            [ next-random ] 5 ndip
            6 nrot
            + ! x + randf()
            [ dup ] 7 ndip
            [ 7 nrot ] dip
            v-scale ! vdu.muls(x + randf())

            [ [ second ] keep swap ] 3 ndip
            4 nrot ! y is top of the stack
            [ next-random ] 6 ndip
            7 nrot
            + ! y + randf()

            [ dup ] 7 ndip
            [ 7 nrot ] dip
            v-scale ! vdv.muls(y + randf())
            v-add ! vdu.muls(x + randf()).add(vdv.muls(y + randf()))
            v-add ! world.camera.lt.add(vdu.muls(x + randf()).add(vdv.muls(y + randf())))

            [ dup ] dip
            swap
            v-sub
            unit
            <ray>

            ! random, ray, spheres, depth
            [ dup ] 3 ndip 4 nrot ! random
            swap ! ray
            [ [ spheres>> ] keep ] 7 ndip 9 nrot ! spheres
            0 ! depth
            trace
            [ 4 nrot swap 4 -nrot drop ] dip
        ] map
        { 0 0 0 } [ v-add ] reduce
        [ dup ] 2 ndip
        rot
        1 swap
        /f
        v-scale ! color.div(SAMPLES);
        [ drop ] dip
    ] map
    8 nrot 8 nrot
    rot
    write-ppm
    5 ndrop
    ;

MAIN: main

