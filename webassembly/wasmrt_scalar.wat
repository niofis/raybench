(module
    (import "wasi_snapshot_preview1" "proc_exit" (func $wasi|exit (param i32)))
    (import "wasi_snapshot_preview1" "fd_write" (func $wasi|print (param i32 i32 i32 i32) (result i32)))
    (memory 1)
    (export "memory" (memory 0))
    (global $width i32 (i32.const 1280))
    (global $width|f32 f32 (f32.const 1280))
    (global $height i32 (i32.const 720))
    (global $height|f32 f32 (f32.const 720))
    (global $samples i32 (i32.const 50))
    (global $samples|f32 f32 (f32.const 50))
    (global $max_depth i32 (i32.const 5))
    (global $chars i32 (i32.const 0))
    (data (global.get $chars) "0123456789ABCDEFP3 \n.-")
    (global $ciovec|buf i32 (i32.const 24))
    (global $ciovec|size i32 (i32.const 28))
    (global $ciovec|res i32 (i32.const 32))
    (global $str_buffer i32 (i32.const 36)) (;100 chars;)
    (global $i32_to_str_buffer i32 (i32.const 135)) (;20 chars;)
    (global $i32_to_str_buffer|end i32 (i32.const 155))
    (global $rand|x i32 (i32.const 156))
    (global $rand|y i32 (i32.const 160))
    (global $rand|z i32 (i32.const 164))
    (global $rand|w i32 (i32.const 168))
    (global $world|camera|eye i32 (i32.const 176))
    (global $world|camera|lt i32 (i32.const 192))
    (global $world|camera|rt i32 (i32.const 208))
    (global $world|camera|lb i32 (i32.const 224))
    (global $vec|size i32 (i32.const 16))
    (global $sphere|size i32 (i32.const 48)) (; 48 (3 vectors);)
    (global $world|spheres i32 (i32.const 256)) (; 48 bytes x 8 = 384;)
    (; next available memory location should start at 512 ;)
    ;; (global $number f32 (f32.const 1.5))
    (global $main|vdu i32 (i32.const 512)) (;vector;)
    (global $main|vdv i32 (i32.const 528)) (;vector;)
    (global $main|tvec i32 (i32.const 544)) (; vector ;)
    (global $main|tvec2 i32 (i32.const 560)) (; vector ;)

    (global $ray i32 (i32.const 640))
    (global $ray|origin i32 (i32.const 640))
    (global $ray|direction i32 (i32.const 656))

    (func $i32_to_str (param $buf i32) (param $num i32) (param $base i32) (result i32)
        (local $count i32)
        (local $rem i32)
        (local $ptr i32)
        (local.set $count (i32.const 0))
        (local.set $ptr (global.get $i32_to_str_buffer|end))
        (block
            (loop
                (memory.copy
                    (local.get $ptr) ;; destination
                    (block (result i32)
                        (i32.rem_u
                            (local.get $num)
                            (local.get $base))
                        (global.get $chars)
                        (i32.add)) ;; source = chars + (num % 10)
                        (i32.const 1))

                (local.set $num 
                    (i32.div_u
                        (local.get $num) 
                        (local.get $base))) ;;num = Math.floor(num / 10)
                
                
                (local.set $count
                    (i32.add
                        (local.get $count)
                        (i32.const 1))) ;;count = count + 1

                (br_if 1 
                    (i32.eq
                        (local.get $num)
                        (i32.const 0))) ;; if num is zero, exit loop
                (local.set $ptr 
                    (i32.sub 
                        (local.get $ptr) 
                        (i32.const 1))) ;;move ptr one byte to the left
                (br 0)
            )
        )

        (memory.copy
            (local.get $buf)
            (local.get $ptr)
            (local.get $count)) ;; copy local string buffer to parameter buffer
        (local.get $count)
    )
    (func $print_cr
        (call $print
            (i32.add 
                (global.get $chars)
                (i32.const 19))
            (i32.const 1)) ;; print CR
    )
    (func $print_space
        (call $print
            (i32.add 
                (global.get $chars)
                (i32.const 18))
            (i32.const 1)) ;; print SPACE
    ) 
    (func $print (param $str_ptr i32) (param $str_len i32)
        (i32.store (global.get $ciovec|buf) (local.get $str_ptr))
        (i32.store (global.get $ciovec|size) (local.get $str_len))
        (call $wasi|print
            (i32.const 1)
            (global.get $ciovec|buf)
            (i32.const 1)
            (global.get $ciovec|res))
        (drop)
    )
    (func $printf32 (param $val f32)
        (if (f32.lt (local.get $val) (f32.const 0))
                (call $print
                    (i32.add 
                        (global.get $chars)
                        (i32.const 21))
                    (i32.const 1)))
        (call $print
            (global.get $str_buffer) 
            (call $i32_to_str
                (global.get $str_buffer)
                (i32.trunc_f32_s (f32.abs (local.get $val)))
                (i32.const 10)))
        (call $print
            (i32.add 
                (global.get $chars)
                (i32.const 20))
            (i32.const 1))
        (call $print
            (global.get $str_buffer) 
            (call $i32_to_str
                (global.get $str_buffer)
                (i32.trunc_f32_s 
                    (f32.mul
                        (f32.sub
                            (f32.abs (local.get $val))
                            (f32.floor (f32.abs (local.get $val))))
                        (f32.const 100000)))
                (i32.const 10)))
    )
    (func $write_ppm_header (local $addr i32)
        (local $count i32)
        
        (call $print
            (i32.add 
                (global.get $chars)
                (i32.const 16))
            (i32.const 2)) ;; print P3

        (call $print_cr)

        (call $print
            (global.get $str_buffer) 
            (call $i32_to_str
                (global.get $str_buffer)
                (global.get $width)
                (i32.const 10)))

        (call $print_space)

        (call $print
            (global.get $str_buffer) 
            (call $i32_to_str
                (global.get $str_buffer)
                (global.get $height)
                (i32.const 10)))
        
        (call $print_cr)

        (call $print
            (global.get $str_buffer) 
            (call $i32_to_str
                (global.get $str_buffer)
                (i32.const 255)
                (i32.const 10)))

        (call $print_cr)
    )
    (func $write_ppm_color (param $x f32) (param $y f32) (param $z f32)
        (call $print
            (global.get $str_buffer) 
            (call $i32_to_str
                (global.get $str_buffer)
                (i32.trunc_f32_s (f32.mul (local.get $x) (f32.const 255.99)))
                (i32.const 16)))

        (call $print_space)

        (call $print
            (global.get $str_buffer) 
            (call $i32_to_str
                (global.get $str_buffer)
                (i32.trunc_f32_s (f32.mul (local.get $y) (f32.const 255.99)))
                (i32.const 16)))

        (call $print_space)

        (call $print
            (global.get $str_buffer) 
            (call $i32_to_str
                (global.get $str_buffer)
                (i32.trunc_f32_s (f32.mul (local.get $z) (f32.const 255.99)))
                (i32.const 16)))

        (call $print_space)
    )

    (func $vec|print (param $vec i32)
        (call $printf32 (f32.load
                            (i32.add
                                (local.get $vec)
                                (i32.const 0))))
        (call $print_space)
        (call $printf32 (f32.load
                            (i32.add
                                (local.get $vec)
                                (i32.const 4))))
        (call $print_space)
        (call $printf32 (f32.load
                            (i32.add
                                (local.get $vec)
                                (i32.const 8))))
        (call $print_space)
        (call $print_cr)
    )
    (func $vec|init (param $vec i32) (param $x f32) (param $y f32) (param $z f32)
        (f32.store (i32.add (local.get $vec) (i32.const 0)) (local.get $x))
        (f32.store (i32.add (local.get $vec) (i32.const 4)) (local.get $y))
        (f32.store (i32.add (local.get $vec) (i32.const 8)) (local.get $z))
        (f32.store (i32.add (local.get $vec) (i32.const 12)) (f32.const 0))
    )
    (func $init_sphere (param $idx i32)
        (param $x f32) (param $y f32) (param $z f32)
        (param $r f32) (param $g f32) (param $b f32)
        (param $radius f32) (param $is_light i32)
        (local $ptr i32)
        (local.set $ptr 
            (i32.add
                (global.get $world|spheres)
                (i32.mul (local.get $idx) (i32.const 32))))
        (call $vec|init (local.get $ptr) (local.get $x) (local.get $y) (local.get $z))
        (local.set $ptr
            (i32.add
                (local.get $ptr)
                (global.get $vec|size)))
        (call $vec|init (local.get $ptr) (local.get $r) (local.get $g) (local.get $b))
        (local.set $ptr
            (i32.add
                (local.get $ptr)
                (global.get $vec|size)))
        (f32.store (local.get $ptr) (local.get $radius))
        (i32.store (i32.add (local.get $ptr) (i32.const 4)) (local.get $is_light))    
    )
    (func $init_world
        (call $vec|init (global.get $world|camera|eye) (f32.const 0) (f32.const 4.5) (f32.const 75))
        (call $vec|init (global.get $world|camera|lt) (f32.const -8) (f32.const 9) (f32.const 50))
        (call $vec|init (global.get $world|camera|rt) (f32.const 8) (f32.const 9) (f32.const 50))
        (call $vec|init (global.get $world|camera|lb) (f32.const -8) (f32.const 0) (f32.const 50))

        (call $init_sphere (i32.const 0)
            (f32.const 0) (f32.const -10002) (f32.const 0)
            (f32.const 1) (f32.const 1) (f32.const 1)
            (f32.const 9999) (i32.const 0))
        
        (call $init_sphere (i32.const 1)
            (f32.const -10012) (f32.const 0) (f32.const 0)
            (f32.const 1) (f32.const 0) (f32.const 0)
            (f32.const 9999) (i32.const 0))

        (call $init_sphere (i32.const 2)
            (f32.const 10012) (f32.const 0) (f32.const 0)
            (f32.const 0) (f32.const 1) (f32.const 0)
            (f32.const 9999) (i32.const 0))

        (call $init_sphere (i32.const 3)
            (f32.const 0) (f32.const 0) (f32.const -10012)
            (f32.const 1) (f32.const 1) (f32.const 1)
            (f32.const 9999) (i32.const 0))

        (call $init_sphere (i32.const 4)
            (f32.const 0) (f32.const 10012) (f32.const 0)
            (f32.const 1) (f32.const 1) (f32.const 1)
            (f32.const 9999) (i32.const 1))

        (call $init_sphere (i32.const 5)
            (f32.const -5) (f32.const 0) (f32.const 2)
            (f32.const 1) (f32.const 1) (f32.const 0)
            (f32.const 2) (i32.const 0))
            
        (call $init_sphere (i32.const 6)
            (f32.const 0) (f32.const 5) (f32.const -1)
            (f32.const 1) (f32.const 0) (f32.const 0)
            (f32.const 4) (i32.const 0))
        
        (call $init_sphere (i32.const 7)
            (f32.const 8) (f32.const 5) (f32.const -1)
            (f32.const 0) (f32.const 0) (f32.const 1)
            (f32.const 2) (i32.const 0))
    )
    (func $vec|components (param $vec i32) (result f32 f32 f32)
        (f32.load (i32.add (local.get $vec) (i32.const 0)))
        (f32.load (i32.add (local.get $vec) (i32.const 4)))
        (f32.load (i32.add (local.get $vec) (i32.const 8)))
    )
    (func $vec|add (param $lhs i32) (param $rhs i32) (param $out i32)
        (v128.store
            (local.get $out)
            (f32x4.add 
                (v128.load (local.get $lhs))
                (v128.load (local.get $rhs))))
    )
    (func $vec|sub (param $lhs i32) (param $rhs i32) (param $out i32)
        (v128.store
            (local.get $out)
            (f32x4.sub 
                (v128.load (local.get $lhs))
                (v128.load (local.get $rhs))))
    )
    (func $vec|divs (param $vec i32) (param $div f32) (param $out i32)
       (v128.store
            (local.get $out)
            (f32x4.div 
                (v128.load (local.get $vec))
                (f32x4.splat (local.get $div)))) 
    )
    (func $vec|muls (param $vec i32) (param $div f32) (param $out i32)
       (v128.store
            (local.get $out)
            (f32x4.mul 
                (v128.load (local.get $vec))
                (f32x4.splat (local.get $div)))) 
    )
    (func $vec|copy (param $dest i32) (param $origin i32)
        (memory.copy (local.get $dest) (local.get $origin) (i32.const 16))
        ;; (v128.store (local.get $dest) (v128.load (local.get $origin)))
    )
    (func $vec|dot (param $vec v128) (result f32) (local $t v128)
        (local.set $t (f32x4.mul (local.get $vec) (local.get $vec)))

        (f32.add
            (f32x4.extract_lane 0 (local.get $t))
            (f32.add
                (f32x4.extract_lane 1 (local.get $t))
                (f32x4.extract_lane 2 (local.get $t))))
    )
    ;; (func $vec|unit (param $vec i32) (param $out i32) (local $t v128)
    ;;     (local.set (f32x4.mul (v128.load (local.get $vec) (v128.load (local.get $vec)))))
    ;;     (f32x4.div ())
    ;; )
    (func $rand|init
        (i32.store (global.get $rand|x) (i32.const 123456789))
        (i32.store (global.get $rand|y) (i32.const 362436069))
        (i32.store (global.get $rand|z) (i32.const 521288629))
        (i32.store (global.get $rand|w) (i32.const 88675123))
    )
    (func $rand|next (result f32) (local $t i32) (local $x i32) (local $w i32)
        (local.set $x (i32.load (global.get $rand|x)))
        (local.set $w (i32.load (global.get $rand|w)))

        (local.set $t
            (i32.xor (local.get $x) (i32.shl (local.get $x) (i32.const 11)))
            (i32.store (global.get $rand|x) (i32.load (global.get $rand|y))))

        (i32.store (global.get $rand|y) (i32.load (global.get $rand|z)))
        (i32.store (global.get $rand|z) (i32.load (global.get $rand|w)))
        (local.set $w
            (i32.xor
                (local.get $w)
                (i32.xor
                    (i32.shr_u (local.get $w) (i32.const 19))
                    (i32.xor (local.get $t) (i32.shr_u (local.get $t) (i32.const 8)))
        )))

        (i32.store (global.get $rand|w) (local.get $w))

        (f32.div (f32.convert_i32_u (local.get $w)) (f32.const 4294967295))
    )
    (func $ray|init
        (param $ray i32)
        (param $ox f32) (param $oy f32) (param $oz f32)
        (param $dx f32) (param $dy f32) (param $dz f32)
        (f32.store (i32.add (local.get $ray) (i32.const 0)) (local.get $ox))
        (f32.store (i32.add (local.get $ray) (i32.const 4)) (local.get $oy))
        (f32.store (i32.add (local.get $ray) (i32.const 8)) (local.get $oz))
        (f32.store (i32.add (local.get $ray) (i32.const 16)) (local.get $dx))
        (f32.store (i32.add (local.get $ray) (i32.const 20)) (local.get $dy))
        (f32.store (i32.add (local.get $ray) (i32.const 24)) (local.get $dz))
    )
    (func $main (export "_start") (local $x i32) (local $y i32) (local $s i32)
        (call $init_world)
        
        (call $vec|sub (global.get $world|camera|rt) (global.get $world|camera|lt) (global.get $main|vdu))
        (call $vec|divs (global.get $main|vdu) (global.get $width|f32) (global.get $main|vdu))
        (call $vec|sub (global.get $world|camera|lb) (global.get $world|camera|lt) (global.get $main|vdv))
        (call $vec|divs (global.get $main|vdv) (global.get $height|f32) (global.get $main|vdv))

        (call $printf32 (call $vec|dot (v128.load (global.get $main|vdu)))
        (call $print_cr))

        ;; (call $vec|print (global.get $main|vdv))

        ;; (call $rand|init)
        ;; (call $printf32 (call $rand|next))
        ;; (call $print_cr)
        ;; (call $printf32 (call $rand|next))
        ;; (call $print_cr)
        ;; (call $printf32 (call $rand|next))
        ;; (call $print_cr)


        (call $write_ppm_header)
        
        (block
            (local.set $y (i32.const 0))
            (loop
                (block
                    (local.set $x (i32.const 0))
                    (loop
                        (block
                            (local.set $s (i32.const 0))
                            (loop

                                ;; (call $print
                                ;;     (global.get $str_buffer) 
                                ;;     (call $i32_to_str
                                ;;         (global.get $str_buffer)
                                ;;         (local.get $s)
                                ;;         (i32.const 10)))
                                ;; (call $print_space)
                                ;; (call $vec|copy (global.get $ray|origin) (global $world|camera|eye))
                                ;; (call $vec|muls
                                ;;     (global.get $main|vdu)
                                ;;     (f32.add (f32.convert_i32_s (local.get $x)) (call $rand|next))
                                ;;     (global.get $main|tvec))
                                ;; (call $vec|muls
                                ;;     (global.get $main|vdv)
                                ;;     (f32.add (f32.convert_i32_s (local.get $y)) (call $rand|next))
                                ;;     (global.get $main|tvec2))
                                ;; (call $vec|add
                                ;;     (global.get $main|tvec)
                                ;;     (global.get $main|tvec2)
                                ;;     (global.get $ray|direction))
                                ;; (call $vec|add
                                ;;     (global.get $ray|direction)
                                ;;     (global.get $world|camera|lt))
                                ;; (call $vec|sub
                                ;;     (global.get $ray|direction)
                                ;;     (global.get $ay|origin)
                                ;;     (global.get $ray|direction))
                                ;; (call $vec|unit
                                ;;     (global.get $ray|direction)
                                ;;     (global.get $ray|direction))
                                
                                ;; (call $vec|add (global.get $world|camera|lt))
                                ;; (call $ray|init
                                ;;     (global.get $ray)
                                ;;     (f32.load (i32.add (global.get $world|camera|eye) (i32.const 0)))
                                ;;     (f32.load (i32.add (global.get $world|camera|eye) (i32.const 4)))
                                ;;     (f32.load (i32.add (global.get $world|camera|eye) (i32.const 8)))

                                ;;     (f32.load (i32.add (global.get $world|camera|eye) (i32.const 0)))
                                ;;     (f32.load (i32.add (global.get $world|camera|eye) (i32.const 4)))
                                ;;     (f32.load (i32.add (global.get $world|camera|eye) (i32.const 8)))
                                ;;     )


                                (local.set $s (i32.add (local.get $s) (i32.const 1)))
                                (br_if 1 
                                (i32.eq
                                    (local.get $s)
                                    (global.get $samples)))
                                (br 0)
                            )
                        )
                        ;; (call $print_cr)


                        (local.set $x (i32.add (local.get $x) (i32.const 1)))
                        (br_if 1 
                            (i32.eq
                                (local.get $x)
                                (global.get $width)))
                        (br 0)
                    )
                )
                
                (local.set $y (i32.add (local.get $y) (i32.const 1)))
                (br_if 1 
                    (i32.eq
                        (local.get $y)
                        (global.get $height)))
                (br 0)
            )
        )

        (call $write_ppm_color 
            (f32.const 1.0)
            (f32.const 0.5)
            (f32.const 0.25))
        
        (call $wasi|exit
            (i32.const 0))
        unreachable))