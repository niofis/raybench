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
    (data (offset (i32.const 0)) "0123456789ABCDEFP3 \n.-")
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
    (global $world|spheres|center|x i32 (i32.const 256)) (; 32 bytes ;)
    (global $world|spheres|center|y i32 (i32.const 288)) (; 32 bytes ;)
    (global $world|spheres|center|z i32 (i32.const 320)) (; 32 bytes ;)
    (global $world|spheres|radius i32 (i32.const 352)) (; 32 bytes ;)
    (global $world|spheres|is_light i32 (i32.const 384)) (; 32 bytes ;)
    (global $world|spheres|color i32 (i32.const 416)) (;128 bytes;)
    (; next available memory location should start at 544 ;)

    (global $ray i32 (i32.const 640))
    (global $ray|origin i32 (i32.const 640))
    (global $ray|direction i32 (i32.const 656))

    (global $hit i32 (i32.const 672))
    (global $hit|point i32 (i32.const 672))
    (global $hit|normal i32 (i32.const 688))
    (global $hit|dist i32 (i32.const 704))
    (global $hit|did_hit i32 (i32.const 708))

    (func $i32_to_str (param $buf i32) (param $num i32) (result i32)
        (local $count i32)
        (local $rem i32)
        (local $ptr i32)
        (local $base i32)
        (local.set $base (i32.const 10))
        (local.set $count (i32.const 0))
        (local.set $ptr (global.get $i32_to_str_buffer|end))
        (block
            (loop
                (memory.copy
                    (local.get $ptr) ;; destination
                    (block (result i32)
                        (i32.rem_u
                            (local.get $num)
                            (i32.const 10))
                        (global.get $chars)
                        (i32.add)) ;; source = chars + (num % 10)
                        (i32.const 1))

                (local.set $num 
                    (i32.div_u
                        (local.get $num) 
                        (i32.const 10))) ;;num = Math.floor(num / 10)
                
                
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
                ))
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
                ))
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
                ))

        (call $print_space)

        (call $print
            (global.get $str_buffer) 
            (call $i32_to_str
                (global.get $str_buffer)
                (global.get $height)
                ))
        
        (call $print_cr)

        (call $print
            (global.get $str_buffer) 
            (call $i32_to_str
                (global.get $str_buffer)
                (i32.const 255)
                ))

        (call $print_cr)
    )
    (func $write_ppm_color (param $color v128)
        (call $print
            (global.get $str_buffer) 
            (call $i32_to_str
                (global.get $str_buffer)
                (i32.trunc_f32_s (f32.mul (f32x4.extract_lane 0 (local.get $color)) (f32.const 255.99)))
                ))

        (call $print_space)

        (call $print
            (global.get $str_buffer) 
            (call $i32_to_str
                (global.get $str_buffer)
                (i32.trunc_f32_s (f32.mul (f32x4.extract_lane 1 (local.get $color)) (f32.const 255.99)))
                ))

        (call $print_space)

        (call $print
            (global.get $str_buffer) 
            (call $i32_to_str
                (global.get $str_buffer)
                (i32.trunc_f32_s (f32.mul (f32x4.extract_lane 2 (local.get $color)) (f32.const 255.99)))
                ))

        (call $print_space)
    )
    (func $vec|init (param $vec i32) (param $x f32) (param $y f32) (param $z f32)
        (f32.store (i32.add (local.get $vec) (i32.const 0)) (local.get $x))
        (f32.store (i32.add (local.get $vec) (i32.const 4)) (local.get $y))
        (f32.store (i32.add (local.get $vec) (i32.const 8)) (local.get $z))
        (f32.store (i32.add (local.get $vec) (i32.const 12)) (f32.const 0))
    )
    (func $sphere|init (param $idx i32)
        (param $x f32) (param $y f32) (param $z f32)
        (param $r f32) (param $g f32) (param $b f32)
        (param $radius f32) (param $is_light i32)
        (local $ptr i32)
        (f32.store
            (i32.add
                (global.get $world|spheres|center|x)
                (i32.mul (local.get $idx) (i32.const 4)))
            (local.get $x))
        (f32.store
            (i32.add
                (global.get $world|spheres|center|y)
                (i32.mul (local.get $idx) (i32.const 4)))
            (local.get $y))
        (f32.store
            (i32.add
                (global.get $world|spheres|center|z)
                (i32.mul (local.get $idx) (i32.const 4)))
            (local.get $z))

        (call $vec|init
            (i32.add
                (global.get $world|spheres|color)
                (i32.mul
                    (i32.const 16)
                    (local.get $idx)))
            (local.get $r) (local.get $g) (local.get $b))

        (f32.store
            (i32.add
                (global.get $world|spheres|radius)
                (i32.mul (local.get $idx) (i32.const 4)))
            (local.get $radius))
        
        (i32.store
            (i32.add
                (global.get $world|spheres|is_light)
                (i32.mul (local.get $idx) (i32.const 4)))
            (local.get $is_light))
    )
    (func $world|init
        (call $vec|init (global.get $world|camera|eye) (f32.const 0) (f32.const 4.5) (f32.const 75))
        (call $vec|init (global.get $world|camera|lt) (f32.const -8) (f32.const 9) (f32.const 50))
        (call $vec|init (global.get $world|camera|rt) (f32.const 8) (f32.const 9) (f32.const 50))
        (call $vec|init (global.get $world|camera|lb) (f32.const -8) (f32.const 0) (f32.const 50))

        (call $sphere|init (i32.const 0)
            (f32.const 0) (f32.const -10002) (f32.const 0)
            (f32.const 1) (f32.const 1) (f32.const 1)
            (f32.const 9999) (i32.const 0))
        
        (call $sphere|init (i32.const 1)
            (f32.const -10012) (f32.const 0) (f32.const 0)
            (f32.const 1) (f32.const 0) (f32.const 0)
            (f32.const 9999) (i32.const 0))

        (call $sphere|init (i32.const 2)
            (f32.const 10012) (f32.const 0) (f32.const 0)
            (f32.const 0) (f32.const 1) (f32.const 0)
            (f32.const 9999) (i32.const 0))

        (call $sphere|init (i32.const 3)
            (f32.const 0) (f32.const 0) (f32.const -10012)
            (f32.const 1) (f32.const 1) (f32.const 1)
            (f32.const 9999) (i32.const 0))

        (call $sphere|init (i32.const 4)
            (f32.const 0) (f32.const 10012) (f32.const 0)
            (f32.const 1) (f32.const 1) (f32.const 1)
            (f32.const 9999) (i32.const 1))

        (call $sphere|init (i32.const 5)
            (f32.const -5) (f32.const 0) (f32.const 2)
            (f32.const 1) (f32.const 1) (f32.const 0)
            (f32.const 2) (i32.const 0))
            
        (call $sphere|init (i32.const 6)
            (f32.const 0) (f32.const 5) (f32.const -1)
            (f32.const 1) (f32.const 0) (f32.const 0)
            (f32.const 4) (i32.const 0))
        
        (call $sphere|init (i32.const 7)
            (f32.const 8) (f32.const 5) (f32.const -1)
            (f32.const 0) (f32.const 0) (f32.const 1)
            (f32.const 2) (i32.const 0))
    )
    (func $vec|dot (param $lhs v128) (param $rhs v128) (result f32) (local $t v128)
        (local.set $t (f32x4.mul (local.get $lhs) (local.get $rhs)))

        (f32.add
            (f32x4.extract_lane 0 (local.get $t))
            (f32.add
                (f32x4.extract_lane 1 (local.get $t))
                (f32x4.extract_lane 2 (local.get $t))))
    )
    (func $vec|norm (param $vec v128) (result f32)
        (f32.sqrt (call $vec|dot (local.get $vec) (local.get $vec)))
    )
    (func $vec|unit (param $vec v128) (result v128)
        (f32x4.div
            (local.get $vec)
            (f32x4.splat (call $vec|norm (local.get $vec)))
        )
    )
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
    (func $rand|dome (param $normal v128) (result v128)
         (local $p v128)
         (local $d f32)
         (local.set $p (f32x4.splat (f32.const 0)))
         (block 
            (loop
                (local.set $p (f32x4.replace_lane 0 (local.get $p) (f32.add (f32.mul (f32.const 2) (call $rand|next)) (f32.const -1))))
                (local.set $p (f32x4.replace_lane 1 (local.get $p) (f32.add (f32.mul (f32.const 2) (call $rand|next)) (f32.const -1))))
                (local.set $p (f32x4.replace_lane 2 (local.get $p) (f32.add (f32.mul (f32.const 2) (call $rand|next)) (f32.const -1))))
                (local.set $p (call $vec|unit (local.get $p)))
                (br_if 1 (f32.gt (call $vec|dot (local.get $p) (local.get $normal)) (f32.const 0)))
                (br 0)
            )
         )
         (local.get $p)
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

    (func $sphere|hits (param $origin v128) (param $direction v128) (param $group i32) (result v128)
        (local $ocx v128)
        (local $ocy v128)
        (local $ocz v128)
        (local $b v128)
        (local $c v128)
        (local $dis v128)
        (local $e v128)
        (local $t1 v128)
        (local $t2 v128)
        (local $dis_mask v128)
        (local $t1_mask v128)
        (local $t2_mask v128)
        (local $t v128)
        (local $offset i32)

        (local.set $offset (i32.mul (local.get $group) (i32.const 16)))

        (local.set $ocx (f32x4.sub (f32x4.splat (f32x4.extract_lane 0 (local.get $origin))) (v128.load (i32.add (global.get $world|spheres|center|x) (local.get $offset)))))
        (local.set $ocy (f32x4.sub (f32x4.splat (f32x4.extract_lane 1 (local.get $origin))) (v128.load (i32.add (global.get $world|spheres|center|y) (local.get $offset)))))
        (local.set $ocz (f32x4.sub (f32x4.splat (f32x4.extract_lane 2 (local.get $origin))) (v128.load (i32.add (global.get $world|spheres|center|z) (local.get $offset)))))
        
        (local.set $b
            (f32x4.neg
                (f32x4.add
                    (f32x4.mul (local.get $ocx) (f32x4.splat (f32x4.extract_lane 0 (local.get $direction))))
                    (f32x4.add
                        (f32x4.mul (local.get $ocy) (f32x4.splat (f32x4.extract_lane 1 (local.get $direction))))
                        (f32x4.mul (local.get $ocz) (f32x4.splat (f32x4.extract_lane 2 (local.get $direction))))))))
        
        (local.set $c
            (f32x4.sub
                (f32x4.add
                    (f32x4.mul (local.get $ocx) (local.get $ocx))
                    (f32x4.add
                        (f32x4.mul (local.get $ocy) (local.get $ocy))
                        (f32x4.mul (local.get $ocz) (local.get $ocz))))
                (f32x4.mul (v128.load (i32.add (global.get $world|spheres|radius) (local.get $offset))) (v128.load (i32.add (global.get $world|spheres|radius (local.get $offset)))))))
        (local.set $dis
            (f32x4.sub
                (f32x4.mul (local.get $b) (local.get $b))
                (local.get $c)))

        (local.set $dis_mask (f32x4.gt (local.get $dis) (f32x4.splat (f32.const 0))))
        
        (if (i32.eq (i32.const 0) (v128.any_true (local.get $dis_mask))) (return (f32x4.splat (f32.const 1e15))))

        (local.set $dis (v128.and (local.get $dis_mask) (local.get $dis)))

        (local.set $e
            (f32x4.sqrt (local.get $dis)))

        (local.set $t1
            (f32x4.sub (local.get $b) (local.get $e)))
        (local.set $t1_mask
            (v128.and
                (local.get $dis_mask)
                (f32x4.gt
                    (local.get $t1)
                    (f32x4.splat (f32.const 0.007)))))

        (local.set $t (v128.and (local.get $t1) (local.get $t1_mask)))
    

        (local.set $t2
            (f32x4.add (local.get $b) (local.get $e)))
        (local.set $t2_mask 
            (v128.and
                (local.get $dis_mask)
                (f32x4.gt
                    (local.get $t2)
                    (f32x4.splat (f32.const 0.007)))))
        (local.set $t2 (v128.and (local.get $t2) (local.get $t2_mask)))

        (local.set $t (v128.bitselect (local.get $t1) (local.get $t2) (local.get $t1_mask)))

        (local.set $t
            (v128.bitselect
                (local.get $t)
                (f32x4.splat (f32.const 1e15))
                (f32x4.ne (local.get $t) (f32x4.splat (f32.const 0))))
            )

        (local.get $t)
    )
    (func $sphere|center (param $id i32) (result v128)
        (local $center v128)
        
        (local.set $center (f32x4.replace_lane 0 (local.get $center)
            (f32.load
                (i32.add 
                    (global.get $world|spheres|center|x) 
                    (i32.mul 
                       (local.get $id)
                        (i32.const 4))))))

        (local.set $center (f32x4.replace_lane 1 (local.get $center)
            (f32.load
                (i32.add 
                    (global.get $world|spheres|center|y) 
                    (i32.mul 
                       (local.get $id)
                        (i32.const 4))))))

        (local.set $center (f32x4.replace_lane 2 (local.get $center)
            (f32.load
                (i32.add 
                    (global.get $world|spheres|center|z) 
                    (i32.mul 
                       (local.get $id)
                        (i32.const 4))))))

        (f32x4.replace_lane 3 (local.get $center) (f32.const 0))
    )
    (func $trace (param $origin v128) (param $direction v128) (param $depth i32) (result v128)
        (local $g1 v128)
        (local $g2 v128)
        (local $id i32)
        (local $min f32)
        (local $at f32)
        (local $color v128)
        (local $normal v128)

        (local.set $color (f32x4.splat (f32.const 0)))

        (if (i32.eq (local.get $depth) (global.get $max_depth)) (return (local.get $color)))
        
        (call $sphere|hits (local.get $origin) (local.get $direction) (i32.const 0))
        (local.set $g1)
        (call $sphere|hits (local.get $origin) (local.get $direction) (i32.const 1))
        (local.set $g2)

        (local.set $id (i32.const -1))
        (local.set $min (f32.const 1e15))

        (if (f32.gt (local.get $min) (f32x4.extract_lane 0 (local.get $g1)))
            (block
                (local.set $id (i32.const 0))
                (local.set $min (f32x4.extract_lane 0 (local.get $g1)))
            )
        )
        (if (f32.gt (local.get $min) (f32x4.extract_lane 1 (local.get $g1)))
            (block
                (local.set $id (i32.const 1))
                (local.set $min (f32x4.extract_lane 1 (local.get $g1)))
            )
        )
        (if (f32.gt (local.get $min) (f32x4.extract_lane 2 (local.get $g1)))
            (block
                (local.set $id (i32.const 2))
                (local.set $min (f32x4.extract_lane 2 (local.get $g1)))
            )
        )
        (if (f32.gt (local.get $min) (f32x4.extract_lane 3 (local.get $g1)))
            (block
                (local.set $id (i32.const 3))
                (local.set $min (f32x4.extract_lane 3 (local.get $g1)))
            )
        )
        (if (f32.gt (local.get $min) (f32x4.extract_lane 0 (local.get $g2)))
            (block
                (local.set $id (i32.const 4))
                (local.set $min (f32x4.extract_lane 0 (local.get $g2)))
            )
        )
        (if (f32.gt (local.get $min) (f32x4.extract_lane 1 (local.get $g2)))
            (block
                (local.set $id (i32.const 5))
                (local.set $min (f32x4.extract_lane 1 (local.get $g2)))
            )
        )
        (if (f32.gt (local.get $min) (f32x4.extract_lane 2 (local.get $g2)))
            (block
                (local.set $id (i32.const 6))
                (local.set $min (f32x4.extract_lane 2 (local.get $g2)))
            )
        )
        (if (f32.gt (local.get $min) (f32x4.extract_lane 3 (local.get $g2)))
            (block
                (local.set $id (i32.const 7))
                (local.set $min (f32x4.extract_lane 3 (local.get $g2)))
            )
        )

        (if (i32.eq (local.get $id) (i32.const -1)) (return (local.get $color)))

        (local.set $color
            (v128.load
                (i32.add
                    (global.get $world|spheres|color)
                    (i32.mul (local.get $id) (i32.const 16)))))

        (if (i32.ne
            (i32.const 0)
            (i32.load
                (i32.add
                    (global.get $world|spheres|is_light)
                    (i32.mul (local.get $id) (i32.const 4)))))
            (return (local.get $color))
        )

        (local.set $origin
            (f32x4.add
                (local.get $origin)
                (f32x4.mul
                    (local.get $direction)
                    (f32x4.splat (local.get $min))
                )))

        (local.set $normal
            (f32x4.sub (local.get $origin) (call $sphere|center (local.get $id))))
        
        (local.set $normal (call $vec|unit (local.get $normal)))

        (local.set $direction (call $rand|dome (local.get $normal)))


        (local.set $at (call $vec|dot (local.get $direction) (local.get $normal)))

        
        (local.set $color
            (f32x4.mul
                (local.get $color)
                (f32x4.mul
                    (call $trace
                        (local.get $origin)
                        (local.get $direction)
                        (i32.add (local.get $depth) (i32.const 1)))
                    (f32x4.splat (local.get $at)))))
        
        (local.get $color)
    )

    (func $main (export "_start")
        (local $x i32)
        (local $y i32)
        (local $s i32)
        (local $vdu v128)
        (local $vdv v128)
        (local $tvec1 v128)
        (local $tvec2 v128)
        (local $color v128)
        (local $v_samples v128)
        (call $world|init)
        (call $rand|init)

        (local.set $v_samples (f32x4.splat (global.get $samples|f32)))

        (local.set $vdu
            (f32x4.div
                (f32x4.sub
                    (v128.load (global.get $world|camera|rt))
                    (v128.load (global.get $world|camera|lt)))
                (f32x4.splat (global.get $width|f32))))

        (local.set $vdv
            (f32x4.div
                (f32x4.sub
                    (v128.load (global.get $world|camera|lb))
                    (v128.load (global.get $world|camera|lt)))
                (f32x4.splat (global.get $height|f32))))

        (call $write_ppm_header)
        
        (block
            (local.set $y (i32.const 0))
            (loop
                (block
                    (local.set $x (i32.const 0))
                    (loop
                        (block
                            (local.set $color (v128.xor (local.get $color) (local.get $color)))
                            (local.set $s (i32.const 0))
                            (loop
                                (v128.store (global.get $ray|origin) (v128.load (global.get $world|camera|eye)))
                                (local.set $tvec1
                                    (f32x4.mul
                                        (local.get $vdu)
                                            (f32x4.splat
                                                (f32.add
                                                    (f32.convert_i32_s (local.get $x)) (call $rand|next)))))
                                (local.set $tvec2
                                    (f32x4.mul
                                        (local.get $vdv)
                                            (f32x4.splat
                                                (f32.add
                                                    (f32.convert_i32_s (local.get $y)) (call $rand|next)))))
                                
                                (local.set $tvec1 (f32x4.add (local.get $tvec1) (local.get $tvec2)))
                                (local.set $tvec1 (f32x4.add (local.get $tvec1) (v128.load (global.get $world|camera|lt))))
                                (local.set $tvec1 (f32x4.sub (local.get $tvec1) (v128.load (global.get $ray|origin))))
                                (v128.store (global.get $ray|direction) (call $vec|unit (local.get $tvec1)))

                                (local.set $color
                                    (f32x4.add 
                                        (local.get $color)
                                        (call $trace (v128.load (global.get $ray|origin)) (v128.load (global.get $ray|direction)) (i32.const 0))))
                                
                                (local.set $s (i32.add (local.get $s) (i32.const 1)))
                                (br_if 1 
                                    (i32.eq
                                        (local.get $s)
                                        (global.get $samples)))
                                (br 0)
                            )
                        )

                        ;; write pixel color

                        (call $write_ppm_color (f32x4.div (local.get $color) (local.get $v_samples)))

                        (local.set $x (i32.add (local.get $x) (i32.const 1)))
                        (br_if 1 
                            (i32.eq
                                (local.get $x)
                                (global.get $width)))
                        (br 0)
                    )
                )
                
                ;;  end pixels row
                
                (call $print_cr)

                (local.set $y (i32.add (local.get $y) (i32.const 1)))
                (br_if 1 
                    (i32.eq
                        (local.get $y)
                        (global.get $height)))
                (br 0)
            )
        )

        ;; exit the program
        
        (call $wasi|exit
            (i32.const 0))
        unreachable))