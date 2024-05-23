(module

    (func $v128|print (param $vec v128)
        (call $printf32 (f32x4.extract_lane 0 (local.get $vec)))
        (call $print_space)
        (call $printf32 (f32x4.extract_lane 1 (local.get $vec)))
        (call $print_space)
        (call $printf32 (f32x4.extract_lane 2 (local.get $vec)))
        (call $print_space)
        (call $printf32 (f32x4.extract_lane 3 (local.get $vec)))
        (call $print_cr)
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
    (func $sphere|print (param $idx i32)
        (call $printf32 (f32.load (i32.add (global.get $world|spheres|center|x) (i32.mul (local.get $idx) (i32.const 4)))))
        (call $print_space)
        (call $printf32 (f32.load (i32.add (global.get $world|spheres|center|y) (i32.mul (local.get $idx) (i32.const 4)))))
        (call $print_space)
        (call $printf32 (f32.load (i32.add (global.get $world|spheres|center|z) (i32.mul (local.get $idx) (i32.const 4)))))
        (call $print_space)
        (call $print_cr)
        (call $v128|print (v128.load (i32.add (global.get $world|spheres|color) (i32.mul (local.get $idx) (i32.const 16)))))
        (call $printf32 (f32.load (i32.add (global.get $world|spheres|radius) (i32.mul (local.get $idx) (i32.const 4)))))
        (call $print_space)
        (call $print
            (global.get $str_buffer) 
            (call $i32_to_str
                (global.get $str_buffer)
                (i32.load (i32.add (global.get $world|spheres|is_light) (i32.mul (local.get $idx) (i32.const 4))))
                 ))
        (call $print_space)
        (call $print_cr)
    )
)