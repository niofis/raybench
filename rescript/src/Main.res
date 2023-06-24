let width = 1280
let height = 720
let samples = 50
let max_depth = 5

type vector = {x: float, y: float, z: float}

let zero = {x: 0.0, y: 0.0, z: 0.0}

let vadd = (v1: vector, v2: vector): vector => {x: v1.x +. v2.x, y: v1.y +. v2.y, z: v1.z +. v2.z}
let vsub = (v1: vector, v2: vector): vector => {x: v1.x -. v2.x, y: v1.y -. v2.y, z: v1.z -. v2.z}
let vmul = (v1: vector, v2: vector): vector => {x: v1.x *. v2.x, y: v1.y *. v2.y, z: v1.z *. v2.z}
let vmuls = (v1: vector, s: float): vector => {x: v1.x *. s, y: v1.y *. s, z: v1.z *. s}
let vdiv = (v1: vector, v2: vector): vector => {x: v1.x /. v2.x, y: v1.y /. v2.y, z: v1.z /. v2.z}
let vdivs = (v1: vector, s: float): vector => {x: v1.x /. s, y: v1.y /. s, z: v1.z /. s}
let dot = (v1: vector, v2: vector): float => v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z
let norm = (v1: vector): float => dot(v1, v1)->sqrt
let vunit = (v1: vector): vector => vdivs(v1, norm(v1))

type ray = {origin: vector, direction: vector}
let point = (r, d) => vadd(r.origin, vmuls(r.direction, d))

type camera = {eye: vector, lt: vector, rt: vector, lb: vector}

type sphere = {center: vector, radius: float, color: vector, islight: bool}

type hit = {distance: float, point: vector, normal: vector, sphere: sphere}

let usr = (_, _) => %raw(`param >>> param$1`) // uggly hack because asr() does not produce  >>>
let randState = ref((123456789, 362436069, 521288629, 88675123))
let randNext = (): float => {
  let (x, y, z, w) = randState.contents
  let t = usr(lxor(x, lsl(x, 11)), 0)
  let nw = usr(lxor(lxor(w, usr(w, 19)), lxor(t, usr(t, 8))), 0)
  randState := (y, z, w, nw)
  float_of_int(nw) /. 4294967295.
}

let sphit = (sp, ry): option<hit> => {
  let oc = vsub(ry.origin, sp.center)
  let a = dot(ry.direction, ry.direction)
  let b = dot(oc, ry.direction)
  let c = dot(oc, oc) -. sp.radius *. sp.radius
  let dis = b *. b -. a *. c

  if dis > 0. {
    let e = sqrt(dis)
    let t = (-.b -. e) /. a
    let t2 = (-.b +. e) /. a
    if t > 0.007 {
      let pt = point(ry, t)
      Some({distance: t, point: pt, normal: vsub(pt, sp.center)->vunit, sphere: sp})
    } else if t2 > 0.007 {
      let pt2 = point(ry, t2)
      Some({distance: t2, point: pt2, normal: vsub(pt2, sp.center)->vunit, sphere: sp})
    } else {
      None
    }
  } else {
    None
  }
}

type world = {camera: camera, spheres: array<sphere>}
let world = {
  camera: {
    eye: {x: 0., y: 4.5, z: 75.},
    lt: {x: -8., y: 9., z: 50.},
    rt: {x: 8., y: 9., z: 50.},
    lb: {x: -8., y: 0., z: 50.},
  },
  spheres: [
    {
      center: {x: 0., y: -10002., z: 0.},
      radius: 9999.,
      color: {x: 1., y: 1., z: 1.},
      islight: false,
    },
    {
      center: {x: -10012., y: 0., z: 0.},
      radius: 9999.,
      color: {x: 1., y: 0., z: 0.},
      islight: false,
    },
    {
      center: {x: 10012., y: 0., z: 0.},
      radius: 9999.,
      color: {x: 0., y: 1., z: 0.},
      islight: false,
    },
    {
      center: {x: 0., y: 0., z: -10012.},
      radius: 9999.,
      color: {x: 1., y: 1., z: 1.},
      islight: false,
    },
    {center: {x: 0., y: 10012., z: 0.}, radius: 9999., color: {x: 1., y: 1., z: 1.}, islight: true},
    {center: {x: -5., y: 0., z: 2.}, radius: 2., color: {x: 1., y: 1., z: 0.}, islight: false},
    {center: {x: 0., y: 5., z: -1.}, radius: 4., color: {x: 1., y: 0., z: 0.}, islight: false},
    {center: {x: 8., y: 5., z: -1.}, radius: 2., color: {x: 0., y: 0., z: 1.}, islight: false},
  ],
}

let rnd2 = () => 2. *. randNext() -. 1.

let rec rnddome = normal => {
  let pt = vunit({x: rnd2(), y: rnd2(), z: rnd2()})
  let d = dot(pt, normal)
  if d < 0. {
    rnddome(normal)
  } else {
    pt
  }
}

let rec trace = (world, ray, depth): vector =>
  switch depth {
  | 5 => zero
  | _ => {
      let rayhit = sphit(_, ray)
      let closestHit =
        world.spheres
        ->Belt.Array.map(rayhit)
        ->Belt.Array.reduce(None, (closest, hit) =>
          switch (closest, hit) {
          | (_, None) => closest
          | (Some({distance: d1}), Some({distance: d2})) if d1 < d2 => closest
          | (_, _) => hit
          }
        )
      switch closestHit {
      | None => zero
      | Some({sphere: {islight: true, color}}) => color
      | Some({point, normal, sphere: {color}}) => {
          let nray = {origin: point, direction: rnddome(normal)}
          let ncolor = trace(world, nray, depth + 1)
          let at = dot(nray.direction, normal)
          vmul(color, vmuls(ncolor, at))
        }
      }
    }
  }

let to255Str = v => (v *. 255.99)->int_of_float->Belt.Int.toString
let colorToStr = (color): string =>
  `${color.x->to255Str} ${color.y->to255Str} ${color.z->to255Str} `

let print = (str: string): unit => %raw(`function(str) { process.stdout.write(str); }`)(str)

let writeppm = (data: array<vector>) => {
  print(`P3\n${Belt.Int.toString(width)} ${Belt.Int.toString(height)}\n255\n`)
  Belt.Array.forEach(data, color => color->colorToStr->print)
}

let main = () => {
  let vdu = vdivs(vsub(world.camera.rt, world.camera.lt), float_of_int(width))
  let vdv = vdivs(vsub(world.camera.lb, world.camera.lt), float_of_int(height))
  let samplesF = float_of_int(samples)
  let data = []
  let dirFn = (x, y) =>
    vsub(
      vadd(
        world.camera.lt,
        vadd(vmuls(vdu, float_of_int(x) +. randNext()), vmuls(vdv, float_of_int(y) +. randNext())),
      ),
      world.camera.eye,
    )->vunit

  for p in 0 to width * height - 1 {
    let y = p / width
    let x = mod(p, width)
    let col = ref(zero)
    for _ in 0 to samples - 1 {
      let ray = {origin: world.camera.eye, direction: dirFn(x, y)}
      col := vadd(col.contents, trace(world, ray, 0))
    }
    vdivs(col.contents, samplesF)->Js.Array.push(data)->ignore
  }
  data->writeppm
}
let _ = main()
