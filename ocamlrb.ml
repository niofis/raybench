let width = 1280.
let height = 720.
let samples = 50.
let max_depth = 5

type vector = {x:float; y:float; z:float}

let zero = {x=0.; y=0.; z=0.}

let vadd v1 v2 = {x = v1.x +. v2.x; y = v1.y +. v2.y; z = v1.z +. v2.z}
let vsub v1 v2 = {x = v1.x -. v2.x; y = v1.y -. v2.y; z = v1.z -. v2.z}
let vmul v1 v2 = {x = v1.x *. v2.x; y = v1.y *. v2.y; z = v1.z *. v2.z}
let vmuls v1 s = {x = v1.x *. s; y = v1.y *. s; z = v1.z *. s}
let vdiv v1 v2 = {x = v1.x /. v2.x; y = v1.y /. v2.y; z = v1.z /. v2.z}
let vdivs v1 s = {x = v1.x /. s; y = v1.y /. s; z = v1.z /. s}
let dot v1 v2 = (v1.x *. v2.x) +. (v1.y *. v2.y) +. (v1.z *. v2.z)
let norm v1 = sqrt (dot v1 v1)
let vunit v1 = vdivs v1 (norm v1)

type ray = {origin:vector; direction:vector}
let point r d = vadd r.origin (vmuls r.direction d)

type camera = {eye:vector; lt:vector; rt:vector; lb:vector}

type sphere = {center:vector; radius:float; color:vector; islight:bool}

type hit = {distance:float; point:vector; normal:vector; sphere:sphere;}
let nohit = {distance=1e16; point=zero; normal=zero; sphere={center=zero; radius=0.; color=zero; islight=false};}

let sphit sp ry =
  let oc = vsub ry.origin sp.center in
  let a = dot ry.direction ry.direction in
  let b = dot oc ry.direction in
  let c = (dot oc oc) -. (sp.radius *. sp.radius) in
  let dis = (b *. b) -. (a *. c) in
  if dis > 0. then
    let e = sqrt dis in
    let t = (-.b -. e) /. a in
    if t > 0.007 then
      let pt = point ry t in
      {distance = t; point = pt; normal = vsub pt sp.center |> vunit; sphere = sp;}
    else
      let t2 = (-.b +. e) /. a in
      if t2 > 0.007 then
        let pt2 = point ry t2 in
        {distance = t2; point = pt2; normal = vsub pt2 sp.center |> vunit; sphere = sp;}
      else
        nohit
  else
    nohit

type world = {camera:camera; spheres:sphere list}
let world = {
  camera = {eye = {x=0.; y=4.5; z=75.};
            lt = {x=(-8.); y=9.; z=50.};
            rt = {x=8.; y=9.; z=50.};
            lb = {x=(-8.); y=0.; z=50.}};
  spheres = [
    {center = {x=0.; y=(-10002.); z=0.};radius = 9999.;color = {x=1.; y=1.; z=1.};islight = false;};
    {center = {x=(-10012.); y=0.; z=0.};radius = 9999.;color = {x=1.; y=0.; z=0.};islight = false;};
    {center = {x=10012.; y=0.; z=0.};radius = 9999.;color = {x=0.; y=1.; z=0.};islight = false;};
    {center = {x=0.; y=0.; z=(-10012.)};radius = 9999.;color = {x=1.; y=1.; z=1.};islight = false;};
    {center = {x=0.; y=10012.; z=0.};radius = 9999.;color = {x=1.; y=1.; z=1.};islight = true;};
    {center = {x=(-5.); y=0.; z=2.};radius = 2.;color = {x=1.; y=1.; z=0.};islight = false;};
    {center = {x=0.; y=5.; z=(-1.)};radius = 4.;color = {x=1.; y=0.; z=0.};islight = false;};
    {center = {x=8.; y=5.; z=(-1.)};radius = 2.;color = {x=0.; y=0.; z=1.};islight = false;}
  ]}

let rnd2 () = 2. *. Random.float(1.) -. 1.

let rec rnddome normal = 
  let pt = vunit {x=rnd2 (); y=rnd2 (); z=rnd2 ();} in
  let d = dot pt normal in
    if d < 0. then rnddome normal else pt

let rec trace world ray depth =
  let hittest sp = sphit sp ray in
  let hits = List.map hittest world.spheres in
  let compare_hits h1 h2 = if h1.distance < h2.distance then h1 else h2 in
  let closest = List.fold_left compare_hits nohit hits in
  if closest = nohit then
    zero
  else if closest.sphere.islight then
    closest.sphere.color
  else if depth < max_depth then
      let nray = {origin=closest.point; direction = rnddome closest.normal} in
      let ncolor = trace world nray (depth + 1) in
      let at = dot nray.direction closest.normal in
      vmul closest.sphere.color (vmuls ncolor at)
  else
    zero

let to255 v = truncate (v *. 255.99)
let colorToStr ppm color = Printf.fprintf ppm "%i %i %i " (to255 color.x) (to255 color.y) (to255 color.z)

let writeppm data =
  let ppm = open_out "ocamlrb.ppm" in
  Printf.fprintf ppm "P3\n%i %i\n255\n" (truncate width) (truncate height);
  List.iter (fun row -> List.iter (colorToStr ppm) row) data;
  close_out ppm;;

let main () =
  let world = world in
  let vdu = vdivs (vsub world.camera.rt world.camera.lt) width in
  let vdv = vdivs (vsub world.camera.lb world.camera.lt) height in
  let rec samp x y s acc = match s with
    | s when s < samples ->
      let dir = (vunit (vsub 
          (vadd world.camera.lt 
            (vadd 
              (vmuls vdu (x +. Random.float(1.))) 
              (vmuls vdv (y +. Random.float(1.)))))
          world.camera.eye)) in
      let ray = {origin=world.camera.eye; direction = dir} in
      let col = vadd acc (trace world ray 0) in 
        samp x y (s +. 1.) col
    | _ -> acc in
  let rec cols y x acc = if x < width then cols y (x +. 1.) (vdivs (samp x y 0. zero) samples::acc) else acc in
  let rec rows y acc = if y < height then rows (y +. 1.) (cols y 0. []:: acc) else acc in
  writeppm (rows 0. [])
 
let _ = main ();
