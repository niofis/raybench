let width = 1280
let height = 720
let samples = 50
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

type hit = {distance:float; point:vector; normal:vector}

type camera = {eye:vector; lt:vector; rt:vector; lb:vector}

type sphere = {center:vector; radius:float; color:vector; islight:bool}
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
      {distance = t; point = pt; normal = (vunit (vsub pt sp.center))}
    else
      let t2 = (-.b +. e) /. a in
      if t2 > 0.007 then
        let pt2 = point ry t2 in
        {distance = t2; point = pt2; normal = (vunit (vsub pt2 sp.center))}
      else
        {distance = 1e16; point = zero; normal = zero}
  else
    {distance = 1e16; point = zero; normal = zero}

type world = {camera:camera; spheres:sphere array}
let world = {
  camera = {eye = {x=0.; y=4.5; z=75.};
            lt = {x=(-8.); y=9.; z=50.};
            rt = {x=8.; y=9.; z=50.};
            lb = {x=(-8.); y=0.; z=50.}};
  spheres = [| 
    {center = {x=0.; y=(-10002.); z=0.};radius = 9999.;color = {x=1.; y=1.; z=1.};islight = false;};
    {center = {x=(-10002.); y=0.; z=0.};radius = 9999.;color = {x=1.; y=0.; z=0.};islight = false;};
    {center = {x=10002.; y=0.; z=0.};radius = 9999.;color = {x=0.; y=1.; z=0.};islight = false;};
    {center = {x=0.; y=0.; z=(-10002.)};radius = 9999.;color = {x=1.; y=1.; z=1.};islight = false;};
    {center = {x=0.; y=10002.; z=0.};radius = 9999.;color = {x=1.; y=1.; z=1.};islight = true;};
    {center = {x=(-5.); y=0.; z=2.};radius = 2.;color = {x=1.; y=1.; z=0.};islight = false;};
    {center = {x=0.; y=5.; z=(-1.)};radius = 4.;color = {x=1.; y=0.; z=0.};islight = false;};
    {center = {x=8.; y=5.; z=(-1.)};radius = 2.;color = {x=0.; y=0.; z=1.};islight = false;}
  |]}

let rec rnddome normal = 
  let pt = vunit {x=Random.float(1.); y=Random.float(1.); z=Random.float(1.);} in
  let d = dot pt normal in
    if d < 0. then
      rnddome normal
    else
      pt

let rec trace world ray depth =
  if depth >= max_depth then
    zero
  else
    begin
      let hit = {distance=1e16; point=zero; normal=zero} in
      zero
    end
