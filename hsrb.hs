import System.IO
import System.Random

width = 1280.0::Float
height = 720.0::Float
samples = 50::Int
max_depth = 5::Int

world = World {
      camera =  Camera (Vector3 0 4.5 75) (Vector3 (-8) 9 50) (Vector3 8 9 50) (Vector3 (-8) 0 50),
      spheres = [
        Sphere {center = Vector3 0 (-10002) 0, --Floor
                radius = 9999,
                color = Vector3 1 1 1,
                isLight = False},
        Sphere {center = Vector3 (-10012) 0 0, --Left
                radius = 9999,
                color = Vector3 1 0 0,
                isLight = False},
        Sphere {center = Vector3 10012 0 0, --Right
                radius = 9999,
                color = Vector3 0 1 0,
                isLight = False},
        Sphere {center = Vector3 0 0 (-10012), --Back
                radius = 9999,
                color = Vector3 1 1 1,
                isLight = False},
        Sphere {center = Vector3 0 10012 0, --Ceiling
                radius = 9999,
                color = Vector3 1 1 1,
                isLight = True},
        Sphere {center = Vector3 (-5) 0 2, --Other
                radius = 2,
                color = Vector3 1 1 0,
                isLight = False},
        Sphere {center = Vector3 0 5 (-1),
                radius = 4,
                color = Vector3 1 0 0,
                isLight = False},
        Sphere {center = Vector3 8 5 (-1),
                radius = 2,
                color = Vector3 0 0 1,
                isLight = False}
      ]}


data Vector3 = Vector3 {vx :: !Float, vy :: !Float, vz :: !Float} deriving (Show)
vadd (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1+x2) (y1+y2) (z1+z2)
vsub (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1-x2) (y1-y2) (z1-z2)
vmul (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1*x2) (y1*y2) (z1*z2)
vmulS (Vector3 x1 y1 z1) s = Vector3 (x1*s) (y1*s) (z1*s)
vdiv (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1/x2) (y1/y2) (z1/z2)
vdivS (Vector3 x1 y1 z1) s = Vector3 (x1/s) (y1/s) (z1/s)
vdot (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (x1*x2) + (y1*y2) + (z1*z2)
vnorm (Vector3 x1 y1 z1) = sqrt ((x1*x1) + (y1*y1) + (z1*z1))
vunit v1 = v1 `vdivS` (vnorm v1)

data Ray = Ray {origin :: !Vector3, direction :: !Vector3} deriving (Show)
data Camera = Camera {eye :: !Vector3, lt :: !Vector3, rt :: !Vector3, lb :: !Vector3} deriving (Show)
data Pixel = Pixel !Float !Float deriving (Show)
data Sphere = Sphere {center :: !Vector3, radius :: !Float, color :: !Vector3, isLight :: !Bool} deriving (Show)
data Hit = Hit {distance :: !Float, point :: !Vector3, normal :: !Vector3, hitcolor :: !Vector3, sphere :: !Sphere} deriving (Show)
data World = World {camera :: Camera, spheres :: [Sphere]} deriving (Show)

emptyHit = Hit 0 (Vector3 0 0 0) (Vector3 0 0 0) (Vector3 0 0 0) (Sphere (Vector3 0 0 0) 0 (Vector3 0 0 0) False)

rayGetPoint :: Ray -> Float -> Vector3
rayGetPoint (Ray org dir) dist = org `vadd` (dir `vmulS` dist)

sphereGetNormal (Sphere cntr _ _ _) point = vunit (point `vsub` cntr)

sphereHit :: Sphere -> Ray -> Maybe Hit
sphereHit sphere ray =
  let oc = (origin ray) `vsub` (center sphere)
      a = (direction ray) `vdot`(direction ray)
      b = oc `vdot` (direction ray)
      c = (oc `vdot`oc) - (radius sphere) * (radius sphere)
      disc = (b * b) - (a * c)
   in if disc <= 0
         then Nothing
         else let e = sqrt disc
               in let t1 = ((-b) - e) / a
                   in if t1 > 0.007
                         then
                         let pnt = rayGetPoint ray t1
                             nrml = sphereGetNormal sphere pnt
                          in Just (Hit t1 pnt nrml (color sphere) sphere)
                          else let t2 = ((-b) + e) / a
                                in if t2 > 0.007
                                      then
                                      let pnt2 = rayGetPoint ray t2
                                          nrml2 = sphereGetNormal sphere pnt2
                                       in Just (Hit t2 pnt2 nrml2 (color sphere) sphere)
                                       else Nothing

pixels :: Float -> Float -> [[Pixel]]
pixels width height =
  map (\y -> map (\x -> (Pixel x y)) [0..(width-1)]) [0..(height-1)]


primRays :: Camera -> [[Pixel]] -> [[[Ray]]]
primRays (Camera eye lt rt lb) pixels' =
  let vdu = (rt `vsub` lt) `vdivS` width
      vdv = (lb `vsub` lt) `vdivS` height
      toRay (Pixel x y) =
        let rnd = rndsP (floor (y*width + x))
         in map (\n -> Ray eye (vunit
                (vsub (lt `vadd` ((vdu `vmulS` (x + (head (drop n rnd))) ) `vadd` (vdv `vmulS` (y + (head (drop (n+1) rnd))) ))) eye))) [1..samples]
   in
    map (\line -> map toRay line) pixels'

toRGBStr :: Vector3 -> String
toRGBStr (Vector3 x y z) = (show $ floor $ x * 255.99) ++ " " ++ (show $ floor $ y * 255.99) ++ " "  ++ (show $ floor $ z * 255.99) ++ " "


writePPM :: [[Vector3]] -> IO()
writePPM pixels = do
  file <- openFile "./hsrb.ppm" WriteMode
  let header = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"
  hPutStr file header
  hPutStr file (concat (map (\line -> (concat (map toRGBStr line)) ++ "\n") pixels))
  hClose file


rndsP :: Int -> [Float]
rndsP seed = randomRs (0.0, 0.9999) (mkStdGen seed)

rndsD :: Int -> [Float]
rndsD seed = randomRs (-1.0, 1.0) (mkStdGen seed)

rndsH :: [Float]
rndsH = randomRs (-0.5, 0.5) (mkStdGen 25)

rndDome :: [Float] -> Vector3 -> Vector3
rndDome rnds nrml =
  let p = vunit (Vector3 (head (drop 0 rnds)) (head (drop 1 rnds)) (head (drop 2 rnds)))
      d = p `vdot` nrml
   in if d < 0 then rndDome (drop 3 rnds) nrml
               else p

closestHit :: [Maybe Hit] -> Maybe Hit
closestHit (x:[]) = x
closestHit (Nothing:xs) = closestHit xs
closestHit (x:Nothing:xs) = closestHit (x:xs)
closestHit (a@(Just x1):b@(Just x2):xs)
  | (distance x1) < (distance x2) = closestHit (a:xs)
  | otherwise = closestHit (b:xs)

traceRay :: Int -> [Sphere] -> Ray -> Maybe Hit
traceRay depth spheres ray = mapHit $ closestHit (map (\s -> sphereHit s ray) spheres)
  where
    mapHit Nothing = Nothing
    mapHit (Just hit) =
         if depth >= max_depth
                then Nothing
                else if (isLight $ sphere hit) == True
                      then Just hit
                      else let nray = Ray (point hit) (rndDome (rndsD (floor $ (vnorm $ point hit) * 1928374)) (normal hit))
                               at = (direction nray) `vdot` (normal hit)
                               nc = getMHitColor $ traceRay (depth + 1) spheres nray
                               ncolor = (hitcolor hit) `vmul` (nc `vmulS` at)
                            in Just (Hit (distance hit) (point hit) (normal hit) (ncolor) (sphere hit))


traceLine :: [Sphere] -> [[Ray]] -> [[Maybe Hit]]
traceLine spheres rayPkg =
  map (\rays -> map (traceRay 0 spheres) rays) rayPkg


getMHitColor :: Maybe Hit -> Vector3
getMHitColor Nothing = Vector3 0 0 0
getMHitColor (Just (Hit _ _ _ clr _)) = clr

avgHitsColor :: [Maybe Hit] -> Vector3
avgHitsColor hits = (foldr addColor (Vector3 0 0 0) hits) `vdivS` (fromIntegral samples :: Float)
  where
    addColor (Just (Hit _ _ _ clr _)) acc = clr `vadd` acc

render :: World -> [[Vector3]]
render (World camera spheres) =
  let pixels' = pixels width height
      rays = primRays camera pixels'
      hits = map (traceLine spheres) rays
   in
    map (\line -> map (\hits -> avgHitsColor hits) line) hits


main = do
  writePPM $ render world
