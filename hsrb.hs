import           Data.List

import           System.IO
import           System.Random

width :: Float
width = 1280.0

height :: Float
height = 720.0

samples :: Int
samples = 50

maxDepth :: Int
maxDepth = 5

world :: World
world =
    World { camera  = Camera (Vector3 0 4.5 75)
                             (Vector3 (-8) 9 50)
                             (Vector3 8 9 50)
                             (Vector3 (-8) 0 50)
          , spheres =
                [ Sphere { center  = Vector3 0 (-10002) 0  --Floor
                         , radius  = 9999
                         , color   = Vector3 1 1 1
                         , isLight = False
                         }
                , Sphere { center  = Vector3 (-10012) 0 0  --Left
                         , radius  = 9999
                         , color   = Vector3 1 0 0
                         , isLight = False
                         }
                , Sphere { center  = Vector3 10012 0 0  --Right
                         , radius  = 9999
                         , color   = Vector3 0 1 0
                         , isLight = False
                         }
                , Sphere { center  = Vector3 0 0 (-10012)  --Back
                         , radius  = 9999
                         , color   = Vector3 1 1 1
                         , isLight = False
                         }
                , Sphere { center  = Vector3 0 10012 0  --Ceiling
                         , radius  = 9999
                         , color   = Vector3 1 1 1
                         , isLight = True
                         }
                , Sphere { center  = Vector3 (-5) 0 2  --Other
                         , radius  = 2
                         , color   = Vector3 1 1 0
                         , isLight = False
                         }
                , Sphere { center  = Vector3 0 5 (-1)
                         , radius  = 4
                         , color   = Vector3 1 0 0
                         , isLight = False
                         }
                , Sphere { center  = Vector3 8 5 (-1)
                         , radius  = 2
                         , color   = Vector3 0 0 1
                         , isLight = False
                         }
                ]
          }

data Vector3 = Vector3 { vx :: !Float, vy :: !Float, vz :: !Float }
    deriving ( Show )

instance Num Vector3 where
    {-# INLINE (+) #-}
    (Vector3 x1 y1 z1) + (Vector3 x2 y2 z2) =
        Vector3 (x1 + x2) (y1 + y2) (z1 + z2)

    {-# INLINE (-) #-}
    (Vector3 x1 y1 z1) - (Vector3 x2 y2 z2) =
        Vector3 (x1 - x2) (y1 - y2) (z1 - z2)

    {-# INLINE (*) #-}
    (Vector3 x1 y1 z1) * (Vector3 x2 y2 z2) =
        Vector3 (x1 * x2) (y1 * y2) (z1 * z2)

    fromInteger n = Vector3 nf nf nf
      where
        nf = fromInteger n

instance Fractional Vector3 where
    {-# INLINE (/) #-}
    (Vector3 x1 y1 z1) / (Vector3 x2 y2 z2) =
        Vector3 (x1 / x2) (y1 / y2) (z1 / z2)

vmulS :: Vector3 -> Float -> Vector3
vmulS (Vector3 x1 y1 z1) s = Vector3 (x1 * s) (y1 * s) (z1 * s)

vdivS :: Vector3 -> Float -> Vector3
vdivS (Vector3 x1 y1 z1) s = Vector3 (x1 / s) (y1 / s) (z1 / s)

vdot :: Vector3 -> Vector3 -> Float
vdot (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

vnorm :: Vector3 -> Float
vnorm (Vector3 x1 y1 z1) = sqrt ((x1 * x1) + (y1 * y1) + (z1 * z1))

vunit :: Vector3 -> Vector3
vunit v1 = v1 `vdivS` vnorm v1

data Ray = Ray { origin :: !Vector3, direction :: !Vector3 }
    deriving ( Show )

data Camera =
    Camera { eye :: !Vector3, lt :: !Vector3, rt :: !Vector3, lb :: !Vector3 }
    deriving ( Show )

data Pixel = Pixel !Float !Float
    deriving ( Show )

data Sphere = Sphere { center  :: !Vector3
                     , radius  :: !Float
                     , color   :: !Vector3
                     , isLight :: !Bool
                     }
    deriving ( Show )

data Hit = Hit { distance :: !Float
               , point    :: !Vector3
               , normal   :: !Vector3
               , hitcolor :: !Vector3
               , sphere   :: !Sphere
               }
    deriving ( Show )

data World = World { camera :: Camera, spheres :: [Sphere] }
    deriving ( Show )

rayGetPoint :: Ray -> Float -> Vector3
rayGetPoint (Ray org dir) dist = org + (dir `vmulS` dist)

sphereGetNormal :: Sphere -> Vector3 -> Vector3
sphereGetNormal (Sphere cntr _ _ _) point' = vunit (point' - cntr)

sphereHit :: Sphere -> Ray -> Maybe Hit
sphereHit sphere' ray =
    let oc = origin ray - center sphere'
        a = direction ray `vdot` direction ray
        b = oc `vdot` direction ray
        c = (oc `vdot` oc) - radius sphere' * radius sphere'
        disc = (b * b) - (a * c)
    in
        if disc <= 0
        then Nothing
        else let e = sqrt disc
             in
                 let t1 = ((-b) - e) / a
                 in
                     if t1 > 0.007
                     then let pnt = rayGetPoint ray t1
                              nrml = sphereGetNormal sphere' pnt
                          in
                              Just (Hit t1 pnt nrml (color sphere') sphere')
                     else let t2 = ((-b) + e) / a
                          in
                              if t2 > 0.007
                              then let pnt2 = rayGetPoint ray t2
                                       nrml2 = sphereGetNormal sphere' pnt2
                                   in
                                       Just (Hit t2
                                                 pnt2
                                                 nrml2
                                                 (color sphere')
                                                 sphere')
                              else Nothing

pixels :: [[Pixel]]
pixels = map (\y -> map (`Pixel` y) [ 0 .. (width - 1) ]) [ 0 .. (height - 1) ]

primRays :: Camera -> [[Pixel]] -> [[[Ray]]]
primRays (Camera eye' lt' rt' lb') = map (map toRay)
  where
    vdu = (rt' - lt') `vdivS` width

    vdv = (lb' - lt') `vdivS` height

    toRay (Pixel x y) = snd $
        mapAccumL (\(h : h' : xs) _ ->
                   ( xs
                   , Ray eye' $ vunit $ (lt' + (vdu `vmulS` (x + h))
                                         + (vdv `vmulS` (y + h'))) - eye'
                   ))
                  rnd
                  [ 0 .. samples ]
      where
        rnd = rndsP $ floor $ y * width + x

toRGBStr :: Vector3 -> String
toRGBStr (Vector3 x y z) = show (floor $ x * 255.99 :: Int) ++ " "
    ++ show (floor $ y * 255.99 :: Int) ++ " "
    ++ show (floor $ z * 255.99 :: Int) ++ " "

writePPM :: [[Vector3]] -> IO ()
writePPM pixels' = do
    file <- openFile "./hsrb.ppm" WriteMode
    let header = "P3\n" ++ show (floor width :: Int) ++ " "
            ++ show (floor height :: Int) ++ "\n255\n"
    hPutStr file header
    hPutStr file (concatMap (\line -> concatMap toRGBStr line ++ "\n") pixels')
    hClose file

rndsP :: Int -> [Float]
rndsP seed = randomRs (0.0, 0.9999) (mkStdGen seed)

rndsD :: Int -> [Float]
rndsD seed = randomRs (-1.0, 1.0) (mkStdGen seed)

rndDome :: [Float] -> Vector3 -> Vector3
rndDome rnds nrml = let p = vunit (Vector3 (head rnds) (rnds !! 1) (rnds !! 2))
                        d = p `vdot` nrml
                    in
                        if d < 0 then rndDome (drop 3 rnds) nrml else p

closestHit :: [Maybe Hit] -> Maybe Hit
closestHit [ x ] = x
closestHit (Nothing : xs) = closestHit xs
closestHit (x : Nothing : xs) = closestHit (x : xs)
closestHit (a@(Just x1) : b@(Just x2) : xs)
    | distance x1 < distance x2 = closestHit (a : xs)
    | otherwise = closestHit (b : xs)
closestHit [] = Nothing

traceRay :: Int -> [Sphere] -> Ray -> Maybe Hit
traceRay depth spheres' ray = mapHit $
    closestHit (map (`sphereHit` ray) spheres')
  where
    mapHit Nothing = Nothing
    mapHit (Just hit)
        | depth >= maxDepth = Nothing
        | isLight $ sphere hit = Just hit
        | otherwise =
            let nray =
                    Ray (point hit)
                        (rndDome (rndsD (floor $ vnorm (point hit) * 1928374))
                                 (normal hit))
                at = direction nray `vdot` normal hit
                nc = getMHitColor $ traceRay (depth + 1) spheres' nray
                ncolor = hitcolor hit * (nc `vmulS` at)
            in
                Just (Hit (distance hit)
                          (point hit)
                          (normal hit)
                          ncolor
                          (sphere hit))

traceLine :: [Sphere] -> [[Ray]] -> [[Maybe Hit]]
traceLine spheres' = map $ map (traceRay 0 spheres')

getMHitColor :: Maybe Hit -> Vector3
getMHitColor Nothing = Vector3 0 0 0
getMHitColor (Just (Hit _ _ _ clr _)) = clr

avgHitsColor :: [Maybe Hit] -> Vector3
avgHitsColor hits = foldr addColor (Vector3 0 0 0) hits
    `vdivS` (fromIntegral samples :: Float)
  where
    addColor (Just (Hit _ _ _ clr _)) acc = clr + acc
    addColor _ acc = acc

render :: World -> [[Vector3]]
render (World cam spheres') =
    let pixels' = pixels
        rays = primRays cam pixels'
        hits = map (traceLine spheres') rays
    in
        map (map avgHitsColor) hits

main :: IO ()
main = writePPM $ render world
