width = 1280
height = 720

writePPM = do
  let file = openFile ".
  let header = "P3\n" ++ show width ++ " " ++ show height ++ "\n255\n"
  writeFile "./hsrb.ppm" header

main = do
  writePPM
