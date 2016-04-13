import math
WIDTH = 1280
HEIGHT = 720

class Vector3:
  def __init__(self, x, y ,z):
    self.x = x
    self.y = y
    self.z = z
    


def writeppm(data):
  ppm = open("pyrb.ppm", "w")
  ppm.write("P3\n{0} {1}\n255\n".format(WIDTH,HEIGHT))
  for y in range(0,HEIGHT-1):
    for x in range(0,WIDTH-1):
      ppm.write("{0} {1} {2} ".format(
        math.floor(data[y][x].x * 255.99),
        math.floor(data[y][x].y * 255.99),
        math.floor(data[y][x].z * 255.99),
        ))
    ppm.write("\n")
  ppm.close()

def main():
  writeppm([[]])

main()
