object scalarb {

  type OrdType = Float

  val WIDTH = 1280
  val HEIGHT = 720
  val SAMPLES = 50
  val MAX_DEPTH = 5

  object Random {
    var x = 123456789.toLong
    var y = 362436069.toLong 
    var z = 521288629.toLong
    var w = 88675123.toLong
    val max = 0x7FFFFFFF

    def next(): Float = {
      val t = Random.x ^ (Random.x << 11)
      Random.x = Random.y
      Random.y = Random.z
      Random.z = Random.w
      Random.w = (Random.w ^ (Random.w >> 19) ^(t ^ (t >> 8)))
      (Random.w & Random.max).toFloat / Random.max.toFloat
    } 
  }

  val spheres = Array(
    Sphere(Vector3(0, -10002, 0), 9999, Vector3(1, 1, 1)),
    Sphere(Vector3(-10012, 0, 0), 9999, Vector3(1, 0, 0)),
    Sphere(Vector3(10012, 0, 0), 9999, Vector3(0, 1, 0)),
    Sphere(Vector3(0, 0, -10012), 9999, Vector3(1, 1, 1)),
    Sphere(Vector3(0, 10012, 0), 9999, Vector3(1, 1, 1), isLight = true),
    Sphere(Vector3(-5, 0, 2), 2, Vector3(1, 1, 0)),
    Sphere(Vector3(0, 5, -1), 4, Vector3(1, 0, 0)),
    Sphere(Vector3(8, 5, -1), 2, Vector3(0, 0, 1))
  )

  case class Vector3(x: OrdType, y: OrdType, z: OrdType) {

    def +(v: Vector3) = Vector3(x + v.x, y + v.y, z + v.z)

    def -(v: Vector3) = Vector3(x - v.x, y - v.y, z - v.z)

    def *(v: Vector3) = Vector3(x * v.x, y * v.y, z * v.z)

    def *(v: OrdType) = Vector3(x * v, y * v, z * v)

    def /(v: Vector3) = Vector3(x / v.x, y / v.y, z / v.z)

    def /(v: OrdType) = Vector3(x / v, y / v, z / v)

    def dot(v: Vector3): OrdType = (x * v.x) + (y * v.y) + (z * v.z)

    def norm: OrdType = math.sqrt(x * x + y * y + z * z).asInstanceOf[OrdType]

    def unit: Vector3 = this / norm

  }

  object Vector3 {
    val Zero = Vector3(0, 0, 0)
  }

  case class Ray(var origin: Vector3, var direction: Vector3) {
    def point(dist: OrdType): Vector3 = origin + (direction * dist)
  }

  case class Hit(sphere: Sphere, dist: OrdType, point: Vector3) {
    def normal: Vector3 = (point - sphere.center).unit
  }

  object Hit {
    val Empty = Hit(null, 1e15f, Vector3.Zero)
  }

  class Camera {
    val eye = Vector3(0.0f, 4.5f, 75.0f)
    val lt = Vector3(-8, 9, 50)
    val rt = Vector3(8, 9, 50)
    val lb = Vector3(-8, 0, 50)
  }

  case class Sphere(center: Vector3, radius: OrdType, color: Vector3, isLight: Boolean = false) {

    def hit(ray: Ray): Hit = {
      val oc = ray.origin - center
      val a = ray.direction.dot(ray.direction)
      val b = oc.dot(ray.direction)
      val c = oc.dot(oc) - radius * radius
      val discriminant = b * b - a * c
      if (discriminant > 0) {
        val e = math.sqrt(discriminant)
        val t1dist = ((-b - e) / a).asInstanceOf[OrdType]
        val t2dist = ((-b + e) / a).asInstanceOf[OrdType]
        if (t1dist > 0.007f) {
          val point = ray.point(t1dist)
          Hit(this, t1dist, point)
        } else if (t2dist > 0.007f) {
          val point = ray.point(t2dist)
          Hit(this, t2dist, point)
        } else
          Hit.Empty
      } else
        Hit.Empty
    }
  }

  def rndDome(normal: Vector3): Vector3 = {
    var p = Vector3.Zero
    do {
      val px = 2 * Random.next() - 1
      val py = 2 * Random.next() - 1
      val pz = 2 * Random.next() - 1
      p = Vector3(px, py, pz).unit
    } while (p.dot(normal) < 0)
    p
  }

  def trace(ray: Ray, depth: Int): Vector3 = {
    if (depth < MAX_DEPTH) {

      var hit = Hit.Empty
      spheres.foreach { sphere =>
        val lh = sphere.hit(ray)
        if (lh.dist > 0.0001f && lh.dist < hit.dist) {
          hit = lh
        }
      }

      if (hit != Hit.Empty) {
        if (!hit.sphere.isLight) {
          val nray = Ray(hit.point, rndDome(hit.normal))
          val ncolor = trace(nray, depth + 1)
          val at = nray.direction.dot(hit.normal)
          hit.sphere.color * (ncolor * at)
        } else {
          hit.sphere.color
        }
      } else
        Vector3.Zero
    } else
      Vector3.Zero
  }

  def writePPM(data: Array[Array[Vector3]]): Unit = {
    print(s"P3\n$WIDTH $HEIGHT\n255\n")

    for (row <- data) {
      for (color <- row) {
        val r = math.floor(color.x * 255.99).toInt
        val g = math.floor(color.y * 255.99).toInt
        val b = math.floor(color.z * 255.99).toInt
        print(s"$r $g $b ")
      }
      print("\n")
    }
  }

  def main(args: Array[String]): Unit = {
    val data = Array.tabulate[Vector3](HEIGHT, WIDTH)((_, _) => Vector3.Zero)
    val cam = new Camera()
    val vdu = (cam.rt - cam.lt) / WIDTH.toFloat
    val vdv = (cam.lb - cam.lt) / HEIGHT.toFloat

    for (y <- 0 until HEIGHT; x <- 0 until WIDTH) {
      data(y)(x) = (0 until SAMPLES).map { _ =>
        val direction = cam.lt + (vdu * (x + Random.next()) + vdv * (y + Random.next()))
        val ray = Ray(cam.eye, (direction - cam.eye).unit)
        trace(ray, 0)
      }.fold(Vector3.Zero)(_ + _) / SAMPLES.toFloat
    }

    writePPM(data)
  }
}

