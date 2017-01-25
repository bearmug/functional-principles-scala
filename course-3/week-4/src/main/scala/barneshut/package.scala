import common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {

    /** center of mass, X-axis */
    def massX: Float

    /** center of mass, Y-axis */
    def massY: Float

    /** total mass of bodies in the cell */
    def mass: Float

    /** cell center, X-axis */
    def centerX: Float

    /** cell center, Y-axis */
    def centerY: Float

    /** length of the side */
    def size: Float

    /** total number of the bodies in the cell */
    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, Seq(b))
  }

  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    val quads = Seq(nw, ne, sw, se)
    def count[T](f: Quad => T)(implicit num: Numeric[T]): T = quads.map((q: Quad) => f(q)).sum

    val centerX: Float = (nw.centerX + se.centerX) / 2
    val centerY: Float = (nw.centerY + se.centerY) / 2
    val size: Float = nw.size * 2
    val mass: Float = count(_.mass)
    val massX: Float = count(q => q.massX * q.mass) / mass
    val massY: Float = count(q => q.massY * q.mass) / mass
    val total: Int = count(_.total)

    def insert(b: Body): Fork = {
      quads.
        map(q => (q, distance(q.centerX, q.centerY, b.x, b.y))).
        reduce((q1, q2) => if (q1._2 <= q2._2) q1 else q2) match {
        case (quad, _) => quad match {
          case q if q eq nw => Fork(nw.insert(b), ne, sw, se)
          case q if q eq ne => Fork(nw, ne.insert(b), sw, se)
          case q if q eq sw => Fork(nw, ne, sw.insert(b), se)
          case q if q eq se => Fork(nw, ne, sw, se.insert(b))
        }
      }
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
  extends Quad {
    val mass = bodies.map(_.mass).sum
    val (massX, massY) = (
      bodies.map(q => q.x * q.mass).sum / mass: Float,
      bodies.map(q => q.y * q.mass).sum / mass: Float)
    val total: Int = bodies.size

    def insert(b: Body): Quad = size match {
      case s if s <= minimumSize => Leaf(centerX, centerY, size, bodies :+ b)
      case _ => {
        val (dt, sz) = (size / 4, size / 2)
        val (nw, ne, sw, se) = (
          Empty(centerX - dt, centerY - dt, sz),
          Empty(centerX + dt, centerY - dt, sz),
          Empty(centerX - dt, centerY + dt, sz),
          Empty(centerX + dt, centerY + dt, sz))
        (bodies :+ b).foldLeft(Fork(nw, ne, sw, se))((f, b) => f.insert(b))
      }
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
          // no force
        case Leaf(_, _, _, bodies) =>
          // add force contribution of each body by calling addForce
          bodies.foreach(b => addForce(b.mass, b.x, b.y))
        case Fork(nw, ne, sw, se) => quad.size / distance(quad.massX, quad.massY, this.x, this.y) match {
          // see if node is far enough from the body,
          // or recursion is needed
          case farEnough if farEnough >= theta => addForce(quad.mass, quad.massX, quad.massY)
          case _ => Seq (nw, ne, sw, se).foreach(traverse)
        }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {
      ???
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      ???
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}
