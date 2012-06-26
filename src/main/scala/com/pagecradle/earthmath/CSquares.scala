package com.pagecradle.earthmath

import BoolLogic.table

import collection.mutable.ListBuffer
import math._

/**
 * An implementation of "C-Squares".  The short explaination is
 * it's a way of grouping lat/long points into grids of various
 * sizes on the earth's surface in a way that can be queried fast.
 * Details:
 * http://www.marine.csiro.au/csquares/spec1-1.htm
 *
 * @author ckreps
 */
object CSquares {

  sealed case class Quadrant(id: Int)

  sealed case class GlobalQuadrant(override val id: Int) extends Quadrant(id)

  object GlobalQuadrant {
    def apply(lat: Double, lng: Double): GlobalQuadrant = {
      table(lat >= 0, lng >= 0, NE, NW, SE, SW)
    }
  }

  object NE extends GlobalQuadrant(1)

  object SE extends GlobalQuadrant(3)

  object SW extends GlobalQuadrant(5)

  object NW extends GlobalQuadrant(7)

  sealed case class IntermediateQuadrant(override val id: Int) extends Quadrant(id)

  object IntermediateQuadrant {
    def apply(latDigit: Int, lngDigit: Int): IntermediateQuadrant = {
      table(latDigit < 5, lngDigit < 5,
        LOW_LAT_LOW_LONG,
        LOW_LAT_HIGH_LONG,
        HIGH_LAT_LOW_LONG,
        HIGH_LAT_HIGH_LONG)
    }
  }

  object LOW_LAT_LOW_LONG extends IntermediateQuadrant(1)

  object LOW_LAT_HIGH_LONG extends IntermediateQuadrant(2)

  object HIGH_LAT_LOW_LONG extends IntermediateQuadrant(3)

  object HIGH_LAT_HIGH_LONG extends IntermediateQuadrant(4)

  sealed case class Degree(value: BigDecimal, finer: Option[Degree], coarser: Option[Degree])

  object Degree10 extends Degree(BigDecimal(10, 0), Some(Degree5), None)

  object Degree5 extends Degree(BigDecimal(5, 0), Some(Degree1), Some(Degree10))

  object Degree1 extends Degree(BigDecimal(1, 0), Some(Degree05), Some(Degree5))

  object Degree05 extends Degree(BigDecimal(5, 1), Some(Degree01), Some(Degree1))

  object Degree01 extends Degree(BigDecimal(1, 1), Some(Degree05), Some(Degree5))

  object Degree005 extends Degree(BigDecimal(5, 2), Some(Degree001), Some(Degree01))

  object Degree001 extends Degree(BigDecimal(1, 2), Some(Degree0005), Some(Degree005))

  object Degree0005 extends Degree(BigDecimal(5, 3), Some(Degree0001), Some(Degree001))

  object Degree0001 extends Degree(BigDecimal(1, 3), Some(Degree00005), Some(Degree0005))

  object Degree00005 extends Degree(BigDecimal(5, 4), Some(Degree00001), Some(Degree0001))

  object Degree00001 extends Degree(BigDecimal(1, 4), None, Some(Degree00005))

  private val BIG_ZERO = BigDecimal(0, 0)

  private def boundaryFrom(lower: BigDecimal, degree: Degree): Boundary = {
    val upper = if (lower < 0) lower - degree.value else lower + degree.value
    Boundary(lower.toDouble, upper.toDouble)
  }

  sealed trait Cycle {
    def resolution: Degree

    def latBoundary: Boundary

    def lngBoundary: Boundary
  }

  trait HasParentCycle[T <: Cycle] extends Cycle {
    def parent: T

    override val resolution = parent.resolution.finer.getOrElse {
      throw new IllegalArgumentException("Finer resolution not supported for " + parent.resolution)
    }
  }

  trait OffsetChaining {
    def chainLatOffset(value: BigDecimal): BigDecimal

    def chainLngOffset(value: BigDecimal): BigDecimal
  }

  trait QuadrantCycle extends Cycle with OffsetChaining {
    def quadrant: Quadrant
  }

  trait LatLngCycle extends Cycle with OffsetChaining {
    def latitudePart: Int

    def longitudePart: Int

    def latBoundary = boundaryFrom(chainLatOffset(BIG_ZERO), resolution)

    def lngBoundary = boundaryFrom(chainLngOffset(BIG_ZERO), resolution)
  }

  case class PartialCycle(quadrant: Quadrant, parent: LatLngCycle)
    extends QuadrantCycle with HasParentCycle[LatLngCycle] {

    def latBoundary = {
      boundaryFrom(parent.chainLatOffset(offsetIfWithin(HIGH_LAT_LOW_LONG, HIGH_LAT_HIGH_LONG)), resolution)
    }

    def lngBoundary = {
      boundaryFrom(parent.chainLngOffset(offsetIfWithin(LOW_LAT_HIGH_LONG, HIGH_LAT_HIGH_LONG)), resolution)
    }

    private def offsetIfWithin(quadrants: Quadrant*): BigDecimal = {
      quadrants.find(_ == quadrant) match {
        case Some(_) => resolution.value
        case None => BIG_ZERO
      }
    }

    def chainLatOffset(value: BigDecimal) = parent.chainLatOffset(value)

    def chainLngOffset(value: BigDecimal) = parent.chainLngOffset(value)
  }

  case class FullCycle(latitudePart: Int, longitudePart: Int, parent: QuadrantCycle)
    extends LatLngCycle with HasParentCycle[QuadrantCycle] {

    def chainLatOffset(value: BigDecimal): BigDecimal = {
      parent.chainLatOffset((resolution.value * latitudePart) + value)
    }

    def chainLngOffset(value: BigDecimal): BigDecimal = {
      parent.chainLngOffset((resolution.value * longitudePart) + value)
    }
  }

  case class RootCycle(quadrant: Quadrant, latitudePart: Int, longitudePart: Int)
    extends QuadrantCycle with LatLngCycle {
    override val resolution = Degree10

    def chainLatOffset(value: BigDecimal): BigDecimal = {
      negateIfInQuad((resolution.value * latitudePart) + value, NE, SE)
    }

    def chainLngOffset(value: BigDecimal): BigDecimal = {
      negateIfInQuad((resolution.value * longitudePart) + value, SW, NW)
    }

    def negateIfInQuad(value: BigDecimal, quadrants: Quadrant*): BigDecimal = {
      quadrants.find(_ == quadrant) match {
        case Some(_) => value * -1D
        case None => value
      }
    }
  }

  case class CSquare(end: Cycle) {

    lazy val toCSquareString = buildCSquareString(end)

    lazy val latBoundary = end.latBoundary
    lazy val lngBoundary = end.lngBoundary

    private def buildCSquareString(next: Cycle, acc: ListBuffer[String] = new ListBuffer[String]): String = {
      if (next.isInstanceOf[RootCycle]) {
        val RootCycle(quad, latPart, lngPart) = next
        acc.prepend("%d%d%d".format(quad.id, latPart, lngPart))
        acc.mkString
      } else {
        val parent = next match {
          case f: FullCycle => {
            acc.prepend("%d%d".format(f.latitudePart, f.longitudePart))
            f.parent
          }
          case p: PartialCycle => {
            acc.prepend(":%d".format(p.quadrant.id))
            p.parent
          }
        }
        buildCSquareString(parent, acc)
      }
    }

    lazy val center: Position = {
      val Boundary(lowLat, upLat) = latBoundary
      val Boundary(lowLng, upLng) = lngBoundary
      Position(
        abs(lowLat - upLat) / 2 + min(lowLat, upLat),
        abs(lowLng - upLng) / 2 + min(lowLng, upLng))
    }
  }

  object CSquare {

    def apply(lat: Double, lng: Double, decimals: Int): CSquare = {

      val lats = splitUp(lat, decimals)
      val lngs = splitUp(lng, decimals)

      val root: LatLngCycle = RootCycle(GlobalQuadrant(lat, lng), lats(1), (lngs(0) + lngs(1).toString).toInt)

      val end = lats.drop(2).zip(lngs.drop(2)).foldLeft(root)((r, p) => {
        val (latDigit, lngDigit) = p
        FullCycle(latDigit, lngDigit, PartialCycle(IntermediateQuadrant(latDigit, lngDigit), r))
      })

      CSquare(end)
    }

    private def splitUp(n: Double, decimals: Int): Seq[Int] = {
      val absBd = BigDecimal(abs(n))
      (-decimals to 2).map(digitAt(absBd, _)).reverse
    }

    private def digitAt(n: BigDecimal, p10: Int): Int = {
      ((n % pow(10, p10 + 1)) / pow(10, p10)).toInt
    }
  }

}
