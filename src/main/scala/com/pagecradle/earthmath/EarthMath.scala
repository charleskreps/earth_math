package com.pagecradle.earthmath

import math._

/**
 * http://en.wikipedia.org/wiki/Earth_radius
 * http://www.codeproject.com/KB/dotnet/Zip_code_radius_search.aspx
 *
 * @author ckreps
 */
object EarthMath {

  private val FLTN = 6378137
  private val E = 0.0066943799901413165

  /**
   * Returns the radius of a circle with the same curvature as the
   * earth going north/south at the point found at the given latitude.
   *
   * @param theta Latitude in radians.
   * @return Radius in kilometers.
   */
  def meridionalRadius(theta: Double): Double = {
    (FLTN * (1 - E) / pow(1 - E * (pow(sin(theta), 2)), 1.5)) / 1000
  }

  /**
   * Returns the radius of a circle with the same curvature as the
   * earth going east/west at the point found at the given latitude.
   *
   * @param theta Latitude in radians.
   * @return Radius in kilometers.
   */
  def normalRadius(theta: Double): Double = {
    FLTN / sqrt(1 - E * pow(sin(theta), 2)) / 1000
  }


  /**
   * Give the distance in kilometers of one degree latitude at
   * any point on earth with the given latitude.
   *
   * Given latitude should be in degrees.
   */
  def unitLatitude(latitude: Double): Double = {
    (2 * Pi * meridionalRadius(deg2rad(latitude))) / 360
  }

  /**
   * Gives the distance in kilometers of one degree longitude at
   * any point on earth with the given latitude.  No that isn't a
   * copy paste error.  You only need latitude to get the unit longitude.
   *
   * Given latitude should be in degrees.
   */
  def unitLongitude(latitude: Double): Double = {
    (2 * Pi * normalRadius(deg2rad(latitude))) / 360
  }

  /**
   * Gives the latitudinal degrees the given number of kilometers covers
   * at a point on earth at the given latitude.
   */
  def degreesLatitude(latitude: Double, kilometers: Double): Double = {
    kilometers / unitLatitude(latitude)
  }

  /**
   * Gives the lontitudinal degrees the given number of kilometers covers
   * at a point on earth at the given latitude.   No that isn't a copy
   * paste error.  You only need latitude to get the longitude degrees.
   */
  def degreesLongitude(latitude: Double, kilometers: Double): Double = {
    kilometers / unitLongitude(latitude)
  }

  def deg2rad(deg: Double): Double = {
    deg * (Pi / 180)
  }

  def rad2deg(rad: Double): Double = {
    rad * (180 / Pi)
  }

  /**
   * Returns the distance in kilometers between two lat/long points.
   * This implementation uses spherical law of cosines.  Note this isn't
   * accurate when the lat/long points are very far apart since the earth
   * is an ellipsoid but is more efficient than an ellisoidal
   * algorithm and accurate enough at short distances.
   *
   * Given lat/long points should be in degrees.
   */
  def sphericalDistance(a: Position, b: Position): Double = {
    val Position(latA, latB) = a
    val Position(lngA, lngB) = b
    if (latA == latB && lngA == lngB) {
      0D
    } else {
      val theta = lngA - lngB
      val dist = sin(deg2rad(latA)) *
        sin(deg2rad(latB)) +
        cos(deg2rad(latA)) *
          cos(deg2rad(latB)) *
          cos(deg2rad(theta))
      rad2deg(acos(dist)) * 60 * 1.1515 * 1.609344
    }
  }

}
