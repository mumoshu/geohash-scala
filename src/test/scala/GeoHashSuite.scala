package jp.mumoshu.geohash

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class GeoHashSuite extends FunSuite with ShouldMatchers {
  import GeoHash._

  test("test decoding") {
    val (lat,lng) = decode("dqcw4bnrs6s7")
    lat should be ( 39.02474 plusOrMinus 0.00001)
    lng should be (-76.51100 plusOrMinus 0.00001)
  }

  test("test encoding") {
    val geohash = "dqcw4bnrs6s7"
    val (lat,lng) = decode(geohash)
    encode(lat,lng) should equal (geohash)
  }
}
