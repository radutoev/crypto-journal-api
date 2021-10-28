package io.softwarechain.cryptojournal
package util

object math {
  //TODO Can I generalize this using Numeric[T]
  //https://stackoverflow.com/questions/17999409/scala-equivalent-of-javas-number

//  def percentageDiff[T](x1: T, x2: T)(implicit n: Numeric[T]) = {
//    import n._
  def percentageDiff(x1: BigDecimal, x2: BigDecimal): BigDecimal =
    if (x2.signum == 0) {
      x1.signum match {
        case -1 => -100
        case 0  => 0
        case 1  => 100
        case _  => 0
      }
    } else {
      ((x2 - x1) / x1) * 100
    }
}
