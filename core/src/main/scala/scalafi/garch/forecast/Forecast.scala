package scalafi.garch.forecast

import scalafi.garch.OldGarch

trait Forecast[G <: OldGarch] {
  def forecast(n: Int = 10): Seq[G#Forecast]
}