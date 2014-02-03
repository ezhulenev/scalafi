package scalafi.garch.forecast

import scalafi.garch.Garch

trait Forecast[G <: Garch] {
  def forecast(n: Int = 10): Seq[G#Forecast]
}