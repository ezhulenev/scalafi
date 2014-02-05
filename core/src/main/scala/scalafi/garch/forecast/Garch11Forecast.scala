package scalafi.garch.forecast

import scalafi.garch.Garch11

class Garch11Forecast(estimate: Garch11#Estimate) extends Forecast[Garch11] {
  override def forecast(n: Int): Seq[Garch11#Forecast] = {

    val last = estimate.err.length - 1

    // Initialize error & sigma from model fit
    var _errSq = math.pow(estimate.err.valueAt(last), 2)
    var _sigmaSq = math.pow(estimate.sigma.valueAt(last), 2)

    println(_errSq)
    println(_sigmaSq)

    // Forecast sigma squared
    val sigmaSqForecast = for (i <- 0 until n) yield {
      val sigmaSq = estimate.omega.value + estimate.alpha.value * _errSq + estimate.beta.value * _sigmaSq
      _errSq = sigmaSq
      _sigmaSq = sigmaSq
      sigmaSq
    }

    sigmaSqForecast.map(v => math.sqrt(math.abs(v))).map(sigma => estimate.model.Forecast(estimate.mu.value, sigma))
  }
}
