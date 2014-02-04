package scalafi.garch

case class EstimatedValue(value: Double, stdError: Double, tValue: Double) {
  override def toString: String = f"^($value%5.5f, se = $stdError%5.5f, t-value = $tValue%5.5f)"
}