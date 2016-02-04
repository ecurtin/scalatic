package org.framebit.scalatic

object Tabs {
  val tab="\t"
  def apply(n: Int) =
    (for (i <- 1 to n) yield tab).mkString
}