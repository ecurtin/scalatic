package org.framebit.scalatic

/**
  * Created by emily.curtin on 2/3/16.
  */
object Arguments {

  val defaultSource = "source"
  val defaultTarget = "target"

  private def noEndSlash(str: String) = {
    if(str.endsWith("/") || str.endsWith("\\")) {
      str.dropRight(1)
    } else {
      str
    }
  }

  def validateArgs(scalaticArgs: Array[String])
  : Option[(String,String,String)] =
    scalaticArgs match {
      case Array(aPath) =>
        Some((noEndSlash(aPath), defaultSource, defaultTarget))
      case Array(aPath, aSource) =>
        Some((noEndSlash(aPath), noEndSlash(aSource), defaultTarget))
      case Array(aPath, aSource, aTarget, _*) =>
        Some((noEndSlash(aPath), noEndSlash(aSource), noEndSlash(aTarget)))
      case _ =>
        None
    }
}
