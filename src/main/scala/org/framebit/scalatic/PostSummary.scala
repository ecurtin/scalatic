package org.framebit.scalatic

import org.joda.time.LocalDateTime
import org.joda.time.format.{ISODateTimeFormat, DateTimeFormat}

case class PostSummary(url: String, title: String, date: LocalDateTime)
  extends Ordered[PostSummary] {
  override def compare(that: PostSummary): Int = that.date.compareTo(this.date)
}
object PostSummary {
  val df = DateTimeFormat.shortDateTime()
  val dfIso = ISODateTimeFormat.dateTimeNoMillis()

  def toLink(ps: PostSummary): String = {
    s"${Tabs(2)}<article>\n${Tabs(3)}<header>\n" +
      s"${Tabs(4)}<a href='${ps.url}' class='blog-index-link'>${ps.title}</a>\n" +
      s"${Tabs(4)}<time pubdate datetime='${dfIso.print(ps.date)}' class='blog-index-date'>" +
      s"${df.print(ps.date)}</time>\n${Tabs(3)}</header>\n${Tabs(2)}</article>"
  }
}