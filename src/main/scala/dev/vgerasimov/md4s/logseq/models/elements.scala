package dev.vgerasimov.md4s
package logseq
package models

object elements {

  sealed trait Element

  case class Text(content: String) extends Element

  case class Emphasis(
    marker: Emphasis.Marker,
    elements: List[Element]
  ) extends Element

  object Emphasis:
    sealed trait Marker:
      def value: String
    object Marker:
      case class Bold(override val value: String) extends Marker
      case class Italic(override val value: String) extends Marker
      case class StrikeThrough(override val value: String) extends Marker
      case class Code(override val value: String) extends Marker
      case class Highlight(override val value: String) extends Marker

  sealed trait Link extends Element
  object Link:

    sealed trait Internal extends Link:
      def location: Location.Internal
    object Internal:

      case class Classic(
        override val location: Link.Location.Internal,
        label: Option[String] = None
      ) extends Internal

      sealed trait Tag extends Internal:
        override def location: Location.Internal.Page
      object Tag:
        case class WithBrackets(override val location: Location.Internal.Page) extends Tag
        case class WithoutBrackets(override val location: Location.Internal.Page) extends Tag

    end Internal

    case class External(
      location: Location.External,
      label: Option[String] = None
    ) extends Link

    sealed trait Location
    object Location:
      case class External(value: String) extends Location
      sealed trait Internal extends Location
      object Internal:
        case class Page(value: String) extends Internal
        case class Block(value: String) extends Internal

  sealed trait SimpleBlock extends Element
  object SimpleBlock:
    case class Query(value: String) extends SimpleBlock
    case class Video(location: Link.Location.External) extends SimpleBlock

  case class Image(altText: String, url: String, title: Option[String]) extends Element

  sealed trait LineBreak extends Element
  object LineBreak:
    case object Softbreak extends LineBreak
    case object Hardbreak extends LineBreak

  sealed trait Timestamp extends Element
  object Timestamp:
    sealed trait Active extends Timestamp
    sealed trait Inactive extends Timestamp
    sealed trait Range extends Timestamp

    case class ActiveTimestamp(
      date: Date,
      time: Option[Time],
      repeaterOrDelay: Option[RepeaterOrDelay] = None
    ) extends Timestamp
        with Timestamp.Active

    case class InactiveTimestamp(
      date: Date,
      time: Option[Time],
      repeaterOrDelay: Option[RepeaterOrDelay] = None
    ) extends Timestamp
        with Timestamp.Inactive

    case class ActiveTimestampRange(
      from: ActiveTimestamp,
      to: ActiveTimestamp
    ) extends Timestamp
        with Timestamp.Active
        with Timestamp.Range

    case class InactiveTimestampRange(
      from: InactiveTimestamp,
      to: InactiveTimestamp
    ) extends Timestamp
        with Timestamp.Inactive
        with Timestamp.Range

    case class Time(hour: Time.Hour, minute: Time.Minute)

    object Time:
      case class Hour(value: Int)
      case class Minute(value: Int)

    case class Date(
      year: Date.Year,
      month: Date.Month,
      day: Date.Day,
      dayName: Option[Date.DayName] = None
    )

    object Date:
      case class Year(value: Int)
      case class Month(value: Int)
      case class Day(value: Int)

      enum DayName:
        case Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday

    sealed trait RepeaterOrDelay

    object RepeaterOrDelay:
      sealed trait Repeater extends RepeaterOrDelay
      sealed trait Delay extends RepeaterOrDelay
      case class And(repeater: Repeater, delay: Delay) extends RepeaterOrDelay
      case class CumulateRepeater(value: Value, unit: Unit) extends Repeater
      case class CatchUpRepeater(value: Value, unit: Unit) extends Repeater
      case class RestartRepeater(value: Value, unit: Unit) extends Repeater
      case class AllTypeDelay(value: Value, unit: Unit) extends Delay
      case class FirstTypeDelay(value: Value, unit: Unit) extends Delay

      case class Value(value: Int)

      sealed trait Unit
      object Unit:
        case object Hour extends Unit
        case object Day extends Unit
        case object Week extends Unit
        case object Month extends Unit
        case object Year extends Unit

}
