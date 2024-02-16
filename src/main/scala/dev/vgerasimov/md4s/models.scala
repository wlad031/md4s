package dev.vgerasimov.md4s

object models {

  /** Main entity representing entire Markdown document.
    *
    * @param elements
    *   contains any content written before the first headline of the document
    * @param sections
    *   list of all document's highest-level sections
    */
  case class Document(elements: List[Element], sections: List[Section])

  /** Represents Markdown headline.
    *
    * @param level
    *   the level of the headline
    * @param status
    *   optional "to-do" keyword
    * @param priority
    *   optional priority cookie
    * @param title
    *   optional headline title
    */
  case class Headline(
    level: Int,
    status: Option[Headline.Status] = None,
    priority: Option[Headline.Priority] = None,
    title: Option[Headline.Title] = None
  ) {
    require(level > 0, "Headline level cannot be less than 1")
  }

  /** Contains [[Headline]]'s inner models. */
  object Headline {

    /** Represents headline "to-do" keyword. */
    case class Status(value: String)

    /** Represents headline priority cookie. */
    case class Priority(value: Char)

    /** Represents headline title. */
    case class Title(contents: List[Title.Content])
    object Title {

      /** Creates [[Title]] from the given string. */
      def apply(text: String): Title = new Title(List(objects.Text(text)))

      /** Represents any content headline title can contain. */
      sealed trait Content
    }
  }

  /** Represents a section of a Markdown document.
    *
    * @param headline
    *   mandatory headline of the section
    * @param elements
    *   contains all content written before the first subsection of the section
    * @param childSections
    *   contains all direct subsections of the section
    * @param planning
    *   optional section's planning info
    * @param propertyDrawer
    *   optional section's property drawer
    */
  case class Section(
    headline: Headline,
    elements: List[Element] = Nil,
    childSections: List[Section] = Nil,
    planning: Option[Planning] = None,
    propertyDrawer: Option[greater_elements.PropertyDrawer] = None
  )

  case class Planning(info: List[Planning.Info]) extends Element
  object Planning {
    sealed trait Info
    object Info {
      import objects.Timestamp

      case class Deadline(timestamp: Timestamp) extends Info
      case class Scheduled(timestamp: Timestamp) extends Info
      case class Closed(timestamp: Timestamp) extends Info
    }
  }

  sealed trait GreaterElement extends Element

  sealed trait Element

  sealed trait MdObject

  /** Contains implementations for [[GreaterElement]]s. */
  object greater_elements {

    case class PropertyDrawer(nodes: List[elements.NodeProperty]) extends GreaterElement

    case class Table(
      rows: List[elements.TableRow]
    ) extends GreaterElement

    sealed trait PlainList extends GreaterElement
    object PlainList {
      case class Item(
        indentation: Int,
        bullet: Bullet,
        checkbox: Option[Checkbox] = None,
        counterSet: Option[Counter] = None,
        tag: Option[String] = None,
        contents: List[Content] = Nil,
        elements: List[Element] = Nil
      )

      case class Counter(value: String)

      sealed trait Bullet
      object Bullet {
        case class Character(value: Char) extends Bullet
        case class Ordered(value: Counter, character: Char) extends Bullet
      }

      sealed trait Checkbox
      object Checkbox {
        case object Empty extends Checkbox
        case object Checked extends Checkbox
        case object Unchecked extends Checkbox
      }

      sealed trait Content

      case class UnorderedList(items: List[Item]) extends PlainList
      case class OrderedList(items: List[Item]) extends PlainList
    }
  }

  /** Contains implementations for [[Element]]s. */
  object elements {

    case class EmptyLines(length: Int) extends Element

    case class HorizontalRuler(length: Int) extends Element {
      require(length >= 5, "Horizontal ruler should be 5 or more characters long")
    }

    case class Paragraph(objects: List[MdObject]) extends Element {
      def ++ (that: Paragraph): Paragraph = Paragraph(this.objects ++ that.objects)
    }

    case class Comments(lines: List[Comments.Line]) extends Element
    object Comments {
      case class Line(value: String)
    }

    sealed trait TableRow
    object TableRow {
      case object TableSep extends TableRow
      case class TableRowCells(cells: List[objects.TableCell]) extends TableRow
    }

    case class NodeProperty(name: String, value: Option[String] = None) extends Element
  }

  /** Contains implementations for [[MdObject]]s. */
  object objects {
    case object LineBreak extends MdObject

    case class Text(value: String)
        extends MdObject
        with Headline.Title.Content
        with TextMarkup.Content
        with greater_elements.PlainList.Content {
      override def toString: String = value

      def ++ (that: Text): Text = Text(this.value + that.value)
    }

    sealed trait Marker {
      val isNestable: Boolean = this match {
        case Marker.Code     => false
        case Marker.Verbatim => false
        case _               => true
      }

      override def toString: String =
        this match {
          case Marker.Bold          => "*"
          case Marker.Verbatim      => "="
          case Marker.Italic        => "/"
          case Marker.StrikeThrough => "+"
          case Marker.Underline     => "_"
          case Marker.Code          => "~"
        }
    }

    object Marker {
      case object Bold extends Marker
      case object Verbatim extends Marker
      case object Italic extends Marker
      case object StrikeThrough extends Marker
      case object Underline extends Marker
      case object Code extends Marker

      def fromString(s: String): Option[Marker] =
        s match {
          case "*" => Some(Bold)
          case "=" => Some(Verbatim)
          case "/" => Some(Italic)
          case "+" => Some(StrikeThrough)
          case "_" => Some(Underline)
          case "~" => Some(Code)
          case _   => None
        }
    }

    case class TextMarkup(pre: String, marker: Marker, contents: List[TextMarkup.Content])
        extends MdObject
        with Headline.Title.Content
        with TextMarkup.Content
        with greater_elements.PlainList.Content {
      override def toString: String = s"MT($marker${contents}$marker)"
    }

    object TextMarkup {
      sealed trait Content

      def apply(marker: Marker, text: String): TextMarkup =
        new TextMarkup("", marker, List(Text(text)))

      def apply(pre: String, marker: Marker, text: String): TextMarkup =
        new TextMarkup(pre, marker, List(Text(text)))

      def bold(pre: String, value: String): TextMarkup =
        TextMarkup(pre, Marker.Bold, List(Text(value)))

      def verbatim(pre: String, value: String): TextMarkup =
        TextMarkup(pre, Marker.Verbatim, List(Text(value)))

      def italic(pre: String, value: String): TextMarkup =
        TextMarkup(pre, Marker.Italic, List(Text(value)))

      def strikeThrough(pre: String, value: String): TextMarkup =
        TextMarkup(pre, Marker.StrikeThrough, List(Text(value)))

      def underline(pre: String, value: String): TextMarkup =
        TextMarkup(pre, Marker.Underline, List(Text(value)))

      def code(pre: String, value: String): TextMarkup =
        TextMarkup(pre, Marker.Code, List(Text(value)))
    }

    case class Contents(contents: List[MdObject]) {
      override def toString: String = contents.mkString
    }

    object Contents {
      def apply(contents: MdObject*): Contents = new Contents(contents.toList)
    }

    case class Link(
      protocol: Option[String],
      path: String,
      description: Option[Contents] = None
    ) extends MdObject
        with Headline.Title.Content
        with TextMarkup.Content
        with greater_elements.PlainList.Content

    sealed trait Timestamp
        extends MdObject
        with Headline.Title.Content
        with TextMarkup.Content
        with greater_elements.PlainList.Content

    object Timestamp {
      sealed trait Active extends Timestamp
      sealed trait Inactive extends Timestamp
      sealed trait Range extends Timestamp

      case class Diary(value: String) extends Timestamp

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

      object ActiveTimestampRange {
        def apply(from: ActiveTimestamp, toTime: Time): ActiveTimestampRange =
          ActiveTimestampRange(from, from.copy(time = Some(toTime)))
      }

      case class InactiveTimestampRange(
        from: InactiveTimestamp,
        to: InactiveTimestamp
      ) extends Timestamp
          with Timestamp.Inactive
          with Timestamp.Range

      object InactiveTimestampRange {
        def apply(from: InactiveTimestamp, toTime: Time): InactiveTimestampRange =
          InactiveTimestampRange(from, from.copy(time = Some(toTime)))
      }

      case class Time(hour: Time.Hour, minute: Time.Minute) {
        override def toString: String = s"$hour:$minute"
      }

      object Time {
        def of(hour: Int, minute: Int): Time = Time(Hour(hour), Minute(minute))

        case class Hour(value: Int) {
          require(Hour.isValid(value), s"Invalid hour value: $value")

          override def toString: String = f"$value%02d"
        }

        object Hour {

          /** Checks that provided value is valid hour. */
          def isValid(hour: Int): Boolean = 0 <= hour && hour <= 23
        }

        case class Minute(value: Int) {
          require(Minute.isValid(value), s"Invalid minute value: $value")

          override def toString: String = f"$value%02d"
        }

        object Minute {

          /** Checks that provided value is valid minute. */
          def isValid(minute: Int): Boolean = 0 <= minute && minute <= 59
        }
      }

      case class Date(
        year: Date.Year,
        month: Date.Month,
        day: Date.Day,
        dayName: Option[Date.DayName] = None
      ) {
        require(Date.isValid(year, month, day), s"Invalid date: year=$year, month=$month, day=$day")

        override def toString: String =
          dayName match {
            case Some(dn) => s"$year-$month-$day $dn"
            case None     => s"$year-$month-$day"
          }
      }

      object Date {

        def of(year: Int, month: Int, day: Int, dayName: Option[String] = None): Date =
          Date(Year(year), Month(month), Day(day), dayName.flatMap(DayName.fromString))

        /** Checks that provided values represent a valid date. */
        def isValid(year: Year, month: Month, day: Day): Boolean =
          day.value <= month.getNumberOfDays(year.isLeap)

        case class Year(value: Int) {
          require(Year.isValid(value), s"Invalid year value: $value")

          /** Indicates whether this year is leap or not. */
          def isLeap: Boolean =
            if (value % 4 != 0) false
            else if (value % 100 != 0) true
            else if (value % 400 != 0) false
            else true

          override def toString: String = f"$value%04d"
        }

        object Year {

          /** Checks that provided value is valid year. */
          def isValid(year: Int): Boolean = 0 <= year
        }

        case class Month(value: Int) {
          require(Month.isValid(value), s"Invalid month value: $value")

          /** Returns the number of days in this month. */
          def getNumberOfDays(isLeapYear: Boolean = false): Int =
            if (value == 2 && isLeapYear) Month.days(value - 1) + 1
            else Month.days(value - 1)

          override def toString: String = f"$value%02d"
        }

        object Month {

          /** Checks that provided value is valid month. */
          def isValid(month: Int): Boolean = 1 <= month && month <= 12

          /** Numbers of days in all 12 months for non-leap year. */
          private val days: Array[Int] = Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
        }

        case class Day(value: Int) {
          require(Day.isValid(value), s"Invalid day value: $value")

          override def toString: String = f"$value%02d"
        }

        object Day {

          /** Checks that provided value is valid day. */
          def isValid(day: Int): Boolean = 1 <= day && day <= 31
        }

        sealed trait DayName {
          override def toString: String =
            this match {
              case DayName.Monday    => "Mon"
              case DayName.Tuesday   => "Tue"
              case DayName.Wednesday => "Wed"
              case DayName.Thursday  => "Thu"
              case DayName.Friday    => "Fri"
              case DayName.Saturday  => "Sat"
              case DayName.Sunday    => "Sun"
            }
        }

        object DayName {
          case object Monday extends DayName
          case object Tuesday extends DayName
          case object Wednesday extends DayName
          case object Thursday extends DayName
          case object Friday extends DayName
          case object Saturday extends DayName
          case object Sunday extends DayName

          def fromString(dayName: String): Option[DayName] =
            dayName match {
              case x if Set("Mon") contains x => Some(Monday)
              case x if Set("Tue") contains x => Some(Tuesday)
              case x if Set("Wed") contains x => Some(Wednesday)
              case x if Set("Thu") contains x => Some(Thursday)
              case x if Set("Fri") contains x => Some(Friday)
              case x if Set("Sat") contains x => Some(Saturday)
              case x if Set("Sun") contains x => Some(Sunday)
              case _                          => None
            }
        }
      }

      sealed trait RepeaterOrDelay

      object RepeaterOrDelay {
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

        object Unit {
          case object Hour extends Unit
          case object Day extends Unit
          case object Week extends Unit
          case object Month extends Unit
          case object Year extends Unit

          def fromString(unit: String): Option[Unit] =
            unit match {
              case x if Set("h").contains(x) => Some(Hour)
              case x if Set("d").contains(x) => Some(Day)
              case x if Set("w").contains(x) => Some(Week)
              case x if Set("m").contains(x) => Some(Month)
              case x if Set("y").contains(x) => Some(Year)
              case _                         => None
            }
        }
      }
    }

    case class Duration(hours: Int, minutes: Int)

    sealed trait Clock
    object Clock {
      case class Simple(timestamp: Timestamp.Inactive) extends Clock
      case class WithDuration(timestamp: Timestamp.Inactive, duration: Duration) extends Clock
    }

    sealed trait StatCookie extends MdObject with Headline.Title.Content
    object StatCookie {

      case object EmptyPercent extends StatCookie
      case object EmptyFractional extends StatCookie

      case class Percent(value: Int) extends StatCookie {
        require(Percent.isValid(value), s"Invalid percent stat cookie value: $value")
      }

      object Percent {
        def isValid(percent: Int): Boolean = 0 <= percent && percent <= 100
      }

      case class Fractional(amount: Int, total: Int) extends StatCookie {
        require(
          Fractional.isValid(amount, total),
          s"Invalid values of fractional stat cookie: amount=$amount, total=$total"
        )
      }

      object Fractional {
        def isValid(amount: Int, total: Int): Boolean = 0 <= amount && 0 <= total && amount <= total
      }
    }

    case class TableCell(value: String)

    case class Target(target: String)
    case class RadioTarget(contents: String)
  }

}
