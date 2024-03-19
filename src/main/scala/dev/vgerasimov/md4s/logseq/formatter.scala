package dev.vgerasimov.md4s
package logseq

import models.*
import ops.{ *, given }

object formatter:

  private def formatMaybeOrEmpty[A](maybe: Option[A], format: A => String): String =
    maybe.map(format).getOrElse("")

  private def append[A](f: A => String, s: => String): A => String = x => s"${f(x)}$s"
  private def appendLineBreak[A](f: A => String): A => String = append(f, "\n")
  private def appendSpace[A](f: A => String): A => String = append(f, " ")
  private def concatWith[A](ls: List[A], format: A => String, s: => String): String =
    ls.map(format).mkString(s)
  private def concatWithLineBreak[A](ls: List[A], format: A => String): String =
    concatWith(ls, format, "\n")
  private def concatWithSpace[A](ls: List[A], format: A => String): String =
    concatWith(ls, format, " ")
  private def concatMaybesWith[A](ls: List[Option[A]], format: A => String, s: => String): String =
    ls.filter(_.isDefined).map(_.get).map(format).mkString(s)

  private def formatBlockElements(blocks: List[Block]): String =
    concatWithLineBreak(blocks, format)
  private def formatInlineElements(elements: List[Element]): String =
    concatWith(elements, format, "")
  private def formatMaybeSpacing(maybeSpacing: Option[Spacing]): String =
    formatMaybeOrEmpty(maybeSpacing, format)

  def format(document: LogseqMarkdown): String = document match
    case LogseqMarkdown(maybePropertyDrawer, blocks) =>
      val res = StringBuffer()
      maybePropertyDrawer.foreach(x => res.append(formatProperyDrawer(x)))
      if (blocks.nonEmpty)
        if (!res.isEmpty()) res.append("\n")
        res.append(formatBlockElements(blocks))
      res.toString()

  def format(element: Element): String =
    def formatLink(link: Link): String =
      import Link.*
      def formatInternalLink(link: Link.Internal): String =
        import Internal.*
        link match
          case Classic(Location.Internal.Page(page), None) =>
            s"[[${page}]]"
          case Classic(Location.Internal.Page(page), Some(text)) =>
            s"[${text}]([[${page}]])"
          case Classic(Location.Internal.Block(block), None) =>
            s"[[${block}]]"
          case Classic(Location.Internal.Block(block), Some(text)) =>
            s"[${text}](((${block})))"
          case Tag.WithBrackets(Location.Internal.Page(page))    => s"#[[${page}]]"
          case Tag.WithoutBrackets(Location.Internal.Page(page)) => s"#${page}"
      def formatExternalLink(link: Link.External): String =
        link match
          case External(Location.External(location), Some(text)) => s"[${text}](${location})"
          case External(Location.External(location), None) =>
            throw new Exception("External link without text is not supported")
      link match
        case internal: Internal => formatInternalLink(internal)
        case external: External => formatExternalLink(external)

    element match
      case ElementsContainer(elements) => formatInlineElements(elements)
      case Text(content)               => content
      case Emphasis(marker: Emphasis.Marker, contents) =>
        s"${marker.value}${format(contents)}${marker.value}"
      case link: Link => formatLink(link)
      case x          => throw new Exception(s"Unsupported inline element: $x")

  private def formatTable(table: Table): String = {
    import Table.*
    import Row.*

    def formatCell(cell: Cell): String = s"${format(cell.content)}"
    def formatCells(cells: Cells): String = cells match
      case Cells(cells, maybeIndentation) =>
        maybeIndentation.map(format).getOrElse("") + cells.map(formatCell).mkString("|", "|", "|")

    def formatRow(row: Row): String = row match
      case Separator(value, maybeIndentation) => maybeIndentation.map(format).getOrElse("") + value
      case cells: Cells                       => formatCells(cells)

    table.rows.map(formatRow).mkString("\n")
  }

  def format(block: Block): String = block match
    case Paragraph(
          content,
          maybePropertyDrawer,
          maybeCustomProperties,
          planning,
          maybeStatus,
          maybePriority,
          maybeIndentation
        ) =>
      val res = StringBuffer()
      maybeIndentation.foreach(x => res.append(format(x)))
      maybePriority.foreach(x => res.append(format(x)))
      maybeStatus.foreach(x => res.append(format(x)))
      if (content.elements.nonEmpty)
        // if (!res.isEmpty()) res.append("\n")
        res.append(format(content))
      maybePropertyDrawer.foreach(x => res.append("\n").append(formatProperyDrawer(x)))
      if (planning.nonEmpty)
        res.append("\n")
        planning.foreach(x => res.append(format(x)))
      res.toString()
    case Heading(level, maybeContent, maybeStatus, maybePriority, maybePropertyDrawer) =>
      val res = StringBuffer()
      res.append(format(level))
      maybePriority.foreach(x => res.append(format(x)))
      maybeStatus.foreach(x => res.append(format(x)))
      maybeContent.foreach(x => res.append(format(x)))
      maybePropertyDrawer.foreach(x => res.append(formatProperyDrawer(x)))
      res.toString()
    case HeadedSection(heading, content, maybeIndentation) =>
      val res = StringBuffer()
      maybeIndentation.foreach(x => res.append(format(x)))
      res.append(format(heading))
      if (content.nonEmpty) res.append("\n").append(formatBlockElements(content))
      res.toString()
    case MarkdownList.Unordered(items, maybeIndentation) =>
      val indentation = maybeIndentation.map(format).getOrElse("")
      items.map(x => indentation + format(x)).mkString("\n")
    case t: Table => formatTable(t)
    case x        => throw new Exception(s"Unsupported block element: $x")

  private def format(timestamp: Timestamp): String =
    import Timestamp.*

    def formatDate(date: Date): String =
      import Date.*
      def formatYear(year: Year): String = year.value.toString
      def formatMonth(month: Month): String = "%02d".format(month.value)
      def formatDay(day: Day): String = "%02d".format(day.value)
      def formatDayName(dayName: DayName): String = dayName match
        case DayName.Monday    => "Mon"
        case DayName.Tuesday   => "Tue"
        case DayName.Wednesday => "Wed"
        case DayName.Thursday  => "Thu"
        case DayName.Friday    => "Fri"
        case DayName.Saturday  => "Sat"
        case DayName.Sunday    => "Sun"
      date match
        case Date(year, month, day, maybeDayName) =>
          val res = StringBuffer()
          res
            .append(formatYear(year))
            .append("-")
            .append(formatMonth(month))
            .append("-")
            .append(formatDay(day))
          maybeDayName.foreach(x => res.append(" ").append(formatDayName(x)))
          res.toString()

    def formatTime(time: Time): String =
      import Time.*
      def formatHour(hour: Hour): String = hour.value.toString
      def formatMinute(minute: Minute): String = "%02d".format(minute.value)
      time match
        case Time(hour, minute) =>
          val res = StringBuffer()
          res.append(formatHour(hour)).append(":").append(formatMinute(minute))
          res.toString()

    timestamp match
      case Diary(value) => throw new Exception("Diary timestamp is not supported")
      case ActiveTimestamp(date, time, repeaterOrDelay) =>
        val res = StringBuffer()
        res.append("<")
        res.append(formatDate(date))
        time.foreach(x => res.append(" ").append(formatTime(x)))
        repeaterOrDelay.foreach(x => res.append(" ").append(x))
        res.append(">")
        res.toString()
      case InactiveTimestamp(date, time, repeaterOrDelay) =>
        throw new Exception("Inactive timestamp is not supported")
      case ActiveTimestampRange(from, to) =>
        throw new Exception("Active timestamp range is not supported")
      case InactiveTimestampRange(from, to) =>
        throw new Exception("Inactive timestamp range is not supported")

  private def format(planning: Planning): String =
    planning match
      case Planning.Scheduled(
            timestamp,
            maybeSpacingBegoreKeyword,
            maybeSpacingBeforeTimestamp,
            maybeSpacingAfterTimestamp
          ) =>
        val res = StringBuffer()
        maybeSpacingBegoreKeyword.foreach(x => res.append(format(x)))
        res.append("SCHEDULED:")
        maybeSpacingBeforeTimestamp.foreach(x => res.append(format(x)))
        res.append(format(timestamp))
        maybeSpacingAfterTimestamp.foreach(x => res.append(format(x)))
        res.toString()
      case x => throw new Exception(s"Unsupported planning: $x")
  private def format(headingLevel: Heading.Level): String = headingLevel match
    case Heading.Level(value, spacingAfter) => "#" * value + format(spacingAfter)

  private def format(spacing: Spacing): String = spacing.value
  private def format(indentation: Indentation): String = indentation.value
  private def formatProperyDrawer(propertyDrawer: PropertyDrawer): String = {
    import PropertyDrawer.*

    def formatNode(node: Node): String = {
      import Node.*

      def formatKey(key: Key): String = key match
        case Key(value, maybeSpacingAfter) =>
          s"$value::${maybeSpacingAfter.map(format).getOrElse("")}"

      def formatValue(value: Value): String = value match
        case Value(value) => format(value)

      node match
        case Node(key, value, maybeIndentation) =>
          maybeIndentation.map(format).getOrElse("") + formatKey(key) + formatValue(value)
    }

    propertyDrawer.nodes.map(formatNode).mkString("\n")
  }
  private def format(item: MarkdownList.Item): String = item match
    case MarkdownList.Item(content, marker) => format(marker) + formatBlockElements(content)
  private def format(priority: Priority): String = priority match
    case Priority(value, spacingAfter) => s"#$value${formatMaybeSpacing(spacingAfter)}"

  private def format(status: Status): String = status match
    case Status(value, spacingAfter) => s"$value${formatMaybeSpacing(spacingAfter)}"

  private def format(marker: MarkdownList.Item.Marker): String = marker match
    case MarkdownList.Item.Marker(value, spacingAfter) => value + format(spacingAfter)

end formatter
