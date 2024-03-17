package dev.vgerasimov.md4s
package logseq

import models.*
import ops.{ *, given }
import dev.vgerasimov.md4s.logseq.models.Link.ClassicInternalLink
import dev.vgerasimov.md4s.logseq.inlineElements.Timestamp.Diary
import dev.vgerasimov.md4s.logseq.inlineElements.Timestamp.ActiveTimestamp
import dev.vgerasimov.md4s.logseq.inlineElements.Timestamp.InactiveTimestamp
import dev.vgerasimov.md4s.logseq.inlineElements.Timestamp.ActiveTimestampRange
import dev.vgerasimov.md4s.logseq.inlineElements.Timestamp.InactiveTimestampRange
import dev.vgerasimov.md4s.logseq.inlineElements.Timestamp.Date.DayName

object formatter:

  private def formatMaybeOrEmpty[A](maybe: Option[A], format: A => String): String =
    maybe.map(format).getOrElse("")

  private def append[A](f: A => String, s: => String): A => String = x => s"${f(x)}$s"
  private def appendLineBreak[A](f: A => String): A => String = append(f, "\n")
  private def appendSpace[A](f: A => String): A => String = append(f, " ")
  private def concatWith[A](ls: List[A], format: A => String, s: => String): String = ls.map(format).mkString(s)
  private def concatWithLineBreak[A](ls: List[A], format: A => String): String = concatWith(ls, format, "\n")
  private def concatWithSpace[A](ls: List[A], format: A => String): String = concatWith(ls, format, " ")
  private def concatMaybesWith[A](ls: List[Option[A]], format: A => String, s: => String): String =
    ls.filter(_.isDefined).map(_.get).map(format).mkString(s)

  private def formatBlockElements(blocks: List[BlockElement]): String = concatWithLineBreak(blocks, format)
  private def formatInlineElements(elements: List[InlineElement]): String = concatWith(elements, format, "")
  private def formatMaybePropertyDrawer(maybePropertyDrawer: Option[PropertyDrawer]): String =
    formatMaybeOrEmpty(maybePropertyDrawer, format)
  private def formatMaybeSpacing(maybeSpacing: Option[Spacing]): String =
    formatMaybeOrEmpty(maybeSpacing, format)

  def format(document: LogseqMarkdown): String = document match
    case LogseqMarkdown(maybePropertyDrawer, blocks) =>
      val res = StringBuffer()
      maybePropertyDrawer.foreach(x => res.append(format(x)))
      if (blocks.nonEmpty) 
        if (!res.isEmpty()) res.append("\n")
        res.append(formatBlockElements(blocks))
      res.toString()

  def format(inlineElement: InlineElement): String = inlineElement match
    case InlineContainer(elements) => formatInlineElements(elements)
    case Text(content)             => content
    case Emphasis(marker: Emphasis.Marker, contents) =>
      s"${marker.value}${format(contents)}${marker.value}"
    case ClassicInternalLink(Link.Location.Internal.Page(page), None) =>
      s"[[${page}]]"
    case ClassicInternalLink(Link.Location.Internal.Page(page), Some(text)) =>
      s"[${format(text)}]([[${page})]]"
    case ClassicInternalLink(Link.Location.Internal.Block(block), None) =>
      s"[[${block}]]"
    case ClassicInternalLink(Link.Location.Internal.Block(block), Some(text)) =>
      s"[${format(text)}](((${block})))"
    case x => throw new Exception(s"Unsupported inline element: $x")

  def format(blockElement: BlockElement): String = blockElement match
    case Paragraph(content, maybePropertyDrawer, maybeCustomProperties, planning, maybeStatus, maybePriority, maybeIndentation) =>
      val res = StringBuffer()
      maybeIndentation.foreach(x => res.append(format(x)))
      maybePriority.foreach(x => res.append(format(x)))
      maybeStatus.foreach(x => res.append(format(x)))
      if (content.elements.nonEmpty) 
        // if (!res.isEmpty()) res.append("\n")
        res.append(format(content))
      maybePropertyDrawer.foreach(x => res.append("\n").append(format(x)))
      if (planning.nonEmpty) 
        res.append("\n")
        planning.foreach(x => res.append(format(x)))
      res.toString()
    case Heading(level, maybeContent, maybeStatus, maybePriority, maybePropertyDrawer, maybeIndentation) =>
      val res = StringBuffer()
      maybeIndentation.foreach(x => res.append(format(x)))
      res.append(format(level))
      maybePriority.foreach(x => res.append(format(x)))
      maybeStatus.foreach(x => res.append(format(x)))
      maybeContent.foreach(x => res.append(format(x)))
      maybePropertyDrawer.foreach(x => res.append(format(x)))
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
    case x => throw new Exception(s"Unsupported block element: $x")

  private def format(timestamp: Timestamp): String = timestamp match
    case Diary(value) => throw new Exception("Diary timestamp is not supported")
    case ActiveTimestamp(date, time, repeaterOrDelay) =>
      val res = StringBuffer()
      res.append("<")
      res.append(format(date))
      time.foreach(x => res.append(" ").append(format(x)))
      repeaterOrDelay.foreach(x => res.append(" ").append(x))
      res.append(">")
      res.toString()
    case InactiveTimestamp(date, time, repeaterOrDelay) =>
      throw new Exception("Inactive timestamp is not supported")
    case ActiveTimestampRange(from, to) =>
      throw new Exception("Active timestamp range is not supported")
    case InactiveTimestampRange(from, to) =>
      throw new Exception("Inactive timestamp range is not supported")

  private def format(date: Timestamp.Date): String = date match
    case Timestamp.Date(year, month, day, maybeDayName) => 
      val res = StringBuffer()
      res.append(format(year)).append("-").append(format(month)).append("-").append(format(day))
      maybeDayName.foreach(x => res.append(" ").append(format(x)))
      res.toString()
  private def format(year: Timestamp.Date.Year): String = year.value.toString
  private def format(month: Timestamp.Date.Month): String = "%02d".format(month.value)
  private def format(day: Timestamp.Date.Day): String = "%02d".format(day.value)
  private def format(dayName: Timestamp.Date.DayName): String = dayName match
    case DayName.Monday => "Mon"
    case DayName.Tuesday => "Tue"
    case DayName.Wednesday => "Wed"
    case DayName.Thursday => "Thu"
    case DayName.Friday => "Fri"
    case DayName.Saturday => "Sat"
    case DayName.Sunday => "Sun"

  private def format(time: Timestamp.Time): String = time match
    case Timestamp.Time(hour, minute) => 
      val res = StringBuffer()
      res.append(format(hour)).append(":").append(format(minute))
      res.toString()
  private def format(hour: Timestamp.Time.Hour): String = hour.value.toString
  private def format(minute: Timestamp.Time.Minute): String = "%02d".format(minute.value)

  private def format(planning: Planning): String =
    planning match
      case Planning.Scheduled(timestamp, maybeSpacingBegoreKeyword, maybeSpacingBeforeTimestamp, maybeSpacingAfterTimestamp) => 
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
  private def format(propertyDrawer: PropertyDrawer): String = propertyDrawer.nodes.map {
    // FIXME: This should be much shorter
    case PropertyDrawer.Node(
          name,
          Some(value),
          Some(beforeNameSpacing),
          Some(beforeValueSpacing)
        ) =>
      s"${format(beforeNameSpacing)}$name::${format(beforeValueSpacing)}${format(value)}"
    case PropertyDrawer.Node(name, Some(value), None, Some(beforeValueSpacing)) =>
      s"$name::${format(beforeValueSpacing)}${format(value)}"
    case PropertyDrawer.Node(name, Some(value), Some(beforeNameSpacing), None) =>
      s"${format(beforeNameSpacing)}$name::${format(value)}"
    case PropertyDrawer.Node(name, Some(value), None, None) =>
      s"$name::${format(value)}"
    case PropertyDrawer.Node(name, None, Some(beforeNameSpacing), Some(beforeValueSpacing)) =>
      s"${format(beforeNameSpacing)}$name::${format(beforeValueSpacing)}"
    case PropertyDrawer.Node(name, None, None, Some(beforeValueSpacing)) =>
      s"$name::${format(beforeValueSpacing)}"
    case PropertyDrawer.Node(name, None, Some(beforeNameSpacing), None) =>
      s"${format(beforeNameSpacing)}$name::"
    case PropertyDrawer.Node(name, None, None, None) =>
      s"$name::"
  }
    .mkString("\n")
  private def format(item: MarkdownList.Item): String = item match
    case MarkdownList.Item(content, marker) =>      format(marker) + formatBlockElements(content)
  private def format(priority: Priority): String = priority match
    case Priority(value, spacingAfter) => s"#$value${formatMaybeSpacing(spacingAfter)}"
  
  private def format(status: Status): String = status match
    case Status(value, spacingAfter) => s"$value${formatMaybeSpacing(spacingAfter)}"

  private def format(marker: MarkdownList.Item.Marker): String = marker match
    case MarkdownList.Item.Marker(value, spacingAfter) => value + format(spacingAfter)

end formatter
