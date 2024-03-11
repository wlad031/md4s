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

object formatter:

  def format(document: LogseqMarkdown): String = document match
    case LogseqMarkdown(blocks, Some(propertyDrawer)) =>
      s"${format(propertyDrawer)}\n${blocks.map(format).mkString("\n")}"
    case LogseqMarkdown(blocks, None) => s"${blocks.map(format).mkString("\n")}"

  def format(inlineElement: InlineElement): String = inlineElement match
    case InlineContainer(elements) => elements.map(format).mkString
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
    case Paragraph(content, None, Nil, _, _, indentation) =>
      s"${format(indentation)}${format(content)}"
    case Paragraph(content, Some(propertyDrawer), Nil,  _, _, indentation) =>
      s"${format(indentation)}${format(content)}\n${format(propertyDrawer)}"
    case Paragraph(content, None, planning,  _, _, indentation) =>
      s"${format(indentation)}${format(content)}\n${format(planning)}"
    case Paragraph(content, Some(propertyDrawer), planning,  _, _, indentation) =>
      s"${format(indentation)}${format(content)}\n${format(propertyDrawer)}\n${format(planning)}"
    case Heading(Some(content), headerLevel, None, None, None, indentation) =>
      s"${format(indentation)}${"#" * headerLevel} ${format(content)}"
    case HeadedSection(heading, Nil, indentation) =>
      s"${format(heading)}"
    case HeadedSection(heading, content, indentation) =>
      s"${format(heading)}\n${content.map(format).mkString("\n")}"
    case MarkdownList.Unordered(items, indentation) =>
      items.map(item => s"${format(indentation)}${format(item)}").mkString("\n")
    case x => throw new Exception(s"Unsupported block element: $x")

  private def format(timestamp: Timestamp): String = timestamp match
    case Diary(value) => throw new Exception("Diary timestamp is not supported")
    case ActiveTimestamp(date, time, repeaterOrDelay) =>
      s"<${date} ${time}${repeaterOrDelay.map(" " + _).getOrElse("")}>"
    case InactiveTimestamp(date, time, repeaterOrDelay) =>
      throw new Exception("Inactive timestamp is not supported")
    case ActiveTimestampRange(from, to) =>
      throw new Exception("Active timestamp range is not supported")
    case InactiveTimestampRange(from, to) =>
      throw new Exception("Inactive timestamp range is not supported")

  private def format(planning: List[Planning]): String =
    planning.map {
      case Planning.Scheduled(date, None, _, _) => s"SCHEDULED: <${date}>"
      case Planning.Scheduled(date, Some(spacing), _, _) =>
        s"${format(spacing)}SCHEDULED: <${date}>"
      case Planning.Deadline(date, None, _, _)  => s"DEADLINE: <${date}>"
      case Planning.Deadline(date, Some(spacing), _, _) =>
        s"${format(spacing)}DEADLINE: <${date}>"
      case Planning.Closed(date, None, _, _)    => s"CLOSED: <${date}>"
      case Planning.Closed(date, Some(spacing), _, _) =>
        s"${format(spacing)}CLOSED: <${date}>"
    }.mkString("\n")
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
    case MarkdownList.Item(content, marker, Some(spacingAfterMarker)) =>
      s"${marker}${format(spacingAfterMarker)}${content.map(format).mkString("\n")}"
    case MarkdownList.Item(content, marker, None) =>
      s"${marker}${content.map(format).mkString("\n")}"

end formatter
