package dev.vgerasimov.md4s
package logseq

import models.*
import dev.vgerasimov.md4s.logseq.models.MarkdownList.Unordered
import dev.vgerasimov.md4s.logseq.models.Link.ClassicInternalLink
import dev.vgerasimov.md4s.logseq.models.Link.TagInternalLink
import dev.vgerasimov.md4s.logseq.models.Link.ExternalLink
import dev.vgerasimov.md4s.logseq.models.LineBreak.Softbreak
import dev.vgerasimov.md4s.logseq.models.LineBreak.Hardbreak
import dev.vgerasimov.md4s.logseq.models.Timestamp.Diary
import dev.vgerasimov.md4s.logseq.models.Timestamp.ActiveTimestamp
import dev.vgerasimov.md4s.logseq.models.Timestamp.InactiveTimestamp
import dev.vgerasimov.md4s.logseq.models.Timestamp.ActiveTimestampRange
import dev.vgerasimov.md4s.logseq.models.Timestamp.InactiveTimestampRange
import dev.vgerasimov.md4s.logseq.models.Emphasis.Marker

object raw:

  def toRaw(document: MarkdownAST): String =
    toRaw(document.blocks)

  def toRaw(blockElements: List[BlockElement]): String =
    blockElements.map(toRaw).mkString("\n")

  def toRaw(blockElement: BlockElement): String =
    blockElement match
      case HeadedSection(heading, content) => s"${toRaw(heading)}\n${toRaw(content)}"
      case Heading(Some(content), headerLevel, status, priority, propertyDrawer) => s"${"#"*headerLevel} ${toRaw(content)}"
      case Paragraph(content, propertyDrawer) => s"${toRaw(content)}\n"
      case dev.vgerasimov.md4s.logseq.models.MarkdownList.Ordered(items) => items.map(toRaw).mkString
      case Unordered(items) => items.map(toRaw).mkString
      // case HorizontalRuler() =>
      // case Table(rows) =>
      case x => x.toString()

  def toRaw(item: MarkdownList.Item): String =
    item.content.map(toRaw).mkString

  def toRaw(inlineElement: InlineElement): String =
    inlineElement match
      case InlineContainer(elements) => elements.map(toRaw).mkString
      case Text(content) => content
      case Emphasis(marker : Marker.Bold, contents) => s"${marker.value}${toRaw(contents)}${marker.value}"
      case Emphasis(marker : Marker.Code, contents) => s"${marker.value}${toRaw(contents)}${marker.value}"
      case ClassicInternalLink(Link.Location.Internal.Page(value), text) => s"[[${value}]]"
      case ExternalLink(Link.Location.External(value), Some(Text(text))) => s"[$text]($value)"
      case TagInternalLink(location) => s"#${location.value}"
      // case ExternalLink(location, text) =>
      // case Image(altText, url, title) =>
      // case Softbreak =>
      // case Hardbreak =>
      // case Diary(value) =>
      // case ActiveTimestamp(date, time, repeaterOrDelay) =>
      // case InactiveTimestamp(date, time, repeaterOrDelay) =>
      // case ActiveTimestampRange(from, to) =>
      // case InactiveTimestampRange(from, to) =>
      case x => x.toString()
    
    