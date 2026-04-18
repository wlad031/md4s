package dev.vgerasimov.md4s
package obsidian

import models.*

import dev.vgerasimov.slowparse.*

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

object Parser:

  case class Context(
    listMaxIndent: Int = Context.default.listMaxIndent,
    headingMinLevel: Int = Context.default.headingMinLevel,
    headingMaxLevel: Int = Context.default.headingMaxLevel,
    knownLinkProtocols: List[String] = Context.default.knownLinkProtocols,
    enableCallouts: Boolean = Context.default.enableCallouts,
    enableFootnotes: Boolean = Context.default.enableFootnotes,
    enableComments: Boolean = Context.default.enableComments
  )

  object Context:

    object default:
      val listMaxIndent: Int = 40
      val headingMinLevel: Int = 1
      val headingMaxLevel: Int = 6
      val knownLinkProtocols: List[String] = List("https", "http", "mailto")
      val enableCallouts: Boolean = true
      val enableFootnotes: Boolean = true
      val enableComments: Boolean = true

      private lazy val defaultCtx = Context()
      def apply(): Context = defaultCtx

class Parser(ctx: Parser.Context = Parser.Context.default()):

  private val headingLine: Regex = "^(#{1,6})\\s+(.*)$".r
  private val listItemLine: Regex = "^([ \t]*)([-+*]|\\d+\\.)[ \t]+(.*)$".r
  private val taskPrefix: Regex = "^\\[( |x|X)\\][ \t]+(.*)$".r
  private val calloutLine: Regex = "^[ \t]*>\\s*\\[!([A-Za-z0-9_-]+)\\]([+-])?(?:\\s+(.*))?$".r
  private val quoteLine: Regex = "^[ \t]*>\\s?(.*)$".r
  private val footnoteDefinitionLine: Regex = "^\\[\\^([^\\]]+)\\]:\\s*(.*)$".r

  def document: P[Document] = input =>
    val normalized = normalizeEol(input)
    val lines = normalized.split("\n", -1).toVector
    val (frontmatter, bodyStartIdx) = parseFrontmatter(lines)
    val (blocks, _) = parseBlocks(lines, bodyStartIdx, minIndent = 0)
    POut.Success(Document(frontmatter = frontmatter, blocks = blocks), parsed = input, remaining = "")

  private def parseFrontmatter(lines: Vector[String]): (Option[Frontmatter], Int) =
    if lines.headOption.contains("---") then
      val closingIdx = lines.indexWhere(_ == "---", from = 1)
      if closingIdx > 0 then
        val body = lines.slice(1, closingIdx).mkString("\n")
        val values = lines.slice(1, closingIdx).flatMap(parseFrontmatterLine).toMap
        (Some(Frontmatter(raw = body, values = values)), closingIdx + 1)
      else (None, 0)
    else (None, 0)

  private def parseFrontmatterLine(line: String): Option[(String, String)] =
    line.indexOf(':') match
      case -1 => None
      case idx if idx == 0 => None
      case idx =>
        val key = line.substring(0, idx).trim
        val value = line.substring(idx + 1).trim
        if key.isEmpty then None else Some(key -> value)

  private def parseBlocks(
    lines: Vector[String],
    startIdx: Int,
    minIndent: Int
  ): (List[Block], Int) =
    val blocks = ListBuffer.empty[Block]
    var idx = startIdx
    var stop = false

    while idx < lines.length && !stop do
      val line = lines(idx)
      if line.trim.isEmpty then idx += 1
      else
        val lineIndent = indentationWidth(line)
        if lineIndent < minIndent then stop = true
        else
          val content = stripIndent(line, minIndent)
          content match
            case headingLine(hashes, headingContent)
                if hashes.length >= ctx.headingMinLevel && hashes.length <= ctx.headingMaxLevel =>
              blocks += Heading(level = hashes.length, elements = parseInline(headingContent))
              idx += 1

            case listItemLine(localIndent, _, _) if localIndent.isEmpty =>
              val (list, newIdx) = parseList(lines, idx, minIndent)
              blocks += list
              idx = newIdx

            case x if x.startsWith("```") =>
              val (codeBlock, newIdx) = parseCodeBlock(lines, idx, minIndent)
              blocks += codeBlock
              idx = newIdx

            case calloutLine(calloutType, foldable, title)
                if ctx.enableCallouts =>
              val (callout, newIdx) = parseCallout(lines, idx, minIndent, calloutType, foldable, Option(title))
              blocks += callout
              idx = newIdx

            case quoteLine(_) =>
              val (blockquote, newIdx) = parseBlockquote(lines, idx, minIndent)
              blocks += blockquote
              idx = newIdx

            case footnoteDefinitionLine(id, definition)
                if ctx.enableFootnotes =>
              blocks += FootnoteDefinition(id = id, elements = parseInline(definition))
              idx += 1

            case x if x.startsWith("|") =>
              val (table, newIdx) = parseTable(lines, idx, minIndent)
              blocks += table
              idx = newIdx

            case x if isHorizontalRule(x.trim) =>
              blocks += HorizontalRule(x.trim)
              idx += 1

            case x if ctx.enableComments && x.trim == "%%" =>
              val (commentBlock, newIdx) = parseCommentBlock(lines, idx, minIndent)
              blocks += commentBlock
              idx = newIdx

            case x if ctx.enableComments && x.startsWith("%%") && x.endsWith("%%") && x.length >= 4 =>
              blocks += CommentBlock(content = x.substring(2, x.length - 2))
              idx += 1

            case x =>
              blocks += Paragraph(parseInline(x))
              idx += 1

    (blocks.toList, idx)

  private def parseCodeBlock(
    lines: Vector[String],
    startIdx: Int,
    minIndent: Int
  ): (CodeBlock, Int) =
    val first = stripIndent(lines(startIdx), minIndent)
    val metadata = first.stripPrefix("```").trim match
      case "" => None
      case x  => Some(x)

    val content = ListBuffer.empty[String]
    var idx = startIdx + 1
    var closed = false
    while idx < lines.length && !closed do
      val current = lines(idx)
      if indentationWidth(current) >= minIndent && stripIndent(current, minIndent).startsWith("```") then
        closed = true
        idx += 1
      else
        content += stripIndent(current, minIndent)
        idx += 1

    (CodeBlock(content = content.mkString("\n"), metadata = metadata), idx)

  private def parseCallout(
    lines: Vector[String],
    startIdx: Int,
    minIndent: Int,
    calloutType: String,
    foldable: String,
    title: Option[String]
  ): (Callout, Int) =
    val quoteLines = ListBuffer.empty[String]
    var idx = startIdx + 1
    while idx < lines.length && indentationWidth(lines(idx)) >= minIndent do
      stripIndent(lines(idx), minIndent) match
        case quoteLine(value) =>
          quoteLines += value
          idx += 1
        case _ =>
          return (
            Callout(
              calloutType = calloutType,
              foldable = foldableOption(foldable),
              title = title.filter(_.nonEmpty).map(parseInline).getOrElse(Nil),
              blocks = parseBlocks(quoteLines.toVector, 0, 0)._1
            ),
            idx
          )

    (
      Callout(
        calloutType = calloutType,
        foldable = foldableOption(foldable),
        title = title.filter(_.nonEmpty).map(parseInline).getOrElse(Nil),
        blocks = parseBlocks(quoteLines.toVector, 0, 0)._1
      ),
      idx
    )

  private def parseBlockquote(
    lines: Vector[String],
    startIdx: Int,
    minIndent: Int
  ): (Blockquote, Int) =
    val quoteLines = ListBuffer.empty[String]
    var idx = startIdx
    while idx < lines.length && indentationWidth(lines(idx)) >= minIndent do
      stripIndent(lines(idx), minIndent) match
        case quoteLine(value) =>
          quoteLines += value
          idx += 1
        case _ =>
          return (Blockquote(parseBlocks(quoteLines.toVector, 0, 0)._1), idx)

    (Blockquote(parseBlocks(quoteLines.toVector, 0, 0)._1), idx)

  private def parseList(
    lines: Vector[String],
    startIdx: Int,
    minIndent: Int
  ): (MarkdownList, Int) =
    val items = ListBuffer.empty[MarkdownList.Item]

    val firstContent = stripIndent(lines(startIdx), minIndent)
    val listKind: String = firstContent match
      case listItemLine(_, marker, _) if marker.endsWith(".") => "ordered"
      case _                                                   => "unordered"

    var idx = startIdx
    var stop = false

    while idx < lines.length && !stop do
      if lines(idx).trim.isEmpty then stop = true
      else if indentationWidth(lines(idx)) < minIndent then stop = true
      else
        stripIndent(lines(idx), minIndent) match
          case listItemLine(localIndent, marker, content)
              if localIndent.isEmpty && markerIsSameKind(marker, listKind) =>
            val (checkbox, lineContent) = parseCheckbox(content)
            val firstBlock = parseSingleLineBlock(lineContent)
            val itemIndent = indentationWidth(lines(idx))
            val childMinIndent = itemIndent + 2

            val (children, nextIdx) =
              if idx + 1 < lines.length && indentationWidth(lines(idx + 1)) >= childMinIndent then
                parseBlocks(lines, idx + 1, childMinIndent)
              else (Nil, idx + 1)

            items += MarkdownList.Item(
              blocks = firstBlock.toList ++ children,
              marker = MarkdownList.Item.Marker(marker),
              checkbox = checkbox
            )
            idx = nextIdx

          case _ =>
            stop = true

    val list: MarkdownList = listKind match
      case "ordered"   => MarkdownList.Ordered(items = items.toList)
      case "unordered" => MarkdownList.Unordered(items = items.toList)
      case _            => MarkdownList.Unordered(items = items.toList)
    (list, idx)

  private def parseCheckbox(content: String): (Option[MarkdownList.Item.Checkbox], String) =
    content match
      case taskPrefix(mark, body) =>
        (
          Some(MarkdownList.Item.Checkbox(checked = mark.equalsIgnoreCase("x"))),
          body
        )
      case x => (None, x)

  private def parseSingleLineBlock(content: String): Option[Block] =
    if content.isEmpty then None
    else
      content match
        case headingLine(hashes, headingContent)
            if hashes.length >= ctx.headingMinLevel && hashes.length <= ctx.headingMaxLevel =>
          Some(Heading(level = hashes.length, elements = parseInline(headingContent)))
        case footnoteDefinitionLine(id, definition)
            if ctx.enableFootnotes =>
          Some(FootnoteDefinition(id = id, elements = parseInline(definition)))
        case x =>
          Some(Paragraph(parseInline(x)))

  private def parseCommentBlock(
    lines: Vector[String],
    startIdx: Int,
    minIndent: Int
  ): (CommentBlock, Int) =
    val content = ListBuffer.empty[String]
    var idx = startIdx + 1
    var closed = false

    while idx < lines.length && !closed do
      if indentationWidth(lines(idx)) >= minIndent && stripIndent(lines(idx), minIndent).trim == "%%" then
        closed = true
        idx += 1
      else
        content += stripIndent(lines(idx), minIndent)
        idx += 1

    (CommentBlock(content.mkString("\n")), idx)

  private def parseTable(
    lines: Vector[String],
    startIdx: Int,
    minIndent: Int
  ): (Table, Int) =
    import Table.*
    import Row.*

    val rows = ListBuffer.empty[Row]
    var idx = startIdx
    var stop = false

    while idx < lines.length && !stop do
      if lines(idx).trim.isEmpty || indentationWidth(lines(idx)) < minIndent then
        stop = true
      else
        val content = stripIndent(lines(idx), minIndent)
        if !content.startsWith("|") then stop = true
        else
          val trimmed = content.trim
          if isTableSeparatorRow(trimmed) then rows += Separator(trimmed)
          else
            val cells = splitTableCells(trimmed).map(v => Cell(parseInline(v.trim)))
            rows += Cells(cells)
          idx += 1

    (Table(rows = rows.toList), idx)

  private def splitTableCells(row: String): List[String] =
    row.stripPrefix("|").stripSuffix("|").split("\\|", -1).toList

  private def parseInline(content: String): List[Element] =
    val result = ListBuffer.empty[Element]
    val textBuffer = StringBuilder()
    var idx = 0

    def flushText(): Unit =
      if textBuffer.nonEmpty then
        result += Text(textBuffer.toString)
        textBuffer.clear()

    while idx < content.length do
      parseWiki(content, idx)
        .orElse(parseImage(content, idx))
        .orElse(parseMarkdownLink(content, idx))
        .orElse(parseInlineComment(content, idx))
        .orElse(parseFootnoteReference(content, idx))
        .orElse(parseBareExternalLink(content, idx))
        .orElse(parseEmphasis(content, idx))
        .orElse(parseTag(content, idx)) match
        case Some((element, nextIdx)) =>
          flushText()
          result += element
          idx = nextIdx
        case None =>
          textBuffer += content.charAt(idx)
          idx += 1

    flushText()
    result.toList

  private def parseWiki(input: String, start: Int): Option[(Element, Int)] =
    val (embed, markerLen) =
      if input.startsWith("![[", start) then (true, 3)
      else if input.startsWith("[[", start) then (false, 2)
      else return None

    val end = input.indexOf("]]", start + markerLen)
    if end == -1 then None
    else
      val raw = input.substring(start + markerLen, end)
      val aliasSplit = raw.split("\\|", 2)
      val locationRaw = aliasSplit.headOption.getOrElse("")
      val alias = aliasSplit.lift(1).filter(_.nonEmpty)

      val (path, heading, blockId) =
        val blockSplit = locationRaw.split("#\\^", 2)
        if blockSplit.length == 2 then
          (
            blockSplit.headOption.filter(_.nonEmpty),
            None,
            blockSplit.lift(1).filter(_.nonEmpty)
          )
        else
          val headingSplit = locationRaw.split("#", 2)
          if headingSplit.length == 2 then
            (
              headingSplit.headOption.filter(_.nonEmpty),
              headingSplit.lift(1).filter(_.nonEmpty),
              None
            )
          else (Option(locationRaw).filter(_.nonEmpty), None, None)

      Some(
        Link.Wiki(
          target = Link.Wiki.Target(path = path, heading = heading, blockId = blockId),
          alias = alias,
          embed = embed
        ) -> (end + 2)
      )

  private def parseImage(input: String, start: Int): Option[(Element, Int)] =
    if !input.startsWith("![", start) then None
    else
      val labelEnd = input.indexOf("](", start + 2)
      if labelEnd == -1 then None
      else
        val closeParen = input.indexOf(")", labelEnd + 2)
        if closeParen == -1 then None
        else
          val altText = input.substring(start + 2, labelEnd)
          val body = input.substring(labelEnd + 2, closeParen).trim
          splitLinkBody(body).map { case (url, title) =>
            Image(altText = altText, url = url, title = title) -> (closeParen + 1)
          }

  private def parseMarkdownLink(input: String, start: Int): Option[(Element, Int)] =
    if !input.startsWith("[", start) || input.startsWith("[[", start) || input.startsWith("[^", start)
    then None
    else
      val labelEnd = input.indexOf("](", start + 1)
      if labelEnd == -1 then None
      else
        val closeParen = input.indexOf(")", labelEnd + 2)
        if closeParen == -1 then None
        else
          val label = input.substring(start + 1, labelEnd)
          val body = input.substring(labelEnd + 2, closeParen).trim
          splitLinkBody(body).map { case (url, _) =>
            Link.External(location = url, label = Some(label)) -> (closeParen + 1)
          }

  private def splitLinkBody(body: String): Option[(String, Option[String])] =
    val quoteIdx = body.indexOf('"')
    if quoteIdx == -1 then
      Option(body).filter(_.nonEmpty).map(_ -> None)
    else
      val url = body.substring(0, quoteIdx).trim
      val titlePart = body.substring(quoteIdx).trim
      if !titlePart.endsWith("\"") || !titlePart.startsWith("\"") then None
      else
        val title = titlePart.substring(1, titlePart.length - 1)
        if url.isEmpty then None else Some(url -> Some(title))

  private def parseFootnoteReference(input: String, start: Int): Option[(Element, Int)] =
    if !ctx.enableFootnotes || !input.startsWith("[^", start) then None
    else
      val closeIdx = input.indexOf(']', start + 2)
      if closeIdx == -1 then None
      else
        val id = input.substring(start + 2, closeIdx)
        if id.isEmpty then None
        else Some(FootnoteReference(id) -> (closeIdx + 1))

  private def parseInlineComment(input: String, start: Int): Option[(Element, Int)] =
    if !ctx.enableComments || !input.startsWith("%%", start) then None
    else
      val end = input.indexOf("%%", start + 2)
      if end == -1 then None
      else
        val value = input.substring(start + 2, end)
        Some(Comment(value) -> (end + 2))

  private def parseBareExternalLink(input: String, start: Int): Option[(Element, Int)] =
    val protocolVariants = ctx.knownLinkProtocols.flatMap {
      case "mailto" => List("mailto:")
      case p         => List(s"$p://")
    }.sortBy(-_.length)

    protocolVariants
      .find(prefix => input.startsWith(prefix, start))
      .flatMap { _ =>
        val stopChars = Set(' ', '\t', '\n', '\r', ')', ']', '}', '>')
        var idx = start
        while idx < input.length && !stopChars.contains(input.charAt(idx)) do idx += 1
        val value = input.substring(start, idx)
        Option(value).filter(_.nonEmpty).map(v => Link.External(v, label = None) -> idx)
      }

  private def parseEmphasis(input: String, start: Int): Option[(Element, Int)] =
    val markers = List("**", "__", "~~", "==", "`", "*", "_")
    markers.iterator
      .flatMap { marker =>
        if !input.startsWith(marker, start) then Iterator.empty
        else
          val close = input.indexOf(marker, start + marker.length)
          if close <= start + marker.length then Iterator.empty
          else
            val value = input.substring(start + marker.length, close)
            val markerModel: Emphasis.Marker = marker match
              case "**" | "__" => Emphasis.Marker.Bold(marker)
              case "*" | "_"   => Emphasis.Marker.Italic(marker)
              case "~~"         => Emphasis.Marker.StrikeThrough(marker)
              case "`"          => Emphasis.Marker.Code(marker)
              case "=="         => Emphasis.Marker.Highlight(marker)
              case _            => Emphasis.Marker.Italic(marker)
            val elements = markerModel match
              case _: Emphasis.Marker.Code => List(Text(value))
              case _                       => parseInline(value)
            Iterator.single(Emphasis(markerModel, elements) -> (close + marker.length))
      }
      .take(1)
      .toList
      .headOption

  private def parseTag(input: String, start: Int): Option[(Element, Int)] =
    if start >= input.length || input.charAt(start) != '#' then None
    else
      val boundaryBefore =
        start == 0 || Set(' ', '\t', '\n', '(', '[', '{').contains(input.charAt(start - 1))
      if !boundaryBefore then None
      else
        var idx = start + 1
        while idx < input.length && isTagChar(input.charAt(idx)) do idx += 1
        if idx == start + 1 then None
        else Some(Tag(input.substring(start + 1, idx)) -> idx)

  private def isTagChar(ch: Char): Boolean =
    ch.isLetterOrDigit || ch == '_' || ch == '-' || ch == '/'

  private def markerIsSameKind(marker: String, listKind: String): Boolean =
    listKind match
      case "ordered"   => marker.endsWith(".")
      case "unordered" => !marker.endsWith(".")
      case _            => true

  private def foldableOption(v: String): Option[Boolean] =
    Option(v).flatMap {
      case "+" => Some(true)
      case "-" => Some(false)
      case _   => None
    }

  private def isHorizontalRule(value: String): Boolean =
    val compact = value.filterNot(_.isWhitespace)
    compact.matches("(-{3,}|\\*{3,}|_{3,})")

  private def isTableSeparatorRow(value: String): Boolean =
    val compact = value.filterNot(_.isWhitespace)
    compact.nonEmpty && compact.forall(ch => ch == '|' || ch == '-' || ch == ':') && compact.contains('-')

  private def indentationWidth(line: String): Int =
    var idx = 0
    var width = 0
    while idx < line.length && (line.charAt(idx) == ' ' || line.charAt(idx) == '\t') do
      width += (if line.charAt(idx) == '\t' then 2 else 1)
      idx += 1
    width

  private def stripIndent(line: String, width: Int): String =
    if width <= 0 then line
    else
      var idx = 0
      var consumed = 0
      while idx < line.length && consumed < width && (line.charAt(idx) == ' ' || line.charAt(idx) == '\t') do
        consumed += (if line.charAt(idx) == '\t' then 2 else 1)
        idx += 1
      line.substring(idx)

  private def normalizeEol(value: String): String =
    value.replace("\r\n", "\n").replace('\r', '\n')
