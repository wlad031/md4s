package dev.vgerasimov.md4s
package interop

import java.util.Optional
import java.util.{ ArrayList, LinkedHashMap, Map as JMap }

import dev.vgerasimov.md4s.{ logseq, obsidian }
import dev.vgerasimov.slowparse.POut

final class ParseException(
  message: String,
  parserLabel: String | Null = null
) extends RuntimeException(ParseException.withLabel(message, parserLabel)):

  def label: String | Null = parserLabel
  def getParserLabel: Optional[String] = Optional.ofNullable(parserLabel)

object ParseException:
  private def withLabel(message: String, parserLabel: String | Null): String =
    if parserLabel == null then message else s"$message\nParser label: $parserLabel"

final class ParseResult[D <: AnyRef] private (
  val success: Boolean,
  private val documentValue: D | Null,
  private val errorMessageValue: String | Null,
  private val parserLabelValue: String | Null,
  private val parsedValue: String | Null,
  private val remainingValue: String | Null
):

  def isSuccess: Boolean = success

  def documentOrNull: D | Null = documentValue
  def errorMessageOrNull: String | Null = errorMessageValue
  def parserLabelOrNull: String | Null = parserLabelValue
  def parsedOrNull: String | Null = parsedValue
  def remainingOrNull: String | Null = remainingValue

  def getDocument: Optional[D] = Optional.ofNullable(documentValue.asInstanceOf[D])
  def getErrorMessage: Optional[String] = Optional.ofNullable(errorMessageValue)
  def getParserLabel: Optional[String] = Optional.ofNullable(parserLabelValue)
  def getParsed: Optional[String] = Optional.ofNullable(parsedValue)
  def getRemaining: Optional[String] = Optional.ofNullable(remainingValue)

object ParseResult:

  def success[D <: AnyRef](
    document: D,
    parsed: String,
    remaining: String,
    parserLabel: String | Null = null
  ): ParseResult[D] =
    new ParseResult[D](
      success = true,
      documentValue = document,
      errorMessageValue = null,
      parserLabelValue = parserLabel,
      parsedValue = parsed,
      remainingValue = remaining
    )

  def failure[D <: AnyRef](
    errorMessage: String,
    parserLabel: String | Null = null
  ): ParseResult[D] =
    new ParseResult[D](
      success = false,
      documentValue = null,
      errorMessageValue = errorMessage,
      parserLabelValue = parserLabel,
      parsedValue = null,
      remainingValue = null
    )

final class ParsersInterop(
  logseqParser: logseq.Parser = new logseq.Parser(),
  obsidianParser: obsidian.Parser = new obsidian.Parser()
):

  def this() = this(new logseq.Parser(), new obsidian.Parser())

  private val logseqFormatter = logseq.Formatter()
  private val obsidianFormatter = obsidian.Formatter()

  def parseLogseq(input: String): ParseResult[logseq.models.Document] =
    fromPOut(logseqParser.document(input))

  def parseObsidian(input: String): ParseResult[obsidian.models.Document] =
    fromPOut(obsidianParser.document(input))

  def parseLogseqOrThrow(input: String): logseq.models.Document =
    parseOrThrow(logseqParser.document, input)

  def parseObsidianOrThrow(input: String): obsidian.models.Document =
    parseOrThrow(obsidianParser.document, input)

  def parseLogseqAsData(input: String): ParseResult[JMap[String, AnyRef]] =
    fromPOutAsData(logseqParser.document(input))

  def parseObsidianAsData(input: String): ParseResult[JMap[String, AnyRef]] =
    fromPOutAsData(obsidianParser.document(input))

  def formatLogseq(document: logseq.models.Document): String =
    logseqFormatter.format(document)

  def formatObsidian(document: obsidian.models.Document): String =
    obsidianFormatter.format(document)

  private def fromPOut[D <: AnyRef](out: POut[D]): ParseResult[D] = out match
    case POut.Success(value, parsed, remaining, parserLabel) =>
      ParseResult.success(value, parsed, remaining, parserLabel.orNull)
    case POut.Failure(message, parserLabel) =>
      ParseResult.failure(message, parserLabel.orNull)

  private def fromPOutAsData[D <: AnyRef](out: POut[D]): ParseResult[JMap[String, AnyRef]] = out match
    case POut.Success(value, parsed, remaining, parserLabel) =>
      val data = DataConverter.toJava(value).asInstanceOf[JMap[String, AnyRef]]
      ParseResult.success(data, parsed, remaining, parserLabel.orNull)
    case POut.Failure(message, parserLabel) =>
      ParseResult.failure(message, parserLabel.orNull)

  private def parseOrThrow[D <: AnyRef](parser: String => POut[D], input: String): D =
    parser(input) match
      case POut.Success(value, _, _, _) => value
      case POut.Failure(message, parserLabel) => throw ParseException(message, parserLabel.orNull)

object ParsersInterop:

  private lazy val default: ParsersInterop = new ParsersInterop()

  def defaultInstance(): ParsersInterop = default

object ParsersInteropApi:

  private lazy val default: ParsersInterop = ParsersInterop.defaultInstance()

  def defaultInstance(): ParsersInterop = default

  def parseLogseq(input: String): ParseResult[logseq.models.Document] =
    default.parseLogseq(input)

  def parseObsidian(input: String): ParseResult[obsidian.models.Document] =
    default.parseObsidian(input)

  def parseLogseqOrThrow(input: String): logseq.models.Document =
    default.parseLogseqOrThrow(input)

  def parseObsidianOrThrow(input: String): obsidian.models.Document =
    default.parseObsidianOrThrow(input)

  def parseLogseqAsData(input: String): ParseResult[JMap[String, AnyRef]] =
    default.parseLogseqAsData(input)

  def parseObsidianAsData(input: String): ParseResult[JMap[String, AnyRef]] =
    default.parseObsidianAsData(input)

  def formatLogseq(document: logseq.models.Document): String =
    default.formatLogseq(document)

  def formatObsidian(document: obsidian.models.Document): String =
    default.formatObsidian(document)

private object DataConverter:

  def toJava(value: Matchable | Null): AnyRef = value match
    case null => null

    case v: AnyRef if isJavaSimple(v) => v

    case v: Option[?] =>
      v.map(item => toJava(item.asInstanceOf[Matchable | Null])).orNull

    case v: scala.collection.Map[?, ?] =>
      val out = LinkedHashMap[AnyRef, AnyRef]()
      v.foreach { case (k, vv) =>
        out.put(toJava(k.asInstanceOf[Matchable | Null]), toJava(vv.asInstanceOf[Matchable | Null]))
      }
      out

    case v: Iterable[?] =>
      val out = ArrayList[AnyRef]()
      v.foreach(item => out.add(toJava(item.asInstanceOf[Matchable | Null])))
      out

    case v: Product =>
      val out = LinkedHashMap[String, AnyRef]()
      out.put("_type", v.productPrefix)
      v.productElementNames.zip(v.productIterator).foreach { case (name, item) =>
        out.put(name, toJava(item.asInstanceOf[Matchable | Null]))
      }
      out

    case v: Boolean => java.lang.Boolean.valueOf(v)
    case v: Byte    => java.lang.Byte.valueOf(v)
    case v: Short   => java.lang.Short.valueOf(v)
    case v: Int     => java.lang.Integer.valueOf(v)
    case v: Long    => java.lang.Long.valueOf(v)
    case v: Float   => java.lang.Float.valueOf(v)
    case v: Double  => java.lang.Double.valueOf(v)
    case v: Char    => java.lang.Character.valueOf(v)

    case v => v.toString

  private def isJavaSimple(value: AnyRef): Boolean =
    value.isInstanceOf[String]
      || value.isInstanceOf[java.lang.Boolean]
      || value.isInstanceOf[java.lang.Number]
      || value.isInstanceOf[java.lang.Character]
      || value.isInstanceOf[java.util.UUID]
      || value.isInstanceOf[java.time.temporal.Temporal]
      || value.isInstanceOf[java.net.URI]
      || value.isInstanceOf[java.net.URL]
