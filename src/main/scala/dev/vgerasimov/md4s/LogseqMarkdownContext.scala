package dev.vgerasimov.md4s

/** Configuration of a Logseq Markdown document. */
case class LogseqMarkdownContext(
  statusKeywords: LogseqMarkdownContext.StatusKeywords,
  linkTypes: Seq[String],
)

object LogseqMarkdownContext {

  /** Contains default values for all fields of the [[LogseqMarkdownContext]]. */
  object default {
    val statusKeywords: StatusKeywords = 
      StatusKeywords(Set("TODO", "DOING", "DONE"))
    val linkTypes: Seq[String] = 
      Seq("https", "http", "file")
  }

  /** Default instance of [[OrgContext]]. */
  val defaultCtx: LogseqMarkdownContext = LogseqMarkdownContext(
    statusKeywords = default.statusKeywords,
    linkTypes = default.linkTypes,
  )

  /** Contains collections of valid "to-do" keywords. */
  case class StatusKeywords(values: Set[String]) {
    def contains(s: String): Boolean = values.contains(s)
    def ++ (that: StatusKeywords): StatusKeywords =
      StatusKeywords(this.values ++ that.values)
  }
}
