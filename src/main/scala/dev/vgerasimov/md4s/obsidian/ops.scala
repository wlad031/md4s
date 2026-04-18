package dev.vgerasimov.md4s
package obsidian

import models.*

object ops {

  def h(content: String, level: Int): Heading =
    Heading(level = level, elements = List(Text(content)))

  def p(content: String): Paragraph =
    Paragraph(elements = List(Text(content)))

  def wiki(
    path: Option[String] = None,
    heading: Option[String] = None,
    blockId: Option[String] = None,
    alias: Option[String] = None,
    embed: Boolean = false
  ): Link.Wiki =
    Link.Wiki(
      target = Link.Wiki.Target(path = path, heading = heading, blockId = blockId),
      alias = alias,
      embed = embed
    )

}
