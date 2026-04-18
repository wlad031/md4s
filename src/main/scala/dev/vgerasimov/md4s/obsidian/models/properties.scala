package dev.vgerasimov.md4s
package obsidian
package models

object properties {

  case class Frontmatter(
    raw: String,
    values: Map[String, String] = Map.empty
  )

}
