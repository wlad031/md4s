package dev.vgerasimov.md4s
package obsidian
package models

import java.nio.file.Path
import java.nio.channels.{ FileLock, FileChannel }

import common.models.{ Document as CommonDocument, DocumentWithPath as CommonDocumentWithPath }

import blocks.Block
import properties.Frontmatter

export blocks.*
export elements.*
export properties.*
export spacing.*

case class Document(
  frontmatter: Option[Frontmatter] = None,
  blocks: List[Block] = Nil
) extends CommonDocument

case class DocumentWithPath(
  document: Document,
  path: Path,
  channel: Option[FileChannel],
  lock: Option[FileLock]
) extends CommonDocumentWithPath[Document](document, path, channel, lock)
