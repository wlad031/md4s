package dev.vgerasimov.md4s
package common
package models

import java.nio.file.Path
import java.nio.channels.{ FileLock, FileChannel }

trait Document

trait DocumentWithPath[D <: Document](
  document: D,
  path: Path,
  channel: Option[FileChannel],
  lock: Option[FileLock]
)
