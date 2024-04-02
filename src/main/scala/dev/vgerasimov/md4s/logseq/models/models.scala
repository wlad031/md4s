package dev.vgerasimov.md4s
package logseq
package models

import common.models.{ Document as CommonDocument }

import blocks.Block
import properties.PropertyDrawer

export blocks.*
export elements.*
export properties.*
export spacing.*

case class Document(
  propertyDrawer: Option[PropertyDrawer] = None,
  blocks: List[Block] = Nil
) extends CommonDocument

case class DomainEntityBlock[B <: Block, D](
  block: B,
  entity: D
)
