package dev.vgerasimov.md4s
package logseq

import models.*

import scala.reflect.{ ClassTag, classTag }

object ops:

  object table:
    import models.Table
    import models.Table.*
    import models.Table.Row.*

    extension (table: Table)
      def + (that: Row): Table = Table(table.rows ++ List(that))

    extension (row: Row)
      def + (that: Row): Table = Table(List(row, that))
      def asTable: Table = Table(List(row))

    extension (cells: Cells)
      def | (cell: Cell): Cells = Cells(cells.cells ++ List(cell))

    extension (cell: Cell)
      def | (that: Cell): Cells = Cells(List(cell, that))
      def asRow: Cells = Cells(List(cell))

    def $ : String => Cell = content =>
      Cell(InlineContainer(List(Text(content))))
    def sep: Separator.type = Separator

  private def fold[A >: B, B : ClassTag](
    objects: List[A],
    op: (B, B) => B
  ): List[A] = {
    def aIsB(a: A): Boolean = classTag[B].runtimeClass.isInstance(a)
    objects
      .foldLeft[List[A]](Nil)((ls, item) =>
        item match {
          case x if aIsB(x) =>
            ls match {
              case ::(head, next) =>
                if (aIsB(head))
                  op(head.asInstanceOf[B], x.asInstanceOf[B]) :: next
                else x :: head :: next
              case Nil => List(x)
            }
          case x => x :: ls
        }
      )
      .reverse
  }

  extension (text: Text)
    def ++ (that: Text): Text = Text(text.content ++ that.content)

  // extension (marker: TextMarkup.Marker)
  //   def isNestable: Boolean = marker match
  //     case TextMarkup.Marker.Code     => false
  //     case TextMarkup.Marker.Verbatim => false
  //     case _                          => true

  private[md4s] def foldTexts[A >: Text](objects: List[A]): List[A] =
    fold[A, Text](objects, _ ++ _)

  private[md4s] def collapseHeadedSections(
    elements: List[BlockElement]
  ): List[BlockElement] =
    elements
      .foldLeft[List[BlockElement]](Nil)((accumulator, element) =>
        (element, accumulator) match
          case (h: Heading, Nil) => List(HeadedSection(h, Nil))
          case (x, Nil)          => List(x)
          case (
                curH @ Heading(_, lvl, _, _, _),
                (prevSec @ HeadedSection(
                  prevH @ Heading(_, prevLvl, _, _, _),
                  prevContent
                )) :: t
              ) =>
            if (lvl > prevLvl) HeadedSection(prevH, curH :: prevContent) :: t
            else HeadedSection(curH, Nil) :: prevSec :: t
          case (x, HeadedSection(prevH, prevContent) :: t) =>
            HeadedSection(prevH, x :: prevContent) :: t
          case (x, ls) => x :: ls
      )
      .map {
        case HeadedSection(heading, content) =>
          HeadedSection(heading, content.reverse)
        case x => x
      }
      .reverse

// private[md4s] def foldParagraphs[A >: Paragraph](objects: List[A]): List[A] =
//   for {
//     element <- fold[A, Paragraph](objects, _ ++ _)
//   } yield element match {
//     case Paragraph(objects) => Paragraph(foldTexts(objects))
//     case _                  => element
//   }
