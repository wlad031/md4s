package dev.vgerasimov.md4s
package logseq

import models.*

import scala.reflect.{ ClassTag, classTag }

object ops {

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

  extension (text: Text) def ++ (that: Text): Text = Text(text.content ++ that.content)

  def listItem(content: Element, marker: String): MarkdownList.Item =
    MarkdownList.Item(
      List(Paragraph(List(content))),
      MarkdownList.Item.Marker(marker, spacingAfter = Spacing.default())
    )

  def listItem(content: String, marker: String): MarkdownList.Item = listItem(Text(content), marker)

  def classicInternalLink(
    page: String,
    text: Option[String] = None
  ): Link.Internal.Classic =
    Link.Internal.Classic(Link.Location.Internal.Page(page), text)

  def h(
    content: String,
    level: Int,
    status: Option[String] = None,
    propertyDrawer: Option[PropertyDrawer] = None
  ): Heading = h(List(Text(content)), level, status, propertyDrawer)

  def h(
    content: List[Element],
    level: Int,
    status: Option[String],
    propertyDrawer: Option[PropertyDrawer]
  ): Heading =
    Heading(
      Heading.Level(level),
      content,
      status = status.map(s => Status(s, spacingAfter = Some(Spacing.default()))),
      propertyDrawer = propertyDrawer
    )

  private[md4s] def foldTexts[A >: Text](objects: List[A]): List[A] =
    fold[A, Text](objects, _ ++ _)

  extension (document: Document) {

    def append(blocks: List[Block]): Document = document.copy(blocks = document.blocks ++ blocks)

    def setDocumentProperty(node: PropertyDrawer.Node): Document =
      import properties.setOrCreate
      document.copy(propertyDrawer = Some(document.propertyDrawer.setOrCreate(node)))

    def setDocumentPropertyValue(key: String, value: String): Document =
      setDocumentProperty(properties.node(key, value))

    def removeDocumentProperty(key: String): Document =
      import properties.remove
      document.copy(propertyDrawer = document.propertyDrawer.map(_.remove(key)))

    def extractRecursively[D](
      pf: PartialFunction[Block, List[DomainEntityBlock[Block, D]]]
    ): (Option[Document], List[DomainEntityBlock[Block, D]]) = {
      def recPf: PartialFunction[Block, List[DomainEntityBlock[Block, D]]] = pf.orElse {
        case HeadedSection(_, blocks, _)      => blocks.flatMap(recPf)
        case MarkdownList.Unordered(items, _) => items.flatMap(_.blocks).flatMap(recPf)
        case _                                => List.empty[DomainEntityBlock[Block, D]]
      }
      document.blocks.flatMap(recPf) match {
        case Nil => (None, Nil)
        case ls  => (Some(document), ls)
      }
    }

    def findBlockByPropertyNode(predicate: (PropertyDrawer.Node => Boolean)): Option[Block] =
      findBlock {
        case b @ HeadedSection(Heading(_, _, _, _, _, Some(PropertyDrawer(nodes)), _), blocks, _)
            if nodes.exists(predicate) =>
          true
        case b @ Paragraph(_, _, _, _, Some(PropertyDrawer(nodes)), _, _)
            if nodes.exists(predicate) =>
          true
        case _ => false
      }

    def findBlock(predicate: Block => Boolean): Option[Block] = {
      def f(blocks: List[Block]): Option[Block] = {
        blocks.foldLeft(Option.empty[Block])((acc, block) =>
          acc.orElse {
            block match {
              case b if predicate(b)                => Some(b)
              case HeadedSection(_, blocks, _)      => f(blocks)
              case MarkdownList.Unordered(items, _) => f(items.flatMap(_.blocks))
              case MarkdownList.Ordered(items, _)   => f(items.flatMap(_.blocks))
              case _                                => None
            }
          }
        )
      }
      f(document.blocks)
    }
  }

  /** Contains utilities for working with property-related models. */
  object properties {
    import PropertyDrawer.*
    import Node.*

    /** Contains extension methods for [[PropertyDrawer]]. */
    extension (propertyDrawer: PropertyDrawer) {

      /** Appends a node to the property drawer. */
      def append(node: Node): PropertyDrawer =
        propertyDrawer.copy(nodes = propertyDrawer.nodes :+ node)

      /** Alias for [[append]]. */
      def :+ (node: Node): PropertyDrawer = append(node)

      /** Appends a node to the property drawer if it doesn't have a node with the same key. */
      def appendIfDoesntHave(node: Node): PropertyDrawer =
        node match {
          case Node(Key(k, _), _, _) if propertyDrawer.has(k) => propertyDrawer
          case _                                              => append(node)
        }

      /** Prepends a node to the property drawer. */
      def prepend(node: Node): PropertyDrawer =
        propertyDrawer.copy(nodes = node :: propertyDrawer.nodes)

      /** Alias for [[prepend]]. */
      def :: (node: Node): PropertyDrawer = prepend(node)

      /** Gets a node by key. */
      def get(key: String): Option[Node] = get(key) { case n => n }

      def get[A](key: String)(pf: PartialFunction[Node, A]): Option[A] =
        propertyDrawer.nodes
          .find { case Node(Key(k, _), _, _) => k == key }
          .flatMap(pf.lift)

      def set(key: String, value: Value): PropertyDrawer = {
        get(key) match {
          case Some(node) => {
            propertyDrawer.copy(
              nodes = propertyDrawer.nodes.map {
                case n if n == node => n.copy(value = value)
                case n              => n
              }
            )
          }
          case None => append(node(key, value))
        }
      }

      def remove(key: String): PropertyDrawer =
        propertyDrawer.copy(nodes = propertyDrawer.nodes.filterNot(_.key.value == key))

      /** Checks if the property drawer has a node with the given key. */
      def has(key: String): Boolean = get(key).isDefined

      /** Checks if the property drawer has a node with the given key and a non-empty value. */
      def hasNonEmpty(key: String): Boolean = get(key).filter {
        case Node(_, Value.Classic(Text(value) :: Nil), _) => value.nonEmpty
        case Node(
              _,
              Value.CommaSeparated(Value.CommaSeparated.SpacedElement(Text(value), _, _) :: Nil),
              _
            ) =>
          value.nonEmpty
        case _ => false
      }.isDefined
    }

    /** Contains extension methods for [[Option]] of [[PropertyDrawer]]. */
    extension (maybePropertyDrawer: Option[PropertyDrawer]) {

      /** Appends a node to the property drawer.
        *
        * If the property drawer is empty, creates a new one with the given node.
        */
      def appendOrCreate(node: PropertyDrawer.Node): PropertyDrawer = maybePropertyDrawer match {
        case None                 => PropertyDrawer(List(node))
        case Some(propertyDrawer) => propertyDrawer.append(node)
      }

      /** Appends a node to the property drawer if it doesn't have a node with the same key.
        *
        * If the property drawer is empty, creates a new one with the given node.
        */
      def appendIfDoesntHaveOrCreate(node: PropertyDrawer.Node): PropertyDrawer =
        maybePropertyDrawer match {
          case None                 => PropertyDrawer(List(node))
          case Some(propertyDrawer) => propertyDrawer.appendIfDoesntHave(node)
        }

      def setOrCreate(node: PropertyDrawer.Node): PropertyDrawer = maybePropertyDrawer match {
        case None                 => PropertyDrawer(List(node))
        case Some(propertyDrawer) => propertyDrawer.set(node.key.value, node.value)
      }
    }

    /** Creates a property drawer node with the given key and simple text value. */
    def node(key: String, value: String): Node =
      node(key, Node.Value.Classic(List(Text(value))))

    def node(key: String, value: Node.Value): Node =
      Node(Node.Key(key), value)
  }

  /** Contains utilities for working with [[Block]]s. */
  object blocks {
    import ops.properties.{ *, given }

    /** Contains extension methods for [[Block]]s. */
    extension (block: Block) {

      /** Appends a property node to the block. */
      def appendPropertyNode(node: PropertyDrawer.Node): Block =
        appendPropertyNode(_.appendOrCreate(_))(node)

      /** Appends a property node to the block if it doesn't have a node with the same key. */
      def appendPropertyNodeIfDoesntHave(node: PropertyDrawer.Node): Block =
        appendPropertyNode(_.appendIfDoesntHaveOrCreate(_))(node)

      /** Actual implementation for appendPropertyNode* methods. */
      private def appendPropertyNode(
        f: (Option[PropertyDrawer], PropertyDrawer.Node) => PropertyDrawer
      )(node: PropertyDrawer.Node): Block = block match {
        case b @ HeadedSection(h @ Heading(_, _, _, _, _, maybePropertyDrawer, _), _, _) =>
          b.copy(heading = h.copy(propertyDrawer = Some(f(maybePropertyDrawer, node))))
        case b @ Paragraph(_, _, _, _, maybePropertyDrawer, _, _) =>
          b.copy(propertyDrawer = Some(f(maybePropertyDrawer, node)))
        case b => b
      }

      /** Appends a [[CustomProperties]] to the block. */
      def appendCustomProperties(customProperties: CustomProperties): Block =
        appendCustomProperties(_ :+ _)(customProperties)

      /** Appends a [[CustomProperties]] to the block if it doesn't have it with the same name. */
      def appendCustomPropertiesIfNotExists(customProperties: CustomProperties): Block =
        appendCustomProperties((existing, newOne) =>
          if existing.exists(_.blockName == newOne.blockName) then existing else existing :+ newOne
        )(customProperties)

      /** Actual implementation for appendCustomProperties* methods. */
      private def appendCustomProperties(
        f: (List[CustomProperties], CustomProperties) => List[CustomProperties]
      )(customProperties: CustomProperties): Block = block match {
        case b @ Paragraph(_, _, _, _, _, customPropertiesList, _) =>
          b.copy(customProperties = f(customPropertiesList, customProperties))
        case b => b
      }

      def set(elements: List[Element]): Block = block match {
        case b @ HeadedSection(heading, _, _) =>
          b.copy(heading = heading.copy(elements = elements))
        case b: Paragraph => b.copy(elements = elements)
        case b            => b
      }
    }

    object table {
      import models.Table
      import models.Table.*
      import models.Table.Row.*

      extension (table: Table) def + (that: Row): Table = Table(table.rows ++ List(that))

      extension (row: Row)
        def + (that: Row): Table = Table(List(row, that))
        def asTable: Table = Table(List(row))

      extension (cells: Cells) def | (cell: Cell): Cells = Cells(cells.cells ++ List(cell))

      extension (cell: Cell)
        def | (that: Cell): Cells = Cells(List(cell, that))
        def asRow: Cells = Cells(List(cell))

      def $ : String => Cell = content => Cell(List(Text(content)))
      def sep: Separator.type = Separator
    }
  }

}
