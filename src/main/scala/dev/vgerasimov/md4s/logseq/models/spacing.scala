package dev.vgerasimov.md4s
package logseq
package models

object spacing {
  
  case class Spacing(value: String = Spacing.default.value)

  object Spacing {

    object default {
      private[Spacing] val value = " "
      private lazy val defaultSpacing: Spacing = Spacing()

      def apply(): Spacing = defaultSpacing
    }
  }

  trait RightSpaced {
    def spacingAfter: Spacing = Spacing.default()
  }

  trait MaybeRightSpaced {
    def spacingAfter: Option[Spacing] = None
  }

  trait LeftSpaced {
    def spacingBefore: Spacing = Spacing.default()
  }

  trait MaybeLeftSpaced {
    def spacingBefore: Option[Spacing] = None
  }

  case class Indentation(
    level: Int = Indentation.default.level,
    value: String = Indentation.default.value
  )

  object Indentation {

    object default {
      private[Indentation] val level = 0
      private[Indentation] val value = ""
      private lazy val defaultIndentation: Indentation = Indentation()

      def apply(): Indentation = defaultIndentation
    }

    def fromLevel(level: Int, char: String = "  "): Indentation =
      Indentation(level, value = char * level)
  }

  trait Indentable {
    def indentation: Indentation = Indentation.default()
  }

  trait MaybeIndentable {
    def indentation: Option[Indentation] = None
  }

}
