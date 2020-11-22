package zio.web.docs

import scala.language.implicitConversions

sealed trait Doc {
  self =>

  def <>(that: Doc): Doc =
    Doc.Append(self, that)

  def |(that: Doc): Doc =
    Doc.Union(self, that)
}

object Doc {
  // Constructors
  case object Empty                                          extends Doc
  final case class CodeBlock(language: String, code: String) extends Doc
  final case class Heading(content: Text, level: Int)        extends Doc
  final case class Paragraph(content: Text)                  extends Doc
  final case class OrderedList(content: Doc)                 extends Doc
  final case class UnorderedList(content: Doc)               extends Doc

  // Operators
  final case class Append(left: Doc, right: Doc) extends Doc
  final case class Union(left: Doc, right: Doc)  extends Doc

  def h1(content: Text): Doc = Heading(content, 1)

  def h2(content: Text): Doc = Heading(content, 2)

  def h3(content: Text): Doc = Heading(content, 3)

  def h4(content: Text): Doc = Heading(content, 4)

  def h5(content: Text): Doc = Heading(content, 5)

  def h6(content: Text): Doc = Heading(content, 6)

  def p(content: Text): Doc = Paragraph(content)

  def codeBlock(language: String, code: String): Doc = CodeBlock(language = language, code = code)

  def ol(content: Doc): Doc = OrderedList(content)

  def ul(content: Doc): Doc = UnorderedList(content)

  def apply(text: String): Doc = Paragraph(Text.Regular(text))

  def asMarkdown(doc: Doc): String = doc match {
    case Append(left, right)     => asMarkdown(left) + asMarkdown(right)
    case Heading(content, level) => s"${"#" * level} ${Text.asMarkdown(content)}\n"
    case Paragraph(content)      => s"${Text.asMarkdown(content)}\n\n"
    case _                       => "👉 unhandled_doc_element 👈"
  }

  implicit def stringToInlineElement(str: String): Text = Text.Regular(str)
  implicit def stringToDoc(str: String): Doc            = p(str)

  sealed trait Text {
    self =>

    def <>(that: Text): Text =
      Text.Append(self, that)
  }

  object Text {
    final case class Regular(value: String)                 extends Text
    final case class Italic(content: Text)                  extends Text
    final case class Bold(content: Text)                    extends Text
    final case class Strikethrough(content: Text)           extends Text
    final case class Code(value: String)                    extends Text
    final case class Link(url: String, description: String) extends Text
    final case class Append(left: Text, right: Text)        extends Text

    def italic(value: Text): Text        = Italic(value)
    def bold(value: Text): Text          = Bold(value)
    def strikethrough(value: Text): Text = Strikethrough(value)

    def asMarkdown(element: Text): String = element match {
      case Append(left, right)    => asMarkdown(left) + asMarkdown(right)
      case Regular(value)         => value
      case Italic(content)        => s"*${asMarkdown(content)}*"
      case Bold(content)          => s"__${asMarkdown(content)}__"
      case Strikethrough(content) => s"~~${asMarkdown(content)}~~"
      case _                      => "👉 unhandled_inline_element 👈"
    }
  }
}
