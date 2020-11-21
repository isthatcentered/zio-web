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
  case object Empty                                                           extends Doc
  final case class Text(value: String, emphasis: Emphasis = Emphasis.Regular) extends Doc
  final case class Code(language: String, code: String)                       extends Doc
  final case class Link(url: String, description: Option[String])             extends Doc

  // Operators
  final case class Append(left: Doc, right: Doc)     extends Doc
  final case class Union(left: Doc, right: Doc)      extends Doc
  final case class Heading(content: Doc, level: Int) extends Doc
  final case class Paragraph(content: Doc)           extends Doc
  final case class OrderedList(items: Doc*)          extends Doc
  final case class UnorderedList(items: Doc*)        extends Doc

  def h1(content: Doc): Doc = Heading(content, 1)

  def h2(content: Doc): Doc = Heading(content, 2)

  def h3(content: Doc): Doc = Heading(content, 3)

  def h4(content: Doc): Doc = Heading(content, 4)

  def h5(content: Doc): Doc = Heading(content, 5)

  def h6(content: Doc): Doc = Heading(content, 6)

  def p(content: Doc): Doc = Paragraph(content)

  def a(url: String, description: String = ""): Doc =
    Link(url = url, description = description match {
      case ""    => None
      case value => Some(value)
    })

  def code(language: String, code: String): Doc = Code(language = language, code = code)

  def ol(items: Doc*): Doc = OrderedList(items: _*)

  def ul(items: Doc*): Doc = UnorderedList(items: _*)

  def italic(value: String): Doc = Text(value, Emphasis.Italic)

  def bold(value: String): Doc = Text(value, Emphasis.Bold)

  def strikethrough(value: String): Doc = Text(value, Emphasis.StrikeThrough)

  def inlineCode(value: String): Doc = Text(value, Emphasis.InlineCode)

  def apply(text: String): Doc = Text(text)

  // Interpreters
  def asMarkdown(doc: Doc): String = doc match {
    case Empty =>
      ""

    case Union(left, right) =>
      left match {
        case Empty => asMarkdown(right)
        case _     => asMarkdown(left)
      }

    case Append(left, right) =>
      asMarkdown(left) + " " + asMarkdown(right)

    case Paragraph(content) =>
      s"${asMarkdown(content)}\n\n"

    case Heading(content, level) =>
      s"${"#" * level} ${asMarkdown(content)}\n\n"

    case Link(url, description) =>
      description match {
        case Some(description) => s"[$description]($url)"
        case None              => s"<$url>"
      }

    case Code(language, code) =>
      s"\n```$language\n$code\n```\n"

    case Text(value, emphasis) =>
      emphasis match {
        case Emphasis.Regular       => value
        case Emphasis.Italic        => s"_${value}_"
        case Emphasis.Bold          => s"**$value**"
        case Emphasis.InlineCode    => s"`$value`"
        case Emphasis.StrikeThrough => s"~~$value~~"
      }

    case OrderedList(items @ _*) =>
      items.zipWithIndex
        .map(item => s"${item._2 + 1}. ${asMarkdown(item._1)}\n")
        .mkString("")
    case UnorderedList(items @ _*) =>
      items
        .map("- " + asMarkdown(_) + "\n")
        .mkString("")
  }

  implicit def stringToDoc(str: String): Doc = apply(str)

  sealed trait Emphasis

  object Emphasis {
    final case object Regular       extends Emphasis
    final case object Italic        extends Emphasis
    final case object Bold          extends Emphasis
    final case object StrikeThrough extends Emphasis
    final case object InlineCode    extends Emphasis
  }
}
