package zio.web.docs

import zio.test.Assertion._
import zio.test._
import zio.web.docs.Doc.Text.{ bold, italic, strikethrough }

object DocTest extends DefaultRunnableSpec {
  import Doc._

  override def spec: ZSpec[_root_.zio.test.environment.TestEnvironment, Any] =
    /**
     * - Markdown spec https://spec.commonmark.org/0.29/#preliminariesz
     * - Markdown examples taken from https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet
     */
    suite("asMarkdown(doc)")(
      test("Headings") {
        val actual =
          h1("H1") <>
            h2("H2") <>
            h3("H3") <>
            h4("H4") <>
            h5("H5") <>
            h6("H6")

        val expected =
          """# H1
	          |## H2
	          |### H3
	          |#### H4
	          |##### H5
	          |###### H6
		        |""".stripMargin

        assert(asMarkdown(actual))(equalTo(expected))
      },
      test("Emphasis") {
        val actual =
          //                      👇 Having to specify the space ourselves is not nice, this should be dealt with when asMarkdown starts using a context
          p("Emphasis, aka " <> italic("italics") <> ".") <>
            p("Strong emphasis, aka " <> bold("bold") <> ".") <>
            p("And " <> strikethrough("Strikethrough")) <>
            p(
              "Combined emphasis with " <> strikethrough("sriketrhough " <> bold("bold and " <> italic("italic"))) <> "."
            )

        val expected =
          """Emphasis, aka *italics*.
	          |
	          |Strong emphasis, aka __bold__.
	          |
	          |And ~~Strikethrough~~
	          |
	          |Combined emphasis with ~~sriketrhough __bold and *italic*__~~.
						|
						|""".stripMargin

        assert(asMarkdown(actual))(equalTo(expected))
      }
    )
}
