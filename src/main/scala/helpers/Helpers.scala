package helpers

import better.files.File
import scala.io.Source

trait Helpers {

  def fileContentLines(filename: String): Iterable[String] = {
    Source.fromFile(filename).getLines.to(Iterable)
  }

  def fileContent(filename: String): String = {
    Source.fromFile(filename).getLines.to(Iterable).mkString("\n")
  }

  def resourceContentLines(filename: String): Iterable[String] = {
    Source.fromResource(filename).getLines.to(Iterable)
  }

  def resourceContent(filename: String): String = {
    Source.fromResource(filename).getLines.to(Iterable).mkString("\n")
  }

  def stringContentLines(content: String): Iterable[String] = {
    content.split("\n").toList
  }

}
