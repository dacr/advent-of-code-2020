package helpers

import better.files.File
import scala.io.Source

trait Helpers {

  def fileContent(filename:String):Iterable[String] = {
    Source.fromFile(filename).getLines.to(Iterable).map(_.trim).filter(_.size>0)
  }

  def resourceContent(filename:String):Iterable[String] = {
    Source.fromResource(filename).getLines.to(Iterable).map(_.trim).filter(_.size>0)
  }

  def stringContent(content: String):Iterable[String] = {
    content.split("\n").toList.map(_.trim).filter(_.size>0)
  }

}
