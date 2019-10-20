package Utils.JSON

import Utils.Difference.{DiffEnum, DifferenceDir}
import better.files.File
import org.json4s.JsonAST.JValue
import org.json4s.NoTypeHints
import org.json4s.native.{Json, Serialization}
import org.json4s.native.Serialization.{read, write}
import org.json4s.native.JsonMethods.{compact, parse, render}

import scala.io.Source

case class DifferenceDirs(listDiff: Seq[DifferenceDir])
object UJson {
  implicit val formats = Serialization.formats(NoTypeHints) + new org.json4s.ext.EnumSerializer(DiffEnum)

  // ALL
  def serializeT[T: Manifest](json: T): String = {
    write(json)
  }

  def deserializeT[T: Manifest](obj: String): T = {
    read[T](obj)
  }

  def readJson(file: File) : JValue = {
    val ser = Source.fromFile(file.pathAsString).getLines.mkString
    parse(ser)
  }

  def writeSerializedJson(json: String, file: File) : Unit = {
    file.write(json)
  }

  def writeJson[T: Manifest](json: T, file: File) : Unit = {
    file.write(serializeT[T](json))
  }

  def readDeserializedJson[T: Manifest](file: File) : T = {
    val ser = Source.fromFile(file.pathAsString).getLines.mkString
    deserializeT[T](ser)
  }
}
