package Utils.JSON

import Utils.Difference.{DiffEnum, DifferenceDir}
import better.files.File
import org.json4s.JsonAST.JValue
import org.json4s.NoTypeHints
import org.json4s.native.{Json, Serialization}
import org.json4s.native.Serialization.{read, write}
import org.json4s.native.JsonMethods.{parse, render, compact}
import scala.io.Source

case class DifferenceDirs(listDiff: Seq[DifferenceDir])
object UJson {
  implicit val formats = Serialization.formats(NoTypeHints) + new org.json4s.ext.EnumSerializer(DiffEnum)

  def writeSerializeSeqDifferenceDir(seqDiffs: Seq[DifferenceDir], file: File): Unit = {
    file.write(serializeSeqDifferenceDir(seqDiffs))
  }

  def readDeserializeSeqDifferenceDir(file: File): Seq[DifferenceDir] = {
    val ser = Source.fromFile(file.pathAsString).getLines.mkString
    deserializeSeqDifferenceDir(ser)
  }

  def serializeSeqDifferenceDir(seqDiffs: Seq[DifferenceDir]): String = {
    write(DifferenceDirs(seqDiffs))
  }

  def deserializeSeqDifferenceDir(seqDiffs: String): Seq[DifferenceDir] = {
    read[DifferenceDirs](seqDiffs).listDiff
  }

  def writeJson(json: JValue, file: File): Unit = {
    file.write(compact(render(json)))
  }

  def writeSerializedJson(json: String, file: File) : Unit = {
    file.write(json)
  }

  def readDeserializedJson(file: File) : JValue = {
    val ser = Source.fromFile(file.pathAsString).getLines.mkString
    parse(ser)
  }
}
