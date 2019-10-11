package Utils.JSON

import Utils.Difference.{DiffEnum, DifferenceDir}
import better.files.File
import org.json4s.NoTypeHints
import org.json4s.native.{Json, Serialization}
import org.json4s.native.Serialization.{read, write}

import scala.io.Source

case class DifferenceDirs(listDiff: Seq[DifferenceDir])
object Json {
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

}
