package Utils.JSON

import Utils.Difference.DiffEnum
import better.files.File
import org.json4s.JsonAST.{JField, JString}
import org.json4s.NoTypeHints
import org.json4s.native.JsonMethods.{compact, parse, render}
import org.json4s.native.Serialization

object URef {
  implicit val formats = Serialization.formats(NoTypeHints) + new org.json4s.ext.EnumSerializer(DiffEnum)

  def changeCurrentCommit(hash: String, fRef : File): Unit = {
    var jRef = UJson.readDeserializedJson(fRef)
    val currentBranchName: String  = getCurrentBranchName(fRef)

    jRef = jRef transformField{
      case JField(b, JString(s)) if b == currentBranchName => (b, JString(hash))
    }
    UJson.writeJson(jRef, fRef)
  }

  def getHashCurrentCommit(fRef: File) : String = {
    val jRef = UJson.readDeserializedJson(fRef)
    val nameBranch: String = getCurrentBranchName(fRef)

    val hashCommit = jRef transformField{
      case JField(b, JString(s)) if b == nameBranch => return s
    }

    hashCommit.extract[String]
  }

  def getCurrentBranchName(fRef: File) : String = {
    val jRef = UJson.readDeserializedJson(fRef)
    (jRef \ "ref").extract[String]
  }
}
