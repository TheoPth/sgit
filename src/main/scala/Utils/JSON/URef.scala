package Utils.JSON

import Utils.Difference.DiffEnum
import Utils.MoveDir.Sdir
import Utils.archiSgit.{OCommit, UCommit}
import better.files.File
import org.json4s.NoTypeHints
import org.json4s.native.Serialization

case class ORef(ref: String, branchs: Seq[OField], tags: Seq[OField])
case class OField(name: String, ref: String)

object URef {
  implicit val formats = Serialization.formats(NoTypeHints) + new org.json4s.ext.EnumSerializer(DiffEnum)

  def changeCurrentCommit(hash: String, ref : ORef): ORef = {
    val currentBranchName: String  = ref.ref
    val nref = ref.copy(branchs = ref.branchs.map(branch => if (branch.name == currentBranchName) OField(branch.name, hash) else branch))
    nref
  }

  def getHashCurrentCommit(ref: ORef) : String = {
    val nameBranch = getCurrentBranchName(ref)
    ref.branchs.filter(branch => branch.name == nameBranch).map(branch => branch.ref)(0)
  }

  def getCurrentBranchName(fRef: ORef) : String = {
    fRef.ref
  }

  def getBranchs(ref: ORef): Seq[OField] = {
    ref.branchs
  }

  def getTags(ref: ORef): Seq[OField] = {
    ref.tags
  }

  def addBranch(nameBranch: String, ref: ORef): ORef = {
    val curHash = getHashCurrentCommit(ref)
    ref.copy(branchs = ref.branchs :+ OField(nameBranch, curHash))
  }

  def addTag(nameBranch: String, ref: ORef): ORef = {
    val curHash = getHashCurrentCommit(ref)
    ref.copy(tags = ref.tags :+ OField(nameBranch, curHash))
  }

  def initRef(): ORef = {
    ORef("master", Seq(OField("master", "")), Seq())
  }
}
