package Commands.Init

import Commands.Commit.Commit
import Utils.JSON.UJson
import better.files.File

object Init {

  /* Init a sgit repo and print log */
  def initRepo(dir: File): Unit = {
    if (Init.init(dir)) println("Initialized empty Sgit repository in " + dir.path)
    else println("Reinitialized existing Sgit repository in " + dir.path )
  }

  /*
  Return true if .sgit was created correctly
   */
  def init(dir : File): Boolean = {
    if (dir.children.contains(File(dir.path + "/.sgit"))) {
      false
    } else {
      /*
    Archi :
      .sgit
       |-SA.json // Stage Area
       |-branchs
       |-|-master
       |-|-|-commits.json
       |-|-|-HEAD // all files in root directorie
       |-branch // branch act
       |-|-commits.json
       |-|-HEAD // all files of last commit on the act branch
       |-config  // config file with some data
       |-ref // file follow the head
     */
      File(dir + "/.sgit").createIfNotExists(true)
      File(dir + "/.sgit/SA").createIfNotExists(true)
      File(dir + "/.sgit/commits")createIfNotExists (true)
      File(dir + "/.sgit/head").createIfNotExists(true)
      File(dir + "/config.json")
      val fileRef = File(dir + "/.sgit/ref.json").createIfNotExists()

      // Initialize on master branch
      fileRef.write(""" {"ref": "master", "branchs":{"master": ""}} """)

      true
    }
  }
}
