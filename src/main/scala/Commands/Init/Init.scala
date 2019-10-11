package Commands.Init

import better.files.File

object Init {

  /* Init a sgit repo and print log */
  def initRepo(dir: File): Unit = {
    if (Init.init(dir)) println("Initialized empty Git repository in " + dir.path)
    else println("Reinitialized existing Git repository in " + dir.path )
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
      File(dir + "/.sgit/branchs/master/HEAD").createIfNotExists(true, true);
      File(dir + "/.sgit/branchs/master/commits.json").createIfNotExists()
      File(dir + "/.sgit/branch").createIfNotExists(true)
      File(dir + "/config.json")
      File(dir + "/ref.json")
      true
    }
  }
}
