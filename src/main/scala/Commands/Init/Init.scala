package Commands.Init

import better.files.File

object Init {
  def init(dir : String): Unit = {
    if (!File(dir + "/.sgit").isEmpty) {
      println("Dépôt SGit existant dans " + dir);
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
      File(dir + "/.sgit/SA.json").createIfNotExists();
      File(dir + "/.sgit/branchs/master/HEAD").createIfNotExists(true, true);
      File(dir + "/.sgit/branchs/master/commits.json").createIfNotExists()
      File(dir + "/.sgit/branch").createIfNotExists(true)
      File(dir + "/config.json")
      File(dir + "/ref.json")
    }
  }
}
