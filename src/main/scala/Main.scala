import Commands.Add
import Commands.Commit.Commit
import Commands.Init.Init
import better.files.File

import scala.language.postfixOps

object Main extends App {

  val dirAct = File(System.getProperty("user.dir"))

  // Head of args is the command
  if (args.isEmpty) {
    println ("See usage.")
  } else {
    val command = args.head



    command match {
      case "init" => Init.initRepo(dirAct)
      case "add" => Add.add(args.tail, dirAct)
      case "commit" => Commit.commit(args.tail, dirAct)
      case _ => println("git: '" + command + "' is not a git command.")
    }
  }
}



