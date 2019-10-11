import Commands.Add
import Commands.Init.Init
import Utils.Difference.{DiffEnum, DifferenceDir}
import better.files.File



object Main extends App {

  // Head of args is the command
  val command = args.head

  val dirAct = File(System.getProperty("user.dir"))
  command match {
    case "init" => Init.initRepo(dirAct)
    case "add" => Add.add(args.tail, dirAct)
    case _ => println("git: '" +  command + "' is not a git command.")
  }
}



