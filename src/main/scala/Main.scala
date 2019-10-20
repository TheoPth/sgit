import Commands.{Add, Branch, Checkout, Commit, Diff, Init, Log, Status, Tag}
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
      case "status" => Status.status(dirAct)
      case "diff" => Diff.diff(dirAct)
      case "log" => Log.log(args.tail, dirAct)
      case "branch" => Branch.branch(args.tail, dirAct)
      case "checkout" => Checkout.checkout(args.tail, dirAct)
      case "tag" => Tag.tag(args.tail, dirAct)
      case _ => println("sgit: '" + command + "' is not a sgit command.")
    }
  }
}



