package main

import ru.tinkoff.backendacademy.wordstorage.backend.WordRepository

import scala.io.StdIn.readLine

class WordStorageConsoleFrontend(backend: WordRepository) {

  private val commandAndWord = "^(get|put|delete)\\s+(.*)$".r
  def myApp =
    ZIO.whenCaseZIO {
      (Console.print(
        "Please choose one game (minesweeper, snake, tictactoe)? "
      ) *> Console.readLine).orDie
    } {
      case "minesweeper" =>
        Console.print(
          "Please enter the level of the game (easy/hard/medium)?"
        ) *> Console.readLine.flatMap(minesweeper)
      case "snake" =>
        Console.printLine(
          "Please enter the size of the game: "
        ) *> Console.readLine.mapAttempt(_.toInt).flatMap(n => snake(n, n))
      case "tictactoe" => ticTacToe
    }
}

object Main extends App {
  val a = new WordStorageConsoleFrontend(new WordRepository)
  a.myApp
}
  //  def start(): Unit =
  //    while (true) {
  //      val str = readLine()
  //      str match {
  //        case commandAndWord("get", word)    => println(backend.get(word))
  //        case commandAndWord("put", word)    => backend.put(word)
  //        case commandAndWord("delete", word) => backend.delete(word)
  //        case "quit"                         => return
  //        case _ =>
  //          println(
  //            "Could not recognize command, please use one of " +
  //              """"get", "put" or "delete" and then write your word, or write "quit""""
  //          )
  //      }
  //    }
