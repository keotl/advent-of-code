import day1._
import day2._
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import day3._
import day4._
import day5._
import day6._

@main def main(exercise: String, inputFile: String): Unit = {
  val input = Files.readString(Path.of(inputFile))
  val answer = exercise match {
    case "1a" => day1(input)
    case "1b" => day1b(input)
    case "2a" => day2a(input)
    case "2b" => day2b(input)
    case "3a" => day3a(input)
    case "3b" => day3b(input)
    case "4a" => day4a(input)
    case "4b" => day4b(input)
    case "5a" => day5a(input)
    case "5b" => day5b(input)
    case "6a" => day6a(input)
    case "6b" => day6b(input)
    case _    => println("Unsupported")
  }

  println(s"Day ${exercise}, answer: ${answer}")
}
