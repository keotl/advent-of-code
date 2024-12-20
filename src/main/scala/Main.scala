import day1._
import day2._
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import day3._
import day4._
import day5._
import day6._
import day7._
import day8._
import day9._
import day10._
import day11._

@main def main(exercise: String, inputFile: String): Unit = {
  val input = Files.readString(Path.of(inputFile))
  val answer = exercise match {
    case "1a"  => day1(input)
    case "1b"  => day1b(input)
    case "2a"  => day2a(input)
    case "2b"  => day2b(input)
    case "3a"  => day3a(input)
    case "3b"  => day3b(input)
    case "4a"  => day4a(input)
    case "4b"  => day4b(input)
    case "5a"  => day5a(input)
    case "5b"  => day5b(input)
    case "6a"  => day6a(input)
    case "6b"  => day6b(input)
    case "7a"  => day7a(input)
    case "7b"  => day7b(input)
    case "8a"  => day8a(input)
    case "8b"  => day8b(input)
    case "9a"  => day9a(input)
    case "9b"  => day9b(input)
    case "10a" => day10a(input)
    case "10b" => day10b(input)
    case "11a" => day11a(input)
    case "11b" => day11b(input)
    case _     => println("Unsupported")
  }

  println(s"Day ${exercise}, answer: ${answer}")
}
