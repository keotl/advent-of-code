import day1._
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path

@main def main(exercise: String, inputFile: String): Unit = {
  val input = Files.readString(Path.of(inputFile))
  val answer = exercise match {
    case "1a" => day1(input)
    case "1b" => day1b(input)
    case _ => println("Unsupported")
  }

  println(s"Day ${exercise}, answer: ${answer}")
}
