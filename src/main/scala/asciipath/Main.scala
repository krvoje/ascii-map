package asciipath

object Main extends App {

  val asciiMapValidator = new AsciiMapValidator
  val asciiMapService   = new AsciiMapService(asciiMapValidator)

  if (args.isEmpty) {
    println(
      s"""
         |ASCII Map traversal algorithm. Usage
         |java -jar ascii-map.jar filename.txt
         |""".stripMargin)

  } else {
    val filename = args(0)
    val source   = scala.io.Source.fromFile(filename)
    val asciiMap = asciiMapService.parse(source.mkString(""))
    asciiMapService.traverse(asciiMap).fold({ error =>
      println(s"Error parsing the map: $error")
    }, { result =>
      println("ASCII Map:")
      println(asciiMap)
      println(s"Letters: ${result.letters}")
      println(s"Path: ${result.path}")
    })

    source.close()
  }
}
