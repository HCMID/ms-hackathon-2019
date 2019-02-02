import edu.holycross.shot.mid.validator._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.latin._
import scala.io.Source


def readersForString(readerName: String): Vector[MidMarkupReader] = {
  readerName match {
    case "MidProseABReader" =>   MidProseABReader.readers
    case "MidVerseLReader" => MidVerseLReader.readers

    case _ => throw (new Exception(s"${readerName} is not a recognized MidReader in this project."))
  }
}
def orthoForString(readerName: String): MidOrthography = {
  readerName match {
    case "Latin23" => Latin23Alphabet

    case _ => throw (new Exception(s"${readerName} is not a recognized MidReader in this project."))
  }
}



def readerMappings(csvSource : String = "editions/readers.csv") = {
  // drop header row:
  val csvRows = Source.fromFile(csvSource).getLines.toVector.tail
  val pairs = for (row <- csvRows) yield {
    val parts = row.split(",").toVector
    ReadersPairing(CtsUrn(parts(0)), readersForString(parts(1)))
  }
  pairs.toVector
}

def orthoMappings(csvSource : String = "editions/orthographies.csv") = {
  val csvRows = Source.fromFile(csvSource).getLines.toVector.tail
  val pairs = for (row <- csvRows) yield {
    val parts = row.split(",").toVector
    OrthoPairing(CtsUrn(parts(0)), orthoForString(parts(1)))
  }
  pairs.toVector
}


val repo = EditorsRepo(".")
val midValidator = Validator(repo, readerMappings(), orthoMappings())
val reporter = ValidationReporter(midValidator)



def validate(uString : String) = {
  reporter.validate(uString)
}

println("\n\nValidate DSE relations for a given page:")
println("\n\tvalidate(PAGEURN)\n\n")
