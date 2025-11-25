package assignment2

import org.scalatest.funsuite.AnyFunSuite
import com.github.tototoshi.csv._
import java.io._

class MainTest extends AnyFunSuite {
  test("Reading a CSV file should not throw an exception") {
    val filePath = "data/Hotel_Dataset.csv"
    try {
      val csvReader = CSVReader.open(new File(filePath))
      val lines: List[Map[String, String]] = csvReader.all()
      assert(!lines.isEmpty)
      csvReader.close()
    } catch {
      case e: Exception => fail("An error occurred while reading the file: " + e.getMessage)
    }
  }
}
