package assignment2

import com.github.tototoshi.csv._
import java.io._

object Main extends App {
  val filePath = "data/Hotel_Dataset.csv"

  // Use the CSV reader to read the file
  try {
    val csvReader = CSVReader.open(new File(filePath))
    val lines: List[List[String]] = csvReader.all() // This returns all rows as lists of strings

    // Manually convert the list of lists into a list of maps
    val bookingLines: List[Map[String, String]] = lines.map { line =>
      Map(
        "Booking ID" -> line(0),
        "Date of Booking" -> line(1),
        "Time" -> line(2),
        "Customer ID" -> line(3),
        "Gender" -> line(4),
        "Age" -> line(5).toInt.toString,
        "Origin Country" -> line(6),
        "State" -> line(7),
        "Location" -> line(8),
        "Destination Country" -> line(9),
        "Destination City" -> line(10),
        "No. Of People" -> line(11).toInt.toString,
        "Check-in date" -> line(12),
        "No of Days" -> line(13).toInt.toString,
        "Check-Out Date" -> line(14),
        "Rooms" -> line(15).toInt.toString,
        "Hotel Name" -> line(16),
        "Hotel Rating" -> line(17).toDouble.toString,
        "Payment Mode" -> line(20),
        "Bank Name" -> line(18),
        "Booking Price[SGD]" -> line(19).toDouble.toString,
        "Discount" -> 0.0.toString, // Assuming no discount by default until specified otherwise
        "GST" -> 0.0.toString,      // Assuming GST is not provided in the dataset initially
        "Profit Margin" -> 0.0.toString // Similarly assuming no profit margin initially
      )
    }

    for (line <- bookingLines) {
      println(assignment2.Booking(
        bookingId = line("Booking ID"),
        dateOfBooking = line("Date of Booking"),
        time = line("Time"),
        customerId = line("Customer ID"),
        gender = line("Gender"),
        age = line("Age").toInt,
        originCountry = line("Origin Country"),
        state = line("State"),
        location = line("Location"),
        destinationCountry = line("Destination Country"),
        destinationCity = line("Destination City"),
        noOfPeople = line("No. Of People").toInt,
        checkInDate = line("Check-in date"),
        noOfDays = line("No of Days").toInt,
        checkOutDate = line("Check-Out Date"),
        rooms = line("Rooms").toInt,
        hotelName = line("Hotel Name"),
        hotelRating = line("Hotel Rating").toDouble,
        paymentMode = line("Payment Mode"),
        bankName = line("Bank Name"),
        bookingPriceSGD = line("Booking Price[SGD]").toDouble,
        discount = line("Discount").toDouble,
        gst = line("GST").toDouble,
        profitMargin = line("Profit Margin").toDouble
      ))
    }

    csvReader.close()
  } catch {
    case e: Exception => println("Error reading file: " + e.getMessage)
  }
}
