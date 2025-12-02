package assignment2

import com.github.tototoshi.csv.*
import java.io.*

//booking class
case class Booking( bookingId: String,
                    dateOfBooking: String,
                    time: String,
                    customerId: String,
                    gender: String,
                    age: Int,
                    originCountry: String,
                    state: String,
                    location: String,
                    destinationCountry: String,
                    destinationCity: String,
                    noOfPeople: Int,
                    checkInDate: String,
                    noOfDays: Int,
                    checkOutDate: String,
                    rooms: Int,
                    hotelName: String,
                    hotelRating: Double,
                    paymentMode: String,
                    bankName: String,
                    bookingPriceSGD: Double,
                    discount: Double,
                    gst: Double,
                    profitMargin: Double
                  )
//trait for loading bookings (to change data source easily)
trait BookingDataSource:
  def loadBookings(): Seq[Booking]

//csv reader that does the data loading behaviour
class CsvBookingDataSource(filePath: String) extends BookingDataSource:
  private def parsePercentToFraction(s: String): Double =
    val cleaned = s.trim.stripSuffix("%").trim
    if cleaned.isEmpty then 0.0
    else
      try cleaned.toDouble / 100.0
      catch {
        case _: NumberFormatException => 0.0
      }
  def loadBookings(): Seq[Booking] =
    try
      val csvReader = CSVReader.open(new File(filePath))
      try
        val rows: List[Map[String, String]] = csvReader.allWithHeaders()

        val bookings: List[Booking] = rows.map { row =>
          Booking(
            bookingId = row("Booking ID"),
            dateOfBooking = row("Date of Booking"),
            time = row("Time"),
            customerId = row("Customer ID"),
            gender = row("Gender"),
            age = row("Age").toInt,
            originCountry = row("Origin Country"),
            state = row("State"),
            location = row("Location"),
            destinationCountry = row("Destination Country"),
            destinationCity = row("Destination City"),
            noOfPeople = row("No. Of People").toInt,
            checkInDate = row("Check-in date"),
            noOfDays = row("No of Days").toInt,
            checkOutDate = row("Check-Out Date"),
            rooms = row("Rooms").toInt,
            hotelName = row("Hotel Name"),
            hotelRating = row("Hotel Rating").toDouble,
            paymentMode = row("Payment Mode"),
            bankName = row("Bank Name"),
            bookingPriceSGD = row("Booking Price[SGD]").toDouble,
            discount = parsePercentToFraction(row("Discount")),
            gst = row("GST").toDouble,
            profitMargin = row("Profit Margin").toDouble
          )
        }
        bookings
      finally
        csvReader.close()
    catch
      case e: Exception =>
        println("Error reading file: " + e.getMessage)
        Seq.empty[Booking]

//common behaviour of the analysis question
trait AnalysisQuestion[Q]:
  def name: String
  def compute(bookings: Seq[Booking]): Q
  def printResult(result: Q): Unit

//to store the data we need
case class CountryBookingResult(
                               country: String,
                               bookingCount: Int
                               )

case class HotelEconomyResult(
                             hotelName: String,
                             destinationCountry: String,
                             destinationCity: String,
                             averageEconomicalScore: Double
                             )
case class HotelProfitResult(
                            hotelName: String,
                            destinationCountry: String,
                            destinationCity: String,
                            totalHotelProfit: Double,
                            totalHotelVisitors: Int
                            )
//analysis classes
class MostBookedCountryQuestion extends AnalysisQuestion[CountryBookingResult]:
  override val name: String = "Most Booked Country"
  override def compute(bookings: Seq[Booking]): CountryBookingResult =
    val grouped: Map[String, Seq[Booking]] = bookings.groupBy(_.destinationCountry)
    val (country, bookingsForCountry) =
      grouped.maxBy{case(_, bs) => bs.size}
    CountryBookingResult(country, bookingsForCountry.size)
  override def printResult(result: CountryBookingResult): Unit =
    println(s"1. Country with highest number of bookings: ${result.country} with ${result.bookingCount} bookings")

case class MostEconomicalHotelResult(
                                      hotelName: String,
                                      destinationCity: String,
                                      destinationCountry: String
                                    )

class MostEconomicalHotelQuestion extends AnalysisQuestion[MostEconomicalHotelResult]:
  override val name: String = "Most Economical Hotel"

  // convert percentage to double 
  private def parsePercentToFraction(s: String): Double =
    val cleaned = s.trim.stripSuffix("%").trim
    if cleaned.isEmpty then 0.0 else
      try cleaned.toDouble / 100.0 catch {
        case _: NumberFormatException => 0.0
      }

  //filter hotels with lowest profit margin
  override def compute(bookings: Seq[Booking]): MostEconomicalHotelResult =
    val minProfitMargin = bookings.map(_.profitMargin).min
    val filteredByMinProfitMargin =
      bookings.filter(_.profitMargin == minProfitMargin)

    // combine with filter for highest discount, convert double to string before parsing fraction
    val maxDiscount = filteredByMinProfitMargin.map(b => parsePercentToFraction(b.discount.toString)).max
    val filteredByMaxDiscount =
      filteredByMinProfitMargin.filter(
        b => parsePercentToFraction(b.discount.toString) == maxDiscount
      )

    // based on above filters, find lowest booking price
    val minBookingPriceSGD =
      filteredByMaxDiscount.map(_.bookingPriceSGD).min

    // get hotel details based on lowest booking price from prev step
    val bestHotelDetails =
      filteredByMaxDiscount.find(
        _.bookingPriceSGD == minBookingPriceSGD
      ).get

    MostEconomicalHotelResult(
      bestHotelDetails.hotelName,
      bestHotelDetails.destinationCity,
      bestHotelDetails.destinationCountry
    )

  override def printResult(result: MostEconomicalHotelResult): Unit = {
    println(
      f"2. Most Economical Hotel: ${result.hotelName} in " +
        s"${result.destinationCity}, ${result.destinationCountry}"
    )
  }


class MostProfitableHotelQuestion extends AnalysisQuestion[HotelProfitResult]:
  override val name: String = "Most Profitable Hotel"
  override def compute(bookings: Seq[Booking]): HotelProfitResult =
    val groupByHotel: Map[(String, String, String), Seq[Booking]]=
      bookings.groupBy(b => (b.hotelName, b.destinationCountry, b.destinationCity))
    val hotelProfit: Map[(String, String, String), (Double, Int)]=
      groupByHotel.map { case ((hotelName, country, city), bs)=>
        val totalHotelProfit = bs.map {b =>
          val discountedPrice = b.bookingPriceSGD * (1.0 - b.discount)
          val profitPerBooking = discountedPrice * b.profitMargin
          profitPerBooking}.sum
        val totalHotelVisitors = bs.map(_.noOfPeople).sum
        (hotelName, country, city) -> (totalHotelProfit, totalHotelVisitors)
      }
    val ((bestHotelName, bestCountry, bestCity), (bestProfit, bestVisitors))=
      hotelProfit.maxBy {case(_, (totalHotelProfit,_))=> totalHotelProfit}
    HotelProfitResult(bestHotelName, bestCountry, bestCity, bestProfit, bestVisitors)
  override def printResult(result: HotelProfitResult): Unit =
    println(f"3.Most Profitable Hotel: ${result.hotelName}, ${result.destinationCountry}, ${result.destinationCity}" +
      f" with a total profit of ${result.totalHotelProfit}%.2f with ${result.totalHotelVisitors} visitors")

object Main extends App:
  val filePath = "data/Hotel_Dataset.csv"
  val dataSource: BookingDataSource = CsvBookingDataSource(filePath)
  val bookings = dataSource.loadBookings()
  println(s"Loaded ${bookings.size} bookings.")

  val questions = Seq(
    MostBookedCountryQuestion(),
    MostEconomicalHotelQuestion(),
    MostProfitableHotelQuestion(),
  )

  for (q <- questions){
    val result = q.compute(bookings)
    q.printResult(result)
  }