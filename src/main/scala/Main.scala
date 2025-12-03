package assignment2

import com.github.tototoshi.csv.*
import scalafx.beans.value

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
        }.distinctBy(_.bookingId)
        println(s"Removed duplicates: ${rows.size - bookings.size}")
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
                             averageRankingScore: Double
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
    val grouped: Map[String, Seq[Booking]] = bookings.groupBy(_.originCountry)
    val (country, bookingsForCountry) =
      grouped.maxBy{case(_, bs) => bs.size}
    CountryBookingResult(country, bookingsForCountry.size)
  override def printResult(result: CountryBookingResult): Unit =
    println(s"1. Country with highest number of bookings: ${result.country} with ${result.bookingCount} bookings")

class MostEconomicalHotelQuestion extends AnalysisQuestion[HotelEconomyResult]:
  override val name: String = "Most Economical Hotel"
  private case class HotelStats(name: String, country: String, city: String, avgP: Double, avgD: Double, avgR: Double)
  private def minMax(value: Double, min: Double, max: Double): Double =
    if max == min then 1.0 else (value - min) / (max - min)
  private def invert(x: Double): Double = 1.0 - x
  override def compute(bookings: Seq[Booking]): HotelEconomyResult =
    val grouped = bookings.groupMap(b => (b.hotelName, b.destinationCountry, b.destinationCity)) {b => (b.bookingPriceSGD / b.noOfDays.toDouble / b.rooms.toDouble, b.discount, b.profitMargin)}
    val statsList: Seq[HotelStats] = grouped.toSeq.map { case ((name, country, city), values) =>
      val n = values.size
      val (sumP, sumD, sumR) =
        values.foldLeft((0.0, 0.0, 0.0)) { case ((sp, sd, sr), (p, d, r)) => (sp + p, sd + d, sr + r) }
      HotelStats(name, country, city, sumP / n, sumD / n, sumR / n)
    }
    if statsList.isEmpty then
      return HotelEconomyResult("None", "None", "None", Double.PositiveInfinity)

    val (minP, maxP, minD, maxD, minR, maxR) =
      statsList.foldLeft((Double.PositiveInfinity, Double.NegativeInfinity, Double.PositiveInfinity, Double.NegativeInfinity, Double.PositiveInfinity, Double.NegativeInfinity)) {
        case ((mnP, mxP, mnD, mxD, mnR, mxR), s) =>
          (math.min(mnP, s.avgP), math.max(mxP, s.avgP),
            math.min(mnD, s.avgD), math.max(mxD, s.avgD),
            math.min(mnR, s.avgR), math.max(mxR, s.avgR))
      }

    val scored: Seq[(HotelStats, Double)] = statsList.map { s =>
      val pScore = minMax(s.avgP, minP, maxP)
      val dScore = invert(minMax(s.avgD, minD, maxD))
      val rScore = minMax(s.avgR, minR, maxR)
      val finalScore = (pScore + dScore + rScore) / 3.0
      (s, finalScore)
    }

    val (bestStat, bestScore) = scored.minBy(_._2)
    HotelEconomyResult(bestStat.name, bestStat.country, bestStat.city, bestScore)
  override def printResult(result: HotelEconomyResult): Unit =
    println(f"2. Most Economical Hotel: ${result.hotelName}, ${result.destinationCountry}, ${result.destinationCity} with average ranking score of ${result.averageRankingScore}%.2f score")

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
    println(f"3. Most Profitable Hotel: ${result.hotelName}, ${result.destinationCountry}, ${result.destinationCity}" +
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


