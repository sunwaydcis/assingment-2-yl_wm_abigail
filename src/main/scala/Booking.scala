package assignment2

case class Booking(
                    bookingId: String,
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
