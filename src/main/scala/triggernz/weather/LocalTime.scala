package triggernz.weather

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneId, ZonedDateTime}

object LocalTime {
  val StartYear = 2019

  // Computes local time by latitude only, ignoring actual (political) timezones
  def computeLocalTime(hours: Hours, longitude: Degrees): ZonedDateTime = {
    val tzHour = (longitude / Degrees(15)).toInt
    val tzString =
      if (tzHour > 0)
        "GMT+" + tzHour.toInt
      else if (tzHour < 0)
        "GMT" + tzHour.toInt
      else
        "GMT"

    val tz = ZoneId.of(tzString)

    val days = hours.value.toInt / 24
    val remainingHours = hours.value.toInt - 24 * days

    LocalDateTime.of(StartYear, 1, 1, 0, 0)
      .plusDays(days)
      .plusHours(remainingHours)
      .atZone(tz)
  }

  val Format = DateTimeFormatter.ofPattern("YYYY-MM-dd'T'hh:mm:ss")
}
