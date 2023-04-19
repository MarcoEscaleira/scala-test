package applications

import java.text.SimpleDateFormat
import lib.picture.Picture
import lib.picture.Picture.*

object MyCalendar {

  /*
  A calendar stores, and displays, events that are scheduled on specific
  days. Each event has a date, a time, and a description.
  */

  case class Date(year: Int, month: Int, day: Int)
  case class Time(hour: Int, minute: Int) {
    def toPicture: Picture = Picture(f"$hour%02d:$minute%02d")
  }
  case class Event(date: Date, time: Time, desc: String)

  /**
   * The columns of the calendar are set to a given width.
   */
  val EventWidth: Int = 14

  /**
   * The place where the events are stored.
   */
  val EventsPath: String = "dat/events.txt"

  /**
   * Returns the number of days in a month. Deals with leap years.
   *
   * @return The number of days in a given month for a given year.
   */
  def daysInMonth(year: Int, month: Int): Int = month match {
    case 1 | 3 | 5 | 7 | 8 | 10 | 12 => 31
    case 4 | 6 | 9 | 11 => 30
    case _ => if (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0) then 29 else 28
  }

  /**
   * The names of the months are stored in a Map relating month numbers (1..) to names.
   * To get the picture representing "JANUARY", write nameOfMonth(1)
   */
  val nameOfMonth: Map[Int, Picture] = Map(
    1 -> Picture("JANUARY"),
    2 -> Picture("FEBRUARY"),
    3 -> Picture("MARCH"),
    4 -> Picture("APRIL"),
    5 -> Picture("MAY"),
    6 -> Picture("JUNE"),
    7 -> Picture("JULY"),
    8 -> Picture("AUGUST"),
    9 -> Picture("SEPTEMBER"),
    10 -> Picture("OCTOBER"),
    11 -> Picture("NOVEMBER"),
    12 -> Picture("DECEMBER")
  )

  /**
   * Days of the week are numbered
   * 0 = Monday
   * 1 = Tuesday
   * 2 = Wednesday
   * 3 = Thursday
   * 4 = Friday
   * 5 = Saturday
   * 6 = Sunday
   *
   * Uses an algorithm published by Tomohiko Sakamoto in 1993
   * See http://www.faqs.org/faqs/sci-math-faq/dayWeek/
   * The algorithm assumes the first day (0) is Sunday. This
   * method adjusts the result so that the first day (0) is
   * Monday.
   *
   * @return the day of the week for a given year/month/day
   *         where 0=Monday, 1=Tuesday,..., 6=Sunday
   */
  def getDayOfWeek(year: Int, month: Int, day: Int): Int = {
    val t = List(0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4)
    val d = day
    val m = month
    val y = if month < 3 then year - 1 else year
    val answerSundayEq0 = (y + y / 4 - y / 100 + y / 400 + t(m - 1) + d) % 7
    val answerMondayEq0 = (answerSundayEq0 + 6) % 7
    answerMondayEq0
  }


  /**
   * The names of the days are stored as a sequence of pictures. To ensure that all the
   * headers in the calendar are the same width, the day names are fixed to the given
   * constant EVENT_WIDTH.
   */
  val namesOfDays: Seq[Picture] =
    Seq("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      .map(Picture(_).fixWidth(EventWidth))

  /**
   * Simple method to read the events from a text file. Absolutely no validation is
   * included. The text file is assumed to be formatted correctly. It reads events
   * by line in the format:
   * year month day hour minute description
   * E.g.
   * 2023 2  22  09  00  CTEC3904 lecture in MS1.02
   *
   * Blank lines are ignored. Multiple spaces are ignored.
   *
   * @param filename The pathname of the file of event data. In IntelliJ this is relative
   *                 to the project root. Thus, "/dat/events.txt" would refer to
   * {{{
   *                 v ProjectName
   *                     > dat
   *                         events.txt
   * }}}
   * @return A sequence of events.
   */
  def readEventsFromFile(filename: String): Seq[Event] =
    import java.io.File
    import scala.io.{BufferedSource, Source}
    val fileHandle: BufferedSource = Source.fromFile(filename)
    val lines: Seq[String] = fileHandle.getLines.toList
    fileHandle.close()
    for
      line <- lines
      if line.nonEmpty
    yield
      val (datetime, description) = line.split("\\s+").toList.splitAt(5)
      val (List(year, month, day), List(hour, minute)) = datetime map (_.toInt) splitAt 3
      Event(Date(year, month, day), Time(hour, minute), description.mkString(" "))

  /**
   * A method to make up random diary events.  Useful for testing.
   * N.B. It over-writes the file at EventsPath
   */
  def makeUpEvents(numberOfEvents: Int, filename: String): Unit = {
    import java.io.File
    import java.io.BufferedWriter
    import java.io.FileWriter
    import scala.io.{BufferedSource, Source}
    val file = new File(filename)
    val fileHandle = new BufferedWriter(new FileWriter(file))
    val r = scala.util.Random
    for (_ <- 1 to numberOfEvents) {
      val yy = r.nextInt(2) + 2023 // Uses dates in 2023 and 2024
      val mm = r.nextInt(12) + 1
      val dd =
        if Set(1, 3, 5, 7, 8, 10, 12) contains mm then
          r.nextInt(31) + 1
        else if Set(4, 6, 9, 11) contains mm then
          r.nextInt(30) + 1
        else if (yy % 4 == 0 && yy % 100 != 0) || (yy % 400 == 0) then
          r.nextInt(29) + 1
        else
          r.nextInt(28) + 1
      val hrs = r.nextInt(14)
      val mins = if r.nextInt(2) == 0 then 0 else 30
      val desc = f"Event-auto-gen ($yy/$mm/$dd @ $hrs%02d:$mins%02d)"
      fileHandle.write(f"$yy%4d$mm%4d$dd%4d  $hrs%02d $mins%02d $desc\n")
    }
    fileHandle.close()
  }

  /** THE COURSEWORK METHOD
   * **********************************************************************************
   * Produces a picture (ready for display on the output console) of the given month in
   * the calendar.  The events are filtered so that only those relevant for the given
   * month are displayed.
   ************************************************************************************
   * @param year   The calendar year. (e.g. 2023)
   * @param month  The calendar month (1 = JANUARY, etc.)
   * @param events The sequence of (all) events. It will need to be filtered to obtain
   *               those events relevant for this month and this year.
   * @return A nicely formatted calendar page that contains a view of the given
   *         month in the given year.
   */
  private case class Slot(day: Int, week: Int)
  private case class EventSlot(slot: Slot, picture: Picture)

  private val inputFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.S")
  private val outputFormatDefault = new SimpleDateFormat("ww")

  private val SLOT_WIDTH: Int = 10

  private def parseLine(event: Event): EventSlot = {
    val day: Int = getDayOfWeek(event.date.year, event.date.month, event.date.day)
    val weekOfYear: Int = outputFormatDefault.format(inputFormat.parse(s"${event.date.year}-${event.date.month}-${event.date.day} 00:00:00.0")).toInt
    val xs = event.desc.drop(5)
    EventSlot(Slot(day, weekOfYear), Picture(xs, ' ').fixWidth(SLOT_WIDTH))
  }

  private def also(p1: Picture, p2: Picture): Picture = p1 ^ Picture("ALSO").borderT(' ').borderB(' ').fixWidth(SLOT_WIDTH) ^ p2

  private val firstWeekDay: Int = 0 // Monday
  private val lastWeekDay: Int = 6 // Sunday

  def displayMonth(year: Int, month: Int, events: Seq[Event]): Picture = {
    val days: List[Int] = (firstWeekDay to lastWeekDay).toList

    val daysInThisMonth = daysInMonth(year, month)
    val firstWeekOfMonthYear = outputFormatDefault.format(inputFormat.parse(s"$year-$month-01 00:00:00.0")).toInt;
    val lastWeekMonthYear = outputFormatDefault.format(inputFormat.parse(s"$year-$month-$daysInThisMonth 00:00:00.0")).toInt

    val weeks: List[Int] = (firstWeekOfMonthYear to lastWeekMonthYear).toList
    val weekLabels = "Week " :: (weeks map (t => s"$t"))
    val margin = weekLabels map (Picture(_) fixWidth (4))

    //first filter events, then sort events using group map, then use picture to events above each other
    val filteredEvents: Seq[Event] = events.filter(e=>e.date.month == month && e.date.year == year)

    val eventsParsed: Seq[EventSlot] = filteredEvents map parseLine

    val groupedEvents: Map[Slot, Seq[EventSlot]] = eventsParsed.groupBy(_.slot)

    val groupedEventsPictures: Map[Slot, Seq[Picture]] =
      groupedEvents map ((slot, evs) => (slot, evs map (_.picture)))

    val groupedAndJoinedActivityPictures: Map[Slot, Picture] =
      groupedEventsPictures map ((slot, evs) => (slot, evs reduceLeft also))

    val tableData: Seq[Seq[Picture]] =
      for week <- weeks
        yield
          for day <- days
            yield
              groupedAndJoinedActivityPictures.getOrElse(Slot(day, week), Picture(' '))

    val timetable: Seq[Seq[Picture]] = margin zip (namesOfDays +: tableData) map (_ +: _)

    val monthName: Picture = nameOfMonth.getOrElse(month, Picture(""))
    val buildBody = Picture(s"$monthName $year") ^ timetable.formatAsTable()
    Picture(s"$buildBody")
  }

  /**
   * A method to create a set of random events ane write them to the
   * default text file, EventsPath.  To change the number of events
   * simply adjust the number in the parameter list.
   */
  @main def constructRandomEventFile(): Unit =
    makeUpEvents(1000, EventsPath)

  @main def coursework(): Unit =
    /* Read the events from an external file... */
    val events = readEventsFromFile(EventsPath)

    /* Or create the data structure by hand... */
    // val es = Seq(
    // Event(Date(2023, 2, 22), Time(9, 0), "CTEC3904 lecture in MS1.02")
    //   //etc.
    // )
    /* Choose a year/month to display from a sequence of events... */
    println(displayMonth(2023, 4, events))
}
