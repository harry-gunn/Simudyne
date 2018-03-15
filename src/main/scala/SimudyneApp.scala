
object SimudyneApp extends App {

  val years: Int = 15

  val csvPath = "src/main/resources/Simudyne_Backend_Test.csv"
  val agents = CsvHelper.parseCsv(csvPath)

  val userBrandFactor: Double = scala.util.Try(args.head.toDouble).getOrElse(0.0)
  val finalCalulatedAgents = AgentChanger.calculateYears(years, agents, userBrandFactor)

  val yearlyStats = for {
    yearsPassed <- 0 to years
    yearlyStats = StatsCalculator.calculateAfterYearOutput(finalCalulatedAgents, yearsPassed)
  } yield yearlyStats

  CsvHelper.createOutput(yearlyStats.toList, finalCalulatedAgents.last)


}