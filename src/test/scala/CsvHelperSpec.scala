import org.scalatest.{FlatSpec, Matchers}

class CsvHelperSpec extends FlatSpec with Matchers {

  val pathToCorrectCsv = "src/test/resources/Test_Csv_Data.csv"

  val baseTestAgent = List(Agent(
    "test_agent_breed",
    "132802008.0",
    10,
    22,
    33,
    44,
    55,
    66,
    0,
    77,
    false
  ))

  behavior of "makeAgent"

  it should "create a List of agents when given valid arguments in csv" in {

    CsvHelper.parseCsv(pathToCorrectCsv) shouldBe baseTestAgent
  }

  it should "return empty List for too short entries" in {
    CsvHelper.parseCsv("src/test/resources/Bad_Test_Csv_Data-Short.csv") shouldBe List()
  }

  it should "return empty List for too long entries" in {
    CsvHelper.parseCsv("src/test/resources/Bad_Test_Csv_Data-Long.csv") shouldBe List()
  }

}
