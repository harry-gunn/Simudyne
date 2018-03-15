import org.scalatest.{FlatSpec, Matchers}

class StatsCalculatorSpec extends FlatSpec with Matchers {

  behavior of "calculateAfterYearOutput"

  val longerListOfAgentsYear1 = List(
    Agent("Breed_C", "1.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_C", "2.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_C", "3.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_C", "4.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_C", "5.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "6.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "7.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "8.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "9.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "10.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "11.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "12.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "13.0", 1, 1, 1, 1, 1, 1, 0, 1, false)
  )
  val longerListOfAgentsYear2 = List(
    Agent("Breed_C", "1.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_C", "2.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "3.0", 1, 1, 1, 1, 1, 1, 0, 1, true),
    Agent("Breed_NC", "4.0", 1, 1, 1, 1, 1, 1, 0, 1, true),
    Agent("Breed_NC", "5.0", 1, 1, 1, 1, 1, 1, 0, 1, true),
    Agent("Breed_NC", "6.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "7.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "8.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "9.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_C", "10.0", 1, 1, 1, 1, 1, 1, 0, 1, true),
    Agent("Breed_C", "11.0", 1, 1, 1, 1, 1, 1, 0, 1, true),
    Agent("Breed_C", "12.0", 1, 1, 1, 1, 1, 1, 0, 1, true),
    Agent("Breed_C", "13.0", 1, 1, 1, 1, 1, 1, 0, 1, true)
  )
  val longerListOfAgentsYear3 = List(
    Agent("Breed_NC", "1.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "2.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "3.0", 1, 1, 1, 1, 1, 1, 0, 1, true),
    Agent("Breed_C", "4.0", 1, 1, 1, 1, 1, 1, 0, 1, true),
    Agent("Breed_C", "5.0", 1, 1, 1, 1, 1, 1, 0, 1, true),
    Agent("Breed_NC", "6.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "7.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "8.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "9.0", 1, 1, 1, 1, 1, 1, 0, 1, false),
    Agent("Breed_NC", "10.0", 1, 1, 1, 1, 1, 1, 0, 1, true),
    Agent("Breed_NC", "11.0", 1, 1, 1, 1, 1, 1, 0, 1, true),
    Agent("Breed_NC", "12.0", 1, 1, 1, 1, 1, 1, 0, 1, true),
    Agent("Breed_NC", "13.0", 1, 1, 1, 1, 1, 1, 0, 1, true)
  )

  val allYearsOfUpdatedAgents = List(longerListOfAgentsYear1, longerListOfAgentsYear2, longerListOfAgentsYear3)

  it should "Calculate the number of Breed_C agents" in {
    StatsCalculator.calculateAfterYearOutput(allYearsOfUpdatedAgents, 0).numberOfBreedC shouldBe 5
  }

  it should "Calculate the number of Breed_NC agents" in {
    StatsCalculator.calculateAfterYearOutput(allYearsOfUpdatedAgents, 0).numberOfBreedNC shouldBe 8
  }

  it should "Calculate the number of Breed_C lost" in {
    StatsCalculator.calculateAfterYearOutput(allYearsOfUpdatedAgents, 1).breedCLost shouldBe 3

  }

  it should "Calculate the number of Breed_C gained" in {
    StatsCalculator.calculateAfterYearOutput(allYearsOfUpdatedAgents, 1).breedCGained shouldBe 4
  }

  it should
    """Calculate the number of original Breed_C agents that changed
      |to Breed_NC but have now changed back to Breed_C""".stripMargin in {
    StatsCalculator.calculateAfterYearOutput(allYearsOfUpdatedAgents, 2).breedCRegained shouldBe 2
  }
}
