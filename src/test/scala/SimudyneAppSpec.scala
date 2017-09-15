import org.scalatest.{FlatSpec, Matchers}

class SimudyneAppSpec extends FlatSpec with Matchers {

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
    77
  ))

  behavior of "makeAgent"

  it should "create a List of agents when given valid arguments in csv" in {

    SimudyneApp.parseCsv(pathToCorrectCsv) shouldBe baseTestAgent
  }

  it should "return empty List for too short entries" in {
    SimudyneApp.parseCsv("src/test/resources/Bad_Test_Csv_Data-Short.csv") shouldBe List()
  }

  it should "return empty List for too long entries" in {
    SimudyneApp.parseCsv("src/test/resources/Bad_Test_Csv_Data-Long.csv") shouldBe List()
  }


  behavior of "passOneYear"

  it should "increase the age of the agent by 1" in {
    val agentWithZeroAge = baseTestAgent.map(agent => agent.copy(age = 0))
    SimudyneApp.increaseAge(baseTestAgent).head.age shouldBe 11
    SimudyneApp.increaseAge(agentWithZeroAge).head.age shouldBe 1
  }

  behavior of "filterOutAutoRenew"

  it should "return empty list when the auto_renew is 1" in {
    val nonAutoRenewAgent = baseTestAgent.map(agent => agent.copy(auto_renew = 1))

    SimudyneApp.filterOutAutoRenew(nonAutoRenewAgent) shouldBe List()
  }

  it should "not filter out an agent when the auto renew is 0" in {
    SimudyneApp.filterOutAutoRenew(baseTestAgent) shouldBe baseTestAgent
  }


  behavior of "calculateBreedChanges"

  val lowAffinityAgent = List(Agent(
    "Breed_C",
    "132802008.0",
    1,
    1,
    1,
    10000,
    1,
    1,
    0,
    1
  ))

  val highAffinityAgent = List(Agent(
    "Breed_C",
    "132802008.0",
    1,
    1,
    1,
    0.00001,
    1,
    1,
    0,
    1
  ))

  it should s"Return a Breed_NC Agent when given a Breed_C and high values" in {
    SimudyneApp.calculateBreedChanges(lowAffinityAgent, 2).head.agent_breed shouldBe "Breed_NC"
  }

  it should s"Return a Breed_C Agent when given a Breed_NC and high values" in {
    val lowAffinityNCAgent = List(lowAffinityAgent.head.copy(agent_breed = "Breed_NC"))
    SimudyneApp.calculateBreedChanges(lowAffinityNCAgent, 2).head.agent_breed shouldBe "Breed_C"
  }

  it should s"Return a Breed_C Agent when given a Breed_C and low values" in {
    SimudyneApp.calculateBreedChanges(highAffinityAgent, 0.1).head.agent_breed shouldBe "Breed_C"
  }

  it should s"Return a Breed_NC Agent when given a Breed_NC and low values" in {
    val highAffinityNCAgent = List(highAffinityAgent.head.copy(agent_breed = "Breed_NC"))
    SimudyneApp.calculateBreedChanges(highAffinityNCAgent, 0.1).head.agent_breed shouldBe "Breed_NC"
  }

  it should
    """Calculate the correct breed given different agent values -
      | this is repeated due to the random element of the calculation""".stripMargin in {

    for (x <- 1 to 1000) {
      SimudyneApp.calculateBreedChanges(lowAffinityAgent, 2).head.agent_breed shouldBe "Breed_NC"

      val lowAffinityNCAgent = List(lowAffinityAgent.head.copy(agent_breed = "Breed_NC"))
      SimudyneApp.calculateBreedChanges(lowAffinityNCAgent, 0.1).head.agent_breed shouldBe "Breed_C"

      SimudyneApp.calculateBreedChanges(highAffinityAgent, 2).head.agent_breed shouldBe "Breed_C"

      val highAffinityNCAgent = List(highAffinityAgent.head.copy(agent_breed = "Breed_NC"))
      SimudyneApp.calculateBreedChanges(highAffinityNCAgent, 0.1).head.agent_breed shouldBe "Breed_NC"
    }
  }

  behavior of "calculate15Years"

  it should "return a List[List[Agent]], each List[Agent] for a different year" in {
    SimudyneApp.calculate15Years(baseTestAgent, 2).length shouldBe 16
  }

  behavior of "calculateAfterYearOutput"

  val longerListOfAgentsYear1 = List(
    Agent("Breed_C", "1.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_C", "2.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_C", "3.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_C", "4.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_C", "5.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "6.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "7.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "8.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "9.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "10.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "11.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "12.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "13.0", 1, 1, 1, 1, 1, 1, 0, 1)
  )
  val longerListOfAgentsYear2 = List(
    Agent("Breed_C", "1.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_C", "2.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "3.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "4.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "5.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "6.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "7.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "8.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "9.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_C", "10.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_C", "11.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_C", "12.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_C", "13.0", 1, 1, 1, 1, 1, 1, 0, 1)
  )
  val longerListOfAgentsYear3 = List(
    Agent("Breed_C", "1.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_C", "2.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_C", "3.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_C", "4.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_C", "5.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "6.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "7.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "8.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "9.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "10.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "11.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "12.0", 1, 1, 1, 1, 1, 1, 0, 1),
    Agent("Breed_NC", "13.0", 1, 1, 1, 1, 1, 1, 0, 1)
  )

  val allYearsOfUpdatedAgents = List(longerListOfAgentsYear1, longerListOfAgentsYear2, longerListOfAgentsYear3)

  it should "Calculate the number of Breed_C agents" in {
    SimudyneApp.calculateAfterYearOutput(allYearsOfUpdatedAgents, 0).numberOfBreedC shouldBe 5
  }

  it should "Calculate the number of Breed_NC agents" in {
    SimudyneApp.calculateAfterYearOutput(allYearsOfUpdatedAgents, 0).numberOfBreedNC shouldBe 8
  }

  it should "Calculate the number of Breed_C lost" in {
    SimudyneApp.calculateAfterYearOutput(allYearsOfUpdatedAgents, 1).breedCLost shouldBe 3

  }

  it should "Calculate the number of Breed_C gained" in {
    SimudyneApp.calculateAfterYearOutput(allYearsOfUpdatedAgents, 2).breedCGained shouldBe 3
  }

  ignore should "Calculate the number of original Breed_C agents that were Breed_NC but have now changed back to Breed_C" in {

  }

}
