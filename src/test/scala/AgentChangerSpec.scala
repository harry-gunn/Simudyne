import org.scalatest.{FlatSpec, Matchers}

class AgentChangerSpec extends FlatSpec with Matchers {

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

  behavior of "calculateYears"

  it should "return a List[List[Agent]], each List[Agent] for a different year" in {
    AgentChanger.calculateYears(15, baseTestAgent, 2).length shouldBe 17
  }

  behavior of "passOneYear"

  it should "increase the age of the agent by 1" in {
    val agentWithZeroAge = baseTestAgent.map(agent => agent.copy(age = 0))
    AgentChanger.increaseAge(baseTestAgent).head.age shouldBe 11
    AgentChanger.increaseAge(agentWithZeroAge).head.age shouldBe 1
  }

  behavior of "filterOutAutoRenew"

  it should "return empty list when the auto_renew is 1" in {
    val nonAutoRenewAgent = baseTestAgent.map(agent => agent.copy(auto_renew = 1))
    AgentChanger.filterOutAutoRenew(nonAutoRenewAgent) shouldBe List()
  }

  it should "not filter out an agent when the auto renew is 0" in {
    AgentChanger.filterOutAutoRenew(baseTestAgent) shouldBe baseTestAgent
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
    1,
    false
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
    1,
    false
  ))

  it should s"Return a Breed_NC Agent when given a Breed_C and high values" in {
    val agent = AgentChanger.calculateBreedChanges(lowAffinityAgent, 2).head

    agent.agent_breed shouldBe "Breed_NC"
    agent.breedChange shouldBe true
  }

  it should s"Return a Breed_C Agent when given a Breed_NC and high values" in {
    val lowAffinityNCAgent = List(lowAffinityAgent.head.copy(agent_breed = "Breed_NC"))
    val agent = AgentChanger.calculateBreedChanges(lowAffinityNCAgent, 2).head

    agent.agent_breed shouldBe "Breed_C"
    agent.breedChange shouldBe true

  }

  it should s"Return a Breed_C Agent when given a Breed_C and low values" in {
    val agent = AgentChanger.calculateBreedChanges(highAffinityAgent, 0.1).head

    agent.agent_breed shouldBe "Breed_C"
    agent.breedChange shouldBe false

  }

  it should s"Return a Breed_NC Agent when given a Breed_NC and low values" in {
    val highAffinityNCAgent = List(highAffinityAgent.head.copy(agent_breed = "Breed_NC"))
    val agent = AgentChanger.calculateBreedChanges(highAffinityNCAgent, 0.1).head

    agent.agent_breed shouldBe "Breed_NC"
    agent.breedChange shouldBe false

  }

  it should
    """Calculate the correct breed given different agent values -
      | this is repeated due to the random element of the calculation""".stripMargin in {

    for (x <- 1 to 1000) {
      AgentChanger.calculateBreedChanges(lowAffinityAgent, 2).head.agent_breed shouldBe "Breed_NC"

      val lowAffinityNCAgent = List(lowAffinityAgent.head.copy(agent_breed = "Breed_NC"))
      AgentChanger.calculateBreedChanges(lowAffinityNCAgent, 0.1).head.agent_breed shouldBe "Breed_C"

      AgentChanger.calculateBreedChanges(highAffinityAgent, 2).head.agent_breed shouldBe "Breed_C"

      val highAffinityNCAgent = List(highAffinityAgent.head.copy(agent_breed = "Breed_NC"))
      AgentChanger.calculateBreedChanges(highAffinityNCAgent, 0.1).head.agent_breed shouldBe "Breed_NC"
    }
  }

}
