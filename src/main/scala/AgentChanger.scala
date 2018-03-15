import scala.annotation.tailrec
import scala.util.Random._

object AgentChanger {

  val NO_BRAND_FACTOR_GIVEN: Double = 0.0

  def calculateYears(years: Int = 15, agents: List[Agent], userBrandFactor: Double): List[List[Agent]] = {

    @tailrec
    def helperMethod(updatedAgents: List[Agent], result: List[List[Agent]], yearCounter: Int): List[List[Agent]] = {
      yearCounter match {
        case -1 => List(updatedAgents) ::: result
        case _ => helperMethod(passOneYear(updatedAgents, userBrandFactor), List(updatedAgents) ::: result, yearCounter - 1)
      }
    }

    helperMethod(agents, List(), years)
  }

  def passOneYear(agents: List[Agent], userBrandFactor: Double): List[Agent] = {
    calculateBreedChanges(increaseAge(agents), userBrandFactor)
  }

  def increaseAge(agents: List[Agent]): List[Agent] = agents.map(agent => agent.copy(age = agent.age + 1))

  def calculateBreedChanges(agents: List[Agent], userBrandFactor: Double):List[Agent] = {
    val agentsToChange = filterOutAutoRenew(agents)
    val calculatedAgentsAfterTheYear = agentsToChange.map(agent => {
      agent.agent_breed match {
        case "Breed_C" =>
          if(calculateAffinity(agent) < (
            agent.social_grade * agent.attribute_brand)) agent.copy(agent_breed = "Breed_NC", breedChange = true)
          else agent
        case "Breed_NC" =>
          if(calculateAffinity(agent) < (
            agent.social_grade * agent.attribute_brand * calculateBrandFactor(userBrandFactor))) agent.copy(
            agent_breed = "Breed_C", breedChange = true)
          else agent
        case _ => agent
      }
    })
    calculatedAgentsAfterTheYear ::: agents.filter(_.auto_renew == 1)
  }

  def filterOutAutoRenew(agents: List[Agent]) = agents.filterNot(_.auto_renew == 1)

  private def calculateBrandFactor(userBrandFactor: Double):Double = {
    if(userBrandFactor == NO_BRAND_FACTOR_GIVEN || isNotInCorrectRange(userBrandFactor)) 0.1 + (nextDouble() * 2.8)
    else userBrandFactor
  }

  private def isNotInCorrectRange(brandFactor: Double): Boolean = brandFactor < 0.1 || brandFactor > 2.9

  private def calculateAffinity(agent: Agent) = {
    val rand: Double = nextDouble() * 3
    agent.payment_at_purchase / agent.attribute_price + (rand * agent.attribute_promotions * agent.inertia_for_switch)
  }
}
