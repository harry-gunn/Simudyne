import com.github.tototoshi.csv._

import scala.annotation.tailrec
import scala.util.Random._

object SimudyneApp extends App {

  val NO_BRAND_FACTOR_GIVEN: Double = 0.0

  val csvPath = "src/main/resources/Simudyne_Backend_Test.csv"
  val agents = parseCsv(csvPath)

  val userBrandFactor: Double = scala.util.Try(args.head.toDouble).getOrElse(0.0)
  val finalCalulatedAgents = calculate15Years(agents, userBrandFactor)

  println(calculateAfterYearOutput(finalCalulatedAgents, 15).numberOfBreedC)
  println(calculateAfterYearOutput(finalCalulatedAgents, 15).numberOfBreedNC)


  def calculateAfterYearOutput(agents: List[List[Agent]], yearNumber: Int): YearlyOutput = {
    def calculateNumberOfBreedC: Int = agents(yearNumber).filter(_.agent_breed == "Breed_C").length
    def calculateNumberOfBreedNC: Int = agents(yearNumber).filter(_.agent_breed == "Breed_NC").length

    def calculateBreedCLost: Int = {
      if(yearNumber == 0) 0 else {
        val filteredPreviousYearIds = agents(yearNumber - 1)
          .filter(_.agent_breed == "Breed_C")
          .map(agent => agent.policy_id)
        agents(yearNumber)
          .filter(_.agent_breed == "Breed_NC")
          .filter(agent => filteredPreviousYearIds.contains(agent.policy_id))
          .length
      }
    }

    def calculateBreedCGained: Int = {
      if(yearNumber == 0) 0 else {
        val filteredPreviousYearIds = agents(yearNumber - 1)
          .filter(_.agent_breed == "Breed_NC")
          .map(agent => agent.policy_id)
        agents(yearNumber)
          .filter(_.agent_breed == "Breed_C")
          .filter(agent => filteredPreviousYearIds.contains(agent.policy_id))
          .length
      }
    }

    YearlyOutput( calculateNumberOfBreedC,
                  calculateNumberOfBreedNC,
                  calculateBreedCGained,
                  calculateBreedCLost
    )
  }

  def calculate15Years(agents: List[Agent], userBrandFactor: Double): List[List[Agent]] = {
    @tailrec
    def helperMethod(updatedAgents: List[Agent], result: List[List[Agent]], yearCounter: Int): List[List[Agent]] = {
      yearCounter match {
        case 0 => List(updatedAgents) ::: result
        case _ => helperMethod(passOneYear(updatedAgents, userBrandFactor), List(updatedAgents) ::: result, yearCounter - 1)
      }
    }

    helperMethod(agents, List(), 15)
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
            agent.social_grade * agent.attribute_brand)) agent.copy(agent_breed = "Breed_NC")
          else agent
        case "Breed_NC" =>
          if(calculateAffinity(agent) < (
            agent.social_grade * agent.attribute_brand * calculateBrandFactor(userBrandFactor))) agent.copy(agent_breed = "Breed_C")
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

  def parseCsv(pathToCsv: String) = {
    val allLinesFromCsv = try {
      val reader = CSVReader.open(pathToCsv)
      reader.all()
    }
    catch {

      case malformedError: MalformedCSVException => List()
    }

    allLinesFromCsv.map(inner => makeAgent(inner))
        .flatten
        .filterNot(_.agent_breed == "Agent_Breed")

  }

  private def makeAgent(rawAgentDetails: List[String]): Option[Agent] = {
    import java.text.DecimalFormat
    val formatter = new DecimalFormat("#.###")
    val agentDetails = rawAgentDetails.map(x => {try {x.toDouble} catch { case exc: NumberFormatException => x} })

    agentDetails match {
      case List(agent_breed: String, policy_id: Double,
                age: Double, social_grade: Double,
                payment_at_purchase: Double, attribute_brand: Double,
                attribute_price: Double, attribute_promotions: Double,
                auto_renew: Double, inertia_for_switch: Double
        ) => Some(
        Agent(agent_breed, f"$policy_id%1.1f",
              age, social_grade,
              payment_at_purchase, attribute_brand,
              attribute_price, attribute_promotions,
              auto_renew, inertia_for_switch))
      case _ => None
    }


  }
}