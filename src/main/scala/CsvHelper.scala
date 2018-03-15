import com.github.tototoshi.csv.{CSVReader, CSVWriter, MalformedCSVException}

object CsvHelper {

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
          auto_renew, inertia_for_switch,
          false))
      case _ => None
    }
  }


  def createOutput(outputByYear: List[YearlyOutput], finalYearOfCalculatedAgents: List[Agent]) = {
    createFinalAgentsOutput(finalYearOfCalculatedAgents)
    createDataOutputFile(outputByYear)
  }


  private def createFinalAgentsOutput(finalYearOfCalculatedAgents: List[Agent]) = {
    val finalAgentsWriter = CSVWriter.open("src/main/resources/output/FinalAgents.csv")
    finalAgentsWriter.writeRow(List(
      "Agent_Breed",
      "Policy_ID",
      "Age",
      "Social_Grade",
      "Payment_at_Purchase",
      "Attribute_Brand",
      "Attribute_Price",
      "Attribute_Promotions",
      "Auto_Renew",
      "Inertia_for_Switch"))

    finalYearOfCalculatedAgents.foreach(agent => {
      finalAgentsWriter.writeRow(List(
        agent.agent_breed,
        agent.policy_id,
        agent.age,
        agent.social_grade,
        agent.payment_at_purchase,
        agent.attribute_brand,
        agent.attribute_price,
        agent.attribute_promotions,
        agent.auto_renew,
        agent.inertia_for_switch))
    })
  }

  private def createDataOutputFile(outputByYear: List[YearlyOutput]) = {
    val finalAgentsWriter = CSVWriter.open("src/main/resources/output/YearlyStats.csv")
    finalAgentsWriter.writeRow(List(
      "numberOfBreedC",
        "numberOfBreedNC",
        "breedCGained",
        "breedCLost",
        "breedCRegained"
    ))

    outputByYear.foreach(yearStats => {
      finalAgentsWriter.writeRow(List(
        yearStats.numberOfBreedC,
        yearStats.numberOfBreedNC,
        yearStats.breedCGained,
        yearStats.breedCLost,
        yearStats.breedCRegained))
    })
  }
}
