object StatsCalculator {

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

    def calculateBreedCRegained: Int = {
      if(yearNumber < 2) 0 else {
        val filteredFirstYearIds = agents(0)
          .filter(_.agent_breed == "Breed_C")
          .map(agent => agent.policy_id)

        agents(yearNumber)
          .filter(_.agent_breed == "Breed_C")
          .filter(_.breedChange == true)
          .filter(agent => filteredFirstYearIds.contains(agent.policy_id))
          .length
      }
    }

    YearlyOutput(calculateNumberOfBreedC,
      calculateNumberOfBreedNC,
      calculateBreedCGained,
      calculateBreedCLost,
      calculateBreedCRegained
    )

  }
}
