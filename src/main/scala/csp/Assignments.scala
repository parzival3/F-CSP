package csp

/**
  * Case class representing an assignment of type Variable -> Value
  * An assignment can be complete or partial respect to a list of variables.
  * @param mapVarValue Map of Variable -> Value
  */
case class Assignments(mapVarValue: Map[Variable, BigInt] = Map[Variable, BigInt]()) {

  def apply(variable: Variable): BigInt = {
    mapVarValue(variable)
  }

  /**
    * Add a value to the current assignments
    * @param unassignedVar
    * @param value
    * @return new Assignments with the new assignment added
    */
  def addValue(unassignedVar: Variable, value: BigInt): Assignments = {
    Assignments(mapVarValue + (unassignedVar -> value))
  }

  /**
   * TODO: Return a list of unassigned variables and do not create a random number generator here
    * Return the first unassigned variable in the current Assignments.
    * This is just an helper method, it should be changed to a random selection or completely removed.
    * @param variables list of variables for the current CSP
    * @return a Variable currently not assigned
    */
  def getUnassignedVariable(variables: List[Variable], seed: Int = 42): Variable = {
    require(isPartial(variables))
    val randg = new scala.util.Random(seed)
    randg.shuffle(variables.filterNot(assigned)).head
  }

  /**
    * Check if a variable is assigned
    * @param variable
    * @return Boolean
    */
  def assigned(variable: Variable): Boolean = {
    mapVarValue.contains(variable)
  }

  /**
    * Check if the list of variables are all assigned
    * @param variables
    * @return Boolean
    */
  def areAssigned(variables: List[Variable]): Boolean = {
    variables.forall(mapVarValue.contains)
  }

  /**
    * Check if the current Assignments is complete for the current CSP.
    * An Assignments is complete only if all the variable of the CSP have a value. Since we are using a Map to describe
    * the relation of Variable -> Value a check of the size of the Map compared with the size of the list of variables
    * should be enough
    * @param variableList
    * @return
    */
  def isComplete(variableList: List[Variable]): Boolean = mapVarValue.size == variableList.size

  /**
    * Check if the current Assignments is partial for the current CSP.
    * An Assignments is partial only if there is a Variable not assigned
    * @param variableList
    * @return
    */
  def isPartial(variableList: List[Variable]): Boolean = !isComplete(variableList)
}
