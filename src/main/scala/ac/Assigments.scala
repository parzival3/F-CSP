package ac

case class Assignments(mapVarValue: Map[Variable, Int] = Map[Variable, Int]()) {

  def addValue(unassignedVar: Variable, value: Int): Assignments = {
    Assignments(mapVarValue + (unassignedVar -> value))
  }

  def getUnassignedVariable(variables: List[Variable]): Variable = {
    require(notComplete(variables))
    variables.filter(notAssigned).head
  }

  def assigned(variable: Variable): Boolean = {
    mapVarValue.contains(variable)
  }
  def areAssigned(variables: List[Variable]): Boolean = {
    variables.forall(mapVarValue.contains)
  }
  def notAssigned(variable: Variable): Boolean = !assigned(variable)

  def isComplete(variableList: List[Variable]): Boolean = mapVarValue.size == variableList.size
  def notComplete(variableList: List[Variable]): Boolean = !isComplete(variableList)
}
