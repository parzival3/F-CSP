package ac

trait Constraint {
  /*
   * A constraint is consistent if there is at least a neighbor that is not assigned or if all its variable are
   * congruent with the constraint
   * @param neighbor the list of variables for which this constraint is applied
   */
  def isConsistent(variables: List[Variable], assignments: Assignments): Boolean

  def relatesTo(variables: List[Variable]): Boolean

  def isSatisfied(mapVariableValue: Map[Variable, Int]): Boolean

  def relatesToVar(variable: Variable): Boolean

  def getOther(variable: Variable): Option[Variable]

  def neighbor: List[Variable]

  def isUnary: Boolean
}

case class Binary(var1: Variable, var2: Variable, fun: (Int, Int) => Boolean) extends Constraint {

  override def isUnary = false

  override def isConsistent(variables: List[Variable], assignments: Assignments): Boolean = {
    require(variables.size == 2)
    !assignments.areAssigned(variables) || fun(assignments.mapVarValue(variables.head), assignments.mapVarValue(variables(1)))
  }

  override def relatesTo(variables: List[Variable]): Boolean = {
    require(variables.size == 2)
    variables.contains(var1) || variables.contains(var2)
  }

  override def isSatisfied(mapVariableValue: Map[Variable, Int]): Boolean =  {
    fun(mapVariableValue(var1), mapVariableValue(var2))
  }

  override def relatesToVar(variable: Variable): Boolean = {
    variable == var1 || variable == var2
  }

  override def getOther(variable: Variable): Option[Variable] = {
    if (variable == var1) Some(var2) else Some(var1)
  }

  override def neighbor: List[Variable] = List(var1, var2)
}

case class Unary(var1: Variable, fun: (Int) => Boolean) extends Constraint {

  override def isUnary = true

  override def isConsistent(variables: List[Variable], assignments: Assignments): Boolean = {
    !assignments.mapVarValue.contains(var1) || fun(assignments.mapVarValue(var1))
  }

  override def relatesTo(variables: List[Variable]): Boolean = {
    false
  }

  override def isSatisfied(mapVariableValue: Map[Variable, Int]): Boolean =  {
    fun(mapVariableValue(var1))
  }

  override def relatesToVar(variable: Variable): Boolean = {
    variable == var1
  }

  override def getOther(variable: Variable): Option[Variable] = None

  override def neighbor: List[Variable] = List()
}
