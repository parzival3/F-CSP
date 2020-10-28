package sv
//import constraint.SVMacros.RandInt

import csp.{Assignments, CSP, Constraint, Domain, Node, Solution, Variable}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.macros.blackbox

object Random{
  type RandInt = Int
  type RandCInt = Int
  type RandArray = Array[Int]
}

class Random(val seed: Int = 42) {

  /**
    * For convenience during development these attributes are public
    * TODO: change back to private
    */
  var cspO: Option[CSP] = None
  var randVars: List[(Variable, Domain)] = List[(Variable, Domain)]()
  var randVarsM: mutable.HashMap[Variable, Domain] = mutable.HashMap[Variable, Domain]()
  val mapOfConstraint: ListBuffer[Constraint] = ListBuffer[Constraint]()
  var randCVars: List[(Variable, Iterator[Int])] = List[(Variable, Iterator[Int])]()
  var randCVarsM: mutable.HashMap[Variable, Iterator[Int]] = mutable.HashMap[Variable, Iterator[Int]]()
  var iterator: Option[Iterator[Solution with Node]] = None
  var cAssignments: Option[Assignments] = Some(Assignments())

  /**
   * Constraint block class. This class encapsulate a list of constraints.
   * @param constraints
   * @param r
   */
  class ConstraintBlock(val constraints: List[Constraint], val r: Random) {
    def disable(): Unit = {
      r.removeConstraints(constraints)
      r.restartIterator()
    }

    def enable(): Unit = {
      r.addConstraints(constraints)
      r.restartIterator()
    }
  }

  def rand_impl(myMap: (String, List[Int])): Variable = {
    val addVar = Variable(myMap._1) -> Domain(myMap._2)
    randVarsM += (Variable(myMap._1) -> Domain(myMap._2))
    randVars = randVars ::: List(addVar)
    Variable(myMap._1)
  }

  def addRandVar(myMap: (String, List[Int])): Unit = {
    randVarsM = randVarsM += (Variable(myMap._1) -> Domain(myMap._2))
  }

  def addRandCVar(myMap: (String, List[Int])): Unit = {
    val iter = LazyList.continually(myMap._2).flatten.iterator
    randCVarsM = randCVarsM += Variable(myMap._1) -> iter
  }

  def rand(param: Array[Int]): Array[Int] = macro sv.RandomMacros.randArray
  def rand(param: Int, dom: Range): Int = macro sv.RandomMacros.randVarDec
  def randc(param: Int, dom: Range): Int = macro sv.RandomMacros.randCVarDec
  def unary(param: (Int) => Boolean): Constraint = macro sv.RandomMacros.createUnary
  def binary(param: (Int, Int) => Boolean): Constraint = macro sv.RandomMacros.createBinary
  def randomize: Boolean = macro sv.RandomMacros.randomMacroImpl

  def randc_impl(myMap: (String, List[Int])): Variable = {
    val iter = LazyList.continually(myMap._2).flatten.iterator
    val addVar = Variable(myMap._1) -> iter
    randCVars = randCVars ::: List(addVar)
    Variable(myMap._1)
  }

  def restartIterator(): Unit = {
    cspO = Some(new CSP(randVarsM.keys.toList, randVarsM.toMap, mapOfConstraint.toList))
    iterator = Some(Solution(cspO.get, Assignments(), seed).backtrackingSearch(cspO.get).iterator)
  }

  def randomizeImp(): Option[Assignments] = {
    // TODO: change return in Boolean
    if (randCVars.isEmpty) {
      cAssignments = Some(Assignments(randCVarsM.flatMap(x => Map(x._1 -> x._2.next())).toMap))
      cAssignments
    } else {
      val cspAss = iterator match {
        case None =>
          restartIterator()
          if (iterator.isEmpty) None else Some(iterator.get.next().assignments)
        case Some(x) => if (x.isEmpty) None else Some(x.next().assignments)
      }

      cAssignments = cspAss match {
        case None => None
        case Some(x) => Some(Assignments(x.mapVarValue ++ randCVarsM.flatMap(x => Map(x._1 -> x._2.next())).toMap))
      }
      cAssignments
    }
  }

  def constraintBlock(constraints: Constraint*): ConstraintBlock = {
    addConstraints(constraints.toList)
    new ConstraintBlock(constraints.toList, this)
  }

  def removeConstraints(constraints: List[Constraint]): Unit = {
    mapOfConstraint --= constraints
  }

  def addConstraints(constraints: List[Constraint]): Unit = {
    mapOfConstraint ++= constraints
  }
}

object RandomMacros extends Random {
  /**
    * Macro to create a binary constraint from lambda function
    * @param c Context
    * @param param lambda function to apply on the specific variables expressed inside the function
    * @return
    */
  def createBinary(c: blackbox.Context)(param: c.Expr[(Int, Int) => Boolean]): c.Tree = {
    import c.universe._
    val Function(args, _) = param.tree
    val ValDef(_, xName, _, _) = args.head
    val ValDef(_, yName, _, _) = args(1)
    val varX = xName.toString
    val varY = yName.toString
    val func = reify {
      param.splice
    }
    c.untypecheck(param.tree)
    q"_root_.csp.Binary(_root_.csp.Variable($varX), _root_.csp.Variable($varY), $func)"
  }

  /**
    * Macro to create a unary constraint from lambda function
    * @param c Context
    * @param param lambda function to apply on the specific variables expressed inside the function
    * @return
    */
  def createUnary(c: blackbox.Context)(param: c.Expr[Int => Boolean]): c.Tree = {
    import c.universe._
    val Function(args, _) = param.tree
    val ValDef(_, name, _, _) = args.head
    val varName = name.toString
    val func = reify {
      param.splice
    }
     c.untypecheck(param.tree)
    q"_root_.csp.Unary(_root_.csp.Variable($varName), $func)"
  }

  /**
    * TODO: Complete the method
    * @param c Context
    * @param param Variable
    * @return
    */
  def randArray(c: blackbox.Context)(param: c.Expr[sv.Random.RandArray]): c.Tree = {
    import c.universe._
    val typeString = param.actualType.toString
    val paramRep = show(param.tree)
    val newName = paramRep.substring(paramRep.lastIndexOf(".") + 1)
    // TODO: How can we check for a specific type?... matching a string is not very safe
    typeString match {
      case "sv.Random.RandArray" => q"Array(10)"
      case _ => q"""
              throw new Exception($newName + " is not declared as RandArray")
              """
    }
  }


  /**
    * Continuous Random Variable Declaration, this functions declares a random variable by setting its value to 0 and
    * adds it to the randCVarM variables database.
    *
    * @param c Context
    * @param param the current field that will be randomized
    * @param dom the domain in which the current variable has meaning
    * @return
    */
 def randCVarDec(c: blackbox.Context)(param: c.Expr[sv.Random.RandCInt], dom: c.Tree): c.Tree = {
    import c.universe._
    // TODO: How can we check for a specific type?... matching a string is not very safe
    val self = c.prefix
    val typeString = param.actualType.toString
    val paramRep = show(param.tree)
    val newName = paramRep.substring(paramRep.lastIndexOf(".") + 1)
    // TODO: How can we check for a specific type?
    typeString match {
      case "sv.Random.RandCInt" =>
        q"""
          $self.addRandCVar(($newName, $dom.toList))
          0
        """
      case _ => q"""
              throw new Exception($newName + " is not declared as RandCInt")
              """
    }
  }

  /**
    * Random Variable Declaration, this functions declares a random variable by setting its value to 0 and
    * adds it to the randVarM variables database.
    *
    * @param c Context
    * @param param the current field that will be randomized
    * @param dom the domain in which the current variable has meaning
    * @return
    */
  def randVarDec(c: blackbox.Context)(param: c.Expr[sv.Random.RandInt], dom: c.Tree): c.Tree = {
    import c.universe._
    // TODO: using Tree is not very safe
    val self = c.prefix
    val typeString = param.actualType.toString
    val paramRep = show(param.tree)
    val newName = paramRep.substring(paramRep.lastIndexOf(".") + 1)
    // TODO: How can we check for a specific type?... matching a string is not very safe
    typeString match {
      case "sv.Random.RandInt" =>
        q"""
          $self.addRandVar(($newName, $dom.toList))
          0
        """
      case _ => q"""
              throw new Exception($newName + " is not declared as RandInt")
              """
    }
  }

  /**
    * Macro that randomizes the current class by solving a CSP
    * TODO: How should we handle the randomization errors?
    * @param c Context
    * @return Boolean
    */
  def randomMacroImpl(c: blackbox.Context): c.Tree = {
    import c.universe._
    val self = c.prefix
    // Get the current class type
    val currentType = self.actualType.baseType(self.actualType.baseClasses.head)
    // Map containing all the terms name and their corresponding setters
    val nsMap = for {
      t <- currentType.members
      s <- currentType.members
      tName = t.name.decodedName.toString.filter(_ != ' ')
      sName = s.name.decodedName.toString
      if t.isTerm && !t.isMethod && sName == tName + "_="
    } yield (t.asTerm.name.toString.filter(_ != ' '), s.asTerm.name)

    /**
      * For each of the terms/variables in the current class, check if the current variable is defined in one of the
      * maps and assign the new value
      * This is necessary because I couldn't find an easy way to lift [[nsMap]] inside a quasiquote.
      */
    val operations = nsMap.map { x =>
       val variable = x._1
       val setter = x._2
      q"""
          val myVar = Variable($variable)
          if ($self.cAssignments.isDefined) {
            if ($self.randVarsM.contains(myVar) || $self.randCVarsM.contains(myVar)) {
              val assignments = $self.cAssignments.get
              $self.$setter = assignments(myVar)
            }
          }
       """
    }

    /**
      * First call randomize implementation in order to populate the assignments and then assign the new value
      * to each variable
       */
    q"""
       $self.randomizeImp()
       ..$operations
       $self.cAssignments.isDefined
     """
  }
}
