package ac

import org.scalatest.flatspec.AnyFlatSpec

class CSPTest extends AnyFlatSpec {
  behavior of "CSP"

  private val generalFixture = new {
    val varA: Variable = Variable("a")
    val varB: Variable = Variable("b")
    val varC: Variable = Variable("c")
    val varD: Variable = Variable("d")

    // TODO: pay attention for now how to assign constraint
    val c1: Binary = Binary(varA, varB, (a: Int, b: Int) => a + 1 <= b)
    val c2: Binary = Binary(varA, varB, (a: Int, b: Int) => a * a == b)
    val c3: Binary = Binary(varB, varC, (b: Int, c: Int) => b * b == 2 * c)
    val c4: Binary = Binary(varC, varB, (b: Int, c: Int) => b * b == 2 * c)
    val c5: Binary = Binary(varB, varD, (b: Int, c: Int) => b * b == 2 * c)

    val listOf2Domains = Map(
      varA -> Domain((0 to 4).toList),
      varB -> Domain((0 to 4).toList),
    )
    val listOf3Domains = Map(
      varA -> Domain((0 to 4).toList),
      varB -> Domain((0 to 4).toList),
      varC -> Domain((0 to 4).toList),
    )
    val listOf4Domains = Map(
      varA -> Domain((0 to 4).toList),
      varB -> Domain((0 to 4).toList),
      varC -> Domain((0 to 4).toList),
      varD -> Domain((0 to 4).toList),
    )
  }

  it should "getConstraint should return List(c1)" in {
    import generalFixture._
    val directGraph =  List(c1)
    val csp = new CSP(List(varA,varB), listOf2Domains, directGraph)
    assert(List(c1) == csp.getConstraints(List(varA, varB)))
  }

  it should "getConstraint should return the correct list for constraints" in {
    import generalFixture._
    val directGraph = List(c1, c3)
    val csp = new CSP(List(varA,varB, varC), listOf3Domains, directGraph)
    println(csp.getConstraints(List(varA, varB)))
    assert(List(c1) == csp.getConstraints(List(varA, varB)))
    assert(List(c3) == csp.getConstraints(List(varC, varB)))
    assert(List(c1) == csp.getConstraints(List(varB, varA)))
    assert(List(c3) == csp.getConstraints(List(varB, varC)))
  }

  it should "able to revise the arcs between its variables" in {
    import generalFixture._
    val directGraph = List(c1, c2, c3, c4, c5)
    val csp = new CSP(List(varA,varB, varC, varD), listOf3Domains, directGraph)
    assert(List() == csp.reviseArcs(varA, varB))
    assert(List((varC, varB), (varD, varB)) == csp.reviseArcs(varB, varA))
    assert(List() == csp.reviseArcs(varC, varB))
    assert(List() == csp.reviseArcs(varD, varB))
  }
}
