
[![Github actions](https://github.com/parzival3/csp/workflows/Scala%20CI/badge.svg)](https://parzival3.github.io/csp/) [![codecov](https://codecov.io/gh/parzival3/csp/branch/main/graph/badge.svg?token=IUUDSY98P9)](https://codecov.io/gh/parzival3/csp)


# Chisel CRV
Chisel CRV is a project that aims to mimic the functionality of SystemVerilog constraint programming and integrates them into [chisel-tester2](https://github.com/ucb-bar/chisel-testers2).
Chisel CRV combines a Constraint Satisfactory Problem Solver, with some helper classes to create and use random objects inside your tests.

## Comparison
### System Verilog

```systemverilog
class frame_t;
rand pkt_type ptype;
rand integer len;
randc bit [1:0] no_repeat;
rand bit  [7:0] payload [];
// Constraint the members
constraint legal {
  len >= 2;
  len <= 5;
  payload.size() == len;
}
```

### Chisel CRV
```scala
class Frame extends sv.Random(33) {
  var pkType: RandInt = rand(pkType, pktType.domainValues())
  var len: RandInt = rand(len, 0 to 10 toList)
  var noRepeat: RandCInt = randc(noRepeat, 0 to 1 toList)
  var payload: RandInt = rand(payload, 0 to 7 toList)

  val myBlock: ConstraintBlock = constraintBlock (
    unary ( len => len >= 2),
    unary (len =>  len <= 5),
    binary ((len, payload) => len == payload)
  )

  override def toString = s"pType = ${pktType(pkType)}, Len = $len, noRepeat = $noRepeat, payload = $payload"
}
```

## CSP Solver
Based on the ideas of the book [Artificial Intelligence A Modern Approach](https://www.pearson.com/us/higher-education/program/Russell-Artificial-Intelligence-A-Modern-Approach-4th-Edition/PGM1263338.html),
Is a combination of  **BacktrackSearching** and **Constraint Propagation**.
The pseudocode for the algorithm used can be found [here](http://aima.cs.berkeley.edu/algorithms.pdf).
The CSP solver and the relative components are stored in the `csp` package.

## Random Class
The random class is the base class that each Random object should extend in order to implement
constrained programming.
### Declaration
Create a class that inherits from the base class `sv.Random`
```scala
class Frame extends sv.Random
```
The class can also be seeded like
```scala
class Frame extends sv.Random(33)
```

You can add random field by declaring a `var` field as type `RandInt` and assigin it a domain
with the `rand` macro. A domain is a list of values. For now only integer values are supported.
```scala
  var len: RandInt = rand(len, 0 to 10 toList)
```

You can also add a continuous Random Integer by adding a `var` field to your class and declaring it
`RandIntC`
```scala
  var noRepeat: RandCInt = randc(noRepeat, 0 to 1 toList)
```
Finally you can constraint the newly added fields by creating a constraint block 
```scala
  val myBlock: ConstraintBlock = constraintBlock (
    unary ( len => len >= 2),
    unary (len =>  len <= 5),
  )
```

### Usage

To randomize the newly create random object, just call the `randomize` method
```scala
val frame = new Frame
while (frame.randomize) {
  println(frame)
  assert(frame.len >= 2)
  assert(frame.len <= 5)
}
```



## Todo list
- [ ] Refactoring the csp package
- [ ] Add random arrays
- [ ] Add soft constraint

