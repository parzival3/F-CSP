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
Based on the ideas in the book [Artificial Intelligence A Modern Approach](https://www.pearson.com/us/higher-education/program/Russell-Artificial-Intelligence-A-Modern-Approach-4th-Edition/PGM1263338.html),
Is a combination of  **BacktrackSearching** and **Constraint Propagation**.
The pseudocode for the algorithm used can be found. [here](http://aima.cs.berkeley.edu/algorithms.pdf)


## Random Class



