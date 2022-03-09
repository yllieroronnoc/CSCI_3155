package edu.colorado.csci3155.project1

/*-- Value type: all the possible values that interpreting a program can result in --*/
sealed trait Value
case class NumValue(d: Double) extends Value
case class BoolValue(b: Boolean) extends Value
// We do not have a value for Error. Instead, we will throw a RuntimeError exception and bail out.


object ValueOps {

    def value2String(v: Value) = v match {
        case NumValue(d) => d.toString()
        case BoolValue(b) => b.toString()
        case _ => throw new RuntimeError(s"Unexpected value $v")
    }

    def isBoolean(v: Value): Boolean = v match {
        case NumValue(d) => false 
        case BoolValue(b) => true 
        case _ => throw new RuntimeError(s"Unexpected value $v")
    }

    /*-- Function op_values
       Unpack the two values. If they are numbers, apply the operation specified by
       op, otherwise just throw a runtime error.
     */
    def op_values(v1: Value, v2: Value, op: (Double, Double) => Value): Value = (v1, v2) match {
        case (NumValue(d1), NumValue(d2)) => op(d1, d2)
        case _ => throw new RuntimeError(s"Type error: expected number but got boolean")
    }


    def plus(v1: Value, v2: Value) = op_values(v1, v2, (a,b) => NumValue(a + b))
    /*
      TODO: Complete the functions below. Try to implement them the same
          way we have done for the plus function above.
     */
    def minus(v1: Value, v2: Value):Value =  op_values(v1,v2,(a,b) => NumValue(a - b))
    def mult(v1: Value, v2: Value): Value = op_values(v1,v2,(a,b) => NumValue(a * b))
    def div(v1: Value, v2: Value): Value = op_values(v1,v2,(a,b) => NumValue(a / b))
    def geq(v1: Value, v2: Value):Value = op_values(v1, v2, (a, b) => BoolValue(a >= b))
    def gt(v1: Value, v2: Value): Value = op_values(v1,v2,(a,b) => BoolValue(a > b))
    def eq(v1: Value, v2: Value): Value = op_values(v1,v2,(a,b) => BoolValue(a == b))

}