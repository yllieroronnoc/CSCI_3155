package edu.colorado.csci3155.project1

import org.scalatest.funsuite._

class ValueOpsTest extends AnyFunSuite {
    test("Test Value Ops") {
        val v1 = NumValue(5.0)
        val v2 = NumValue(15.0)
        assert(ValueOps.plus(v1, v2) == NumValue(20.0))
        assert(ValueOps.minus(v1, v2) == NumValue(-10.0))
        assert(ValueOps.mult(v1, v2) == NumValue(75.0))
        assert(ValueOps.div(v2, v1) == NumValue(3.0))
    }

    test("Test Value Ops # 2") {
        val v1 = NumValue(5.0)
        val v2 = NumValue(15.0)
        assert(ValueOps.geq(v1, v2) == BoolValue(false))
        assert(ValueOps.geq(v2, v1) == BoolValue(true))
        assert(ValueOps.gt(v1, v2) == BoolValue(false))
        assert(ValueOps.gt(v2, v1) == BoolValue(true))
        assert(ValueOps.gt(v2, v2) == BoolValue(false))
        assert(ValueOps.geq(v2, v2) == BoolValue(true))
        assert(ValueOps.eq(v1, v1) == BoolValue(true))
        assert(ValueOps.eq(v1, v2) == BoolValue(false))
    }


}
