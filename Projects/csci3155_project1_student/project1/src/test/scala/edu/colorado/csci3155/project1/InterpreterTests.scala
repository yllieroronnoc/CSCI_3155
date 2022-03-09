package edu.colorado.csci3155.project1

import org.scalatest.funsuite._


class InterpreterTests extends AnyFunSuite {
    test("Test1:  expr + const  + plus + mult + div"){
        val str1 = "( / ( + 2 (* 5 10) ) 26 ) "
        val lp = new LispParser(str1)
        val e1 = lp.parse_expression()
        println(s"Parsed expression: $e1")
        val res = Interpreter.evalExpr(e1, Map.empty)
        assert (res == NumValue(2.0))
    }
    test("Test2:  expr + const  + plus + minux + mult"){
        val str1 = "( - ( + 2 (* 5 10) ) 26 ) "
        val lp = new LispParser(str1)
        val e1 = lp.parse_expression()
        println(s"Parsed expression: $e1")
        val res = Interpreter.evalExpr(e1, Map.empty)
        assert (res == NumValue(26.0))
    }

    test("Test3:  expr + geq "){
        val str1 = "( >= ( + 2 (* 5 10) ) 52 ) "
        val lp = new LispParser(str1)
        val e1 = lp.parse_expression()
        println(s"Parsed expression: $e1")
        val res = Interpreter.evalExpr(e1, Map.empty)
        assert (res == BoolValue(true))
    }

    test("Test4:  expr + gt "){
        val str1 = "( > ( * 2 (+ 6 (* 5 4 ) ) ) 52 ) "
        val lp = new LispParser(str1)
        val e1 = lp.parse_expression()
        println(s"Parsed expression: $e1")
        val res = Interpreter.evalExpr(e1, Map.empty)
        assert (res == BoolValue(false))
    }

    test("Test5:  expr + and "){
        val str1 = "( && ( > ( * 2 (+ 6 (* 5 4 ) ) ) 52 ) ( >= 10 10) ) "
        val lp = new LispParser(str1)
        val e1 = lp.parse_expression()
        println(s"Parsed expression: $e1")
        val res = Interpreter.evalExpr(e1, Map.empty)
        assert (res == BoolValue(false))
    }

    test("Test6:  expr + and + short-circuit"){
        val str1 = "( && ( > ( * 2 (+ 6 (* 5 4 ) ) ) 52 ) ( >= x 10) ) "
        val lp = new LispParser(str1)
        val e1 = lp.parse_expression()
        println(s"Parsed expression: $e1")
        val res = Interpreter.evalExpr(e1, Map.empty)
        assert (res == BoolValue(false))
    }

    test("Test7:  expr + and + short-circuit"){
        val str1 = "( && ( >= ( * 2 (+ 6 (* 5 4 ) ) ) 52 ) ( >= x 10) ) "
        val lp = new LispParser(str1)
        val e1 = lp.parse_expression()
        println(s"Parsed expression: $e1")
        val res = Interpreter.evalExpr(e1, Map("x"-> NumValue(8.0)))
        assert (res == BoolValue(false))
    }


    test("Test8:  expr + or "){
        val str1 = "( || ( > ( * 2 (+ 6 (* 5 4 ) ) ) 52 ) ( >= 10 10) ) "
        val lp = new LispParser(str1)
        val e1 = lp.parse_expression()
        println(s"Parsed expression: $e1")
        val res = Interpreter.evalExpr(e1, Map.empty)
        assert (res == BoolValue(true))
    }

    test("Test9:  expr + or + short-circuit"){
        val str1 = "( || ( > ( * 2 (+ 6 (* 5 4 ) ) ) 50 ) ( >= x 10) ) "
        val lp = new LispParser(str1)
        val e1 = lp.parse_expression()
        println(s"Parsed expression: $e1")
        val res = Interpreter.evalExpr(e1, Map.empty)
        assert (res == BoolValue(true))
    }

    test("Test10:  expr + or "){
        val str1 = "( || ( >= ( * 2 (+ 6 (* 5 4 ) ) ) 88 ) ( >= x 10) ) "
        val lp = new LispParser(str1)
        val e1 = lp.parse_expression()
        println(s"Parsed expression: $e1")
        val res = Interpreter.evalExpr(e1, Map("x"-> NumValue(8.0)))
        assert (res == BoolValue(false))
    }

    test("Test11:  expr + not "){
        val str1 = "( ! ( >= ( * 2 (+ 6 (* 5 4 ) ) ) 88 )  ) "
        val lp = new LispParser(str1)
        val e1 = lp.parse_expression()
        println(s"Parsed expression: $e1")
        val res = Interpreter.evalExpr(e1, Map("x"-> NumValue(8.0)))
        assert (res == BoolValue(true))
    }

    test("Test12:  expr + if "){
        val str1 = "( if ( > 35 22)  5  ( - 20 18 ) ) "
        val lp = new LispParser(str1)
        val e1 = lp.parse_expression()
        println(s"Parsed expression: $e1")
        val res = Interpreter.evalExpr(e1, Map("x"-> NumValue(8.0)))
        assert (res == NumValue(5))
    }



}
