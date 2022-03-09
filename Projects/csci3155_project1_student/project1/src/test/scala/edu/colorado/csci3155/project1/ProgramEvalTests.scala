package edu.colorado.csci3155.project1

import org.scalatest.funsuite._

class ProgramEvalTests extends AnyFunSuite {
    test("Test1: single command test"){
        val cmd1 = "(define xx (/ (+ 2 ( * 3 4 )) 7) )"
        val lp = new LispParser(cmd1)
        val ast_cmd1 = lp.parse_command()
        println(ast_cmd1)
        val new_env = Interpreter.evalCommand(Map.empty, ast_cmd1)
        assert (new_env.contains("xx"))
        assert (new_env("xx") == NumValue(2.0))
    }


    test("Test2: single command test"){
        val cmd1 = "(display (/ (+ 2 ( * 3 4 )) 7) )"
        val lp = new LispParser(cmd1)
        val ast_cmd1 = lp.parse_command()
        println(ast_cmd1)
        val new_env = Interpreter.evalCommand(Map.empty, ast_cmd1)
        assert (new_env.size == 0)
    }

    test("Test3: program # 1"){
        val cmd1 =
            """ (define x 2)
              | (define y (+ x 2))
              | (define z (* x (+ y y)) )
              | (display z) """.stripMargin
        val lp = new LispParser(cmd1)
        val ast_prog1:CalcProgram = lp.parse_list_of_commands()
        println(ast_prog1)
        val new_env = Interpreter.evalProgram(ast_prog1)
        assert (new_env.contains("x"))
        assert (new_env.contains("y"))
        assert (new_env.contains("z"))
        assert (new_env("x") == NumValue(2))
        assert (new_env("y") == NumValue(4))
        assert (new_env("z") == NumValue(16))
    }


    test("Test4: program # 2"){
        val cmd1 = {
            """(define x ( + 2 ( * 3 5 )) )
            |(display x)
            |(define y ( - x ( + 2 5 ) ) )
            |(define z ( + x ( * y y )))""".stripMargin
        }
        val lp = new LispParser(cmd1)
        val ast_prog1:CalcProgram = lp.parse_list_of_commands()
        println(ast_prog1)
        val new_env = Interpreter.evalProgram(ast_prog1)
        assert (new_env.contains("x"))
        assert (new_env.contains("y"))
        assert (new_env.contains("z"))
        assert (new_env("x") == NumValue(17))
        assert (new_env("y") == NumValue(10))
        assert (new_env("z") == NumValue(117))
    }

    test("Test5: Defining Erroneous expression does not modify environment") {
        val cmd = "(define x ( + x x ) )" // Error because x is undefined.
        val env0 = Map("z" -> NumValue(20.2))
        val lp = new LispParser(cmd)
        val cmd_ast = lp.parse_command()
        println(cmd_ast)
        val new_env = Interpreter.evalCommand(env0, cmd_ast)
        assert (new_env.size == 1)
        assert (new_env.contains("z"))
        assert (new_env("z") == NumValue(20.2))
    }

    test("Test6: Defining Erroneous expression does not modify environment") {
        val cmd = "(define x ( - 0 (/ 0 0) )  )" // Error because 0/0 is computed.
        //However, semantics of define on erroneus expression says that the
        // error should be ignored and environment should not be updated.
        val env0 = Map("z" -> NumValue(20.2))
        val lp = new LispParser(cmd)
        val cmd_ast = lp.parse_command()
        println(cmd_ast)
        val new_env = Interpreter.evalCommand(env0, cmd_ast)
        assert (new_env.size == 1)
        assert (new_env.contains("z"))
        assert (new_env("z") == NumValue(20.2))
    }
}
