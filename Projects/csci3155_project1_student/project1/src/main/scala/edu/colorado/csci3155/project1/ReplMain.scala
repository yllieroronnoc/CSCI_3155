package edu.colorado.csci3155.project1

import scala.annotation.tailrec
import scala.io.StdIn.readLine

/*

Repl: Display a command prompt that accepts input from the user.
The inputs can be a line of a lisp calc program or special commands:
  - inspect -- Print out the current environment
  - quit -- Finish the interpreter

Here is how an example interaction would look:
----

Welcome to Lisp Calculator!
lispCalc> (define x ( + 2 ( * 3 5 )) )
lispCalc> (display x)
Displaying result: 17.0
lispCalc> (define y ( - x ( + 2 5 ) ) )
lispCalc>inspect
x -> 17.0
y -> 10.0
lispCalc>quit

----

 */

object ReplMain extends App {
    type Environment = Map[String, Value]
    println("Welcome to Lisp Calculator!")

    /* Pretty Print Enviromnent */
    def displayEnv(env: Environment) : Unit = {
        env.foreach {
            case (s, v) => {
                println(s + " -> " + ValueOps.value2String(v))
            }
        }
    }

    /* Single step of the REPL */
    @tailrec
    def replStep(env: Environment): Environment ={
        // 1. Read some input from the user.
        val s = readLine("lispCalc>").trim
        // 2. Check if it is a special command
        if (s == "inspect"){
            displayEnv(env)
            replStep(env)
        } else if (s == "exit" || s == "quit"){
            env
        } else {
            // It is a lisp calc program
            // Use it to update the environment rather
            // than start from the empty environment.
            val lp = new LispParser(s)
            val cmds = lp.parse_list_of_commands()
            val env_new = Interpreter.evalProgram(cmds, env)
            replStep(env_new)
        }
    }

    // First call to replStep -- This is called as part of the main function
    // that is created through the constructor for this object.
    replStep(Map.empty)

}
