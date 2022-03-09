package edu.colorado.csci3155.project1


class RuntimeError(msg: String) extends Exception {
    override def toString(): String = {
        s"Error: $msg"
    }
}


object Interpreter {


    type Environment = Map[String, Value]



    /*--
        TODO: COMPLETED
     */
    def evalExpr(e: Expr, env: Environment) : Value = e match {
        case Const(v) => NumValue(v)
        case Ident(str) => {
            if (env.contains(str)){
                env(str)
            } else {
                throw new RuntimeError(s"Environment does not contain mapping for $str")
            }
        }
        case Plus(e1 , e2) =>
        {
            ValueOps.plus(evalExpr(e1,env), evalExpr(e2,env))
        }
        case Minus(e1 , e2) =>
        {
            ValueOps.minus(evalExpr(e1,env), evalExpr(e2,env))
        }
        case Mult(e1 , e2) =>
        {
            ValueOps.mult(evalExpr(e1,env), evalExpr(e2,env))
        }
        case Div(e1 , e2) =>
        {
            //catch divide by zero
            val idk2 = evalExpr(e2,env)
            idk2 match{
                case NumValue(0.0) => throw new RuntimeError("cannot divide by zero")
                case _ => ValueOps.div(evalExpr(e1,env), evalExpr(e2,env))
            }

        }
        case Geq(e1, e2) =>
        {
            ValueOps.geq(evalExpr(e1,env), evalExpr(e2,env))
        }
        case Gt(e1, e2) =>
        {
            ValueOps.gt(evalExpr(e1,env), evalExpr(e2,env))
        }
        case Eq(e1, e2) =>
        {
            ValueOps.eq(evalExpr(e1,env), evalExpr(e2,env))
        }
        case And(e1, e2) =>
        {
            val idk1 = evalExpr(e1, env)
            idk1 match{
                case BoolValue(false) => BoolValue(false)
                case BoolValue(true) => {
                    //evaluate the expression
                    val idk2 = evalExpr(e2, env)
                    if(ValueOps.isBoolean(idk2)){
                        //if expression evaluates to a boolean
                        idk2
                    } else {
                        //if it does not evaluate to a boolean
                       throw new RuntimeError("value must be boolean")
                    }
                }
                case _ => throw new RuntimeError("value must be boolean")
            }

        }
        case Or(e1, e2) =>
        {
            val idk1 = evalExpr(e1, env)
            idk1 match{
                case BoolValue(true) => BoolValue(true)
                case BoolValue(false) => {
                    //evaluate second expression
                    val idk2 = evalExpr(e2, env)
                    //determine if the expression evaluates to a boolean value
                    if(ValueOps.isBoolean(idk2)){
                        //expression evaluates to a boolean value
                        idk2
                    }else{
                        //expression does not evaluate to a boolean value
                        throw new RuntimeError("value must be boolean")
                    }

                }
                case _ => throw new RuntimeError("value must be boolean")
            }
        }
        case Not(e1) =>
        {
                val idk1 = evalExpr(e1 , env)
                idk1 match{
                    case BoolValue(true) => BoolValue(false)
                    case BoolValue(false) => BoolValue(true)
                    case _ => throw new RuntimeError("value must be boolean")
                }

        }
        case IfThenElse(e1, e2, e3) =>
        {
            val idk1 = evalExpr(e1, env)
            idk1 match {
                case BoolValue(true) => evalExpr(e2, env)
                case BoolValue(false) => evalExpr(e3, env)
                case _ => throw new RuntimeError("value must evaluate to be boolean")
            }
        }
        case _ => throw new RuntimeError("unhandled case") //unhandled case

    }

    /*--
    TODO: COMPLETED
     */
    def evalVarDefine(x: String, e: Expr, env: Environment): Environment = {
        try {
            //evaluate the expression using evalExpr
            val eval_expr = evalExpr(e, env)
            //add mapping to existing environment
            env + (x -> eval_expr)

        } catch {
            case _:RuntimeError =>  env
        }
    }

    /*-- TODO: COMPLETED --*/

    def evalCommand( env: Environment, cmd: Cmd): Environment = cmd match{
        case Define(x, e) =>
        {
                evalVarDefine(x, e, env)
        }
        case Display(e) =>
        {
            try{
                //evaluate the expression first
                val evaluated_expr = evalExpr(e,env)
                //print if error is not encountered
                println(evaluated_expr)
                env
            } catch{
                //only evaluates when error is encountered
                case _:RuntimeError => {
                    println(e)
                    env
                }
            }

        }
        case _ => throw new RuntimeError("unhandled case")
    }

    /*-- TODO: COMPLETED
     */
    def evalProgram(prog: CalcProgram, env0: Environment = Map.empty): Environment = prog match{
        case TopLevel(lst) =>
        {
            lst.foldLeft[Environment](env0)((acc: Environment, command: Cmd) => {
                evalCommand(acc, command)
            })
        }
        case _ => throw new RuntimeError("unhandled case")


    }



}
