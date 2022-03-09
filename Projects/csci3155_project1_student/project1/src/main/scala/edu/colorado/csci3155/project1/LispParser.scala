package edu.colorado.csci3155.project1
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class ParseError(msg: String) extends Exception {
    override def toString(): String = {
        s"Parse Error: $msg"
    }
}

class LispParser(orig_str: String, val tokenized_stack: ListBuffer[String] = ListBuffer(), debug: Boolean = false ) {

    val arities = Map( "+" -> 2, "-" -> 2, "*" -> 2, "/" -> 2,
                            ">" -> 2, "<" -> 2, ">=" -> 2,
                            "<=" -> 2, "==" -> 2, "&&" -> 2, "||" -> 2, "!" -> 1,
                           "if" -> 3)

    /* -- To do: parse an input string into an AST -- */

    /*-- First make sure to add some spaces around the open and close parenthesis --*/
    val str_fix = orig_str.replace("(", " ( ").replace(")", " ) ").replace("\n","")
    val split_str_fix = str_fix.split("\\s+").filter(_ != "")
    split_str_fix.foreach{ tokenized_stack.append(_) }
    if (debug){
        tokenized_stack.foreach( (x:String) => { print("-{"+ x + "}-") } )
    }
    val floatPat = "[+-]?([0-9]*[.])?[0-9]+".r
    val identPat = "[a-zA-Z][a-zA-Z0-9_]*".r
    
    def is_atom(str: String): Boolean = {
        str match {
            case floatPat(_) => true
            case identPat() => true
            case _ => false
        }
    }

    def string_to_atom(str: String): Expr = {
        str match {
            case floatPat(_) =>  Const(str.toDouble)
            case identPat() => Ident(str) 
            case _ => throw new ParseError (s"Expecting an atom (number or identifier) but got $str")
        }
    }

    def is_op(op:String): Boolean  ={
        arities.contains(op)
    }

    def get_arity(op:String) = {
        
        if (arities.contains(op)){
            arities(op)
        } else {
            throw new ParseError(s"Unknown operator: $op")
        }
    }
    def mk_un_expr(op: String, e1: Expr):Expr = {
        if (op == "!") {
            Not(e1)
        } else {
            throw new ParseError(s"Unknown Unary operator: $op")
        }
    }

    def mk_bin_expr(str: String, expr1: Expr, expr2: Expr): Expr = {
        val todo = Map(
            "+" -> (Plus(_, _)),
            "-" -> (Minus(_, _)),
            "*" -> (Mult(_, _)),
            "/" -> (Div(_, _)),
            ">" -> (Gt(_, _)),
            "<" -> ((e1, e2) => Gt(e2, e1)),
            "==" -> (Eq(_, _)),
            ">=" -> (Geq(_, _)),
            "<=" -> ((e1, e2) => Geq(e2, e1)),
            "&&" -> (And(_, _)),
            "||" -> (Or(_, _))
        )
        if (todo.contains(str)) {
            todo(str) (expr1, expr2)
        } else {
            throw new ParseError(s"Unknown binary operator: $str")
        }
    }

    def mk_ite_expr(op: String, e1: Expr, e2: Expr, e3: Expr): Expr = {
        if (op == "if") {
            IfThenElse(e1, e2, e3)
        } else {
            throw new ParseError(s"Unknown ternary operator: $op")
        }
    }

    def assert_not_eof: Unit = {
        if (tokenized_stack.length == 0){
            throw new ParseError("Unexpected end of string -- was expecting a )")
        }
    }
    def ensure_close_bracket(): Unit = {
        this.assert_not_eof
        val op = tokenized_stack.remove(0)
        if (op != ")") {
            throw new ParseError(s"Expected ) but obtained $op instead.")
        }
    }
    /*-- Parse an expression from list buffer --*/
    def parse_expression(): Expr = {
        // Keep popping the first element out of the list buffer
        this.assert_not_eof
        val hd : String  = tokenized_stack.remove(0)
        if (debug){
            println(s"Encountered: $hd")
        }
        if (is_atom(hd)){
            string_to_atom(hd)
        } else { 
            if (hd != "("){
                throw new ParseError(s"Was expecting ( but instead found $hd")
            }
            this.assert_not_eof
            val op: String = tokenized_stack.head
            if (is_op(op)){
                tokenized_stack.remove(0)
                val ar : Int = get_arity(op)
                if (ar == 1) {
                    val e1 = parse_expression()
                    ensure_close_bracket()
                    mk_un_expr(op, e1)
                } else if (ar == 2) {
                    val e1 = parse_expression()
                    val e2 = parse_expression()
                    ensure_close_bracket()
                    mk_bin_expr(op, e1, e2)
                } else if (ar == 3){
                    val e1 = parse_expression()
                    val e2 = parse_expression()
                    val e3 = parse_expression()
                    ensure_close_bracket()
                    mk_ite_expr(op, e1, e2, e3)
                } else {
                    throw new ParseError("Arity of more than three not supported")
                }
            } else if (is_atom(op)) {
                tokenized_stack.remove(0)
                val e = string_to_atom(op)
                ensure_close_bracket()
                e
            } else if (op == "(") {
                val e = parse_expression()
                ensure_close_bracket()
                e
            } else {
                throw new ParseError(s"Unexpected symbol: $op -- not an operator or rec. expression")
            }
        }          
    }

    def parse_command(): Cmd = {
        // Keep popping the first element out of the list buffer
        this.assert_not_eof
        val hd : String  = tokenized_stack.remove(0)
        if (hd != "(") {
            throw new ParseError(s"Was expecting ( but encountered $hd")
        }
        val next: String = tokenized_stack.remove(0)
        if (next == "def" || next == "define") {
            val ident = tokenized_stack.remove(0)
            val e = parse_expression()
            ensure_close_bracket()
            Define(ident, e)
        } else if (next == "disp" || next == "display") {
            val e = parse_expression() 
            ensure_close_bracket()
            Display(e)
        } else {
            throw new ParseError(s"Expecting display or define kwd. Encountered $next")
        }
    }

    @tailrec
    final def parse_list_of_commands(lstOfCommands: List[Cmd] = Nil): CalcProgram = {
        if (tokenized_stack.length <= 0){
            TopLevel(lstOfCommands.reverse)
        } else {
            val cmd = parse_command()
            parse_list_of_commands(cmd::lstOfCommands)
        }
    }
    
}
