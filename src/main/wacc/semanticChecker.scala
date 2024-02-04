package wacc

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

object semanticChecker {
    def verify(result: Either[String, Node]): Either[String, Node] = result.flatMap(_ match {
        case prog: Prog => Analyser(prog).getResult
        case _ => Left("Invalid AST type for top-level verification")
    })
}

case class Analyser(val prog: Prog) {
    var errList: ListBuffer[String] = ListBuffer.empty
    var globalTable = new SymbolTable();
    var funcTable: HashMap[String, (Type, List[Type])] = HashMap.empty

    prog.funcs.foreach { func =>
        if (funcTable.contains(func.name)) {
            errList.addOne(s"${func.name} already exists, no support for overloaded functions.")
        }
        funcTable.addOne((func.name, (func.retType, func.params.map(_.declType))))
    }

    prog.funcs.foreach(checkFunction(_))
    prog.stats.foreach(checkStatement(_, globalTable))

    def checkFunction(func: Func): Unit = {
        // create new child table
        val funcSymbolTable = new SymbolTable()

        // add parameters to the table
        func.params.foreach{ param =>
            if (!funcSymbolTable.contains(param.name)) 
                funcSymbolTable.addOne(param.name, param.declType)
            else 
                errList.addOne(s"Scope error: Parameter ${param.name} has already been declared in function")
        }
        func.stats.foreach(checkStatement(_, funcSymbolTable))
    }

    def checkStatement(stat: Stat, currentScope: SymbolTable): Unit = stat match {
        case AssignNew(t, ident, rvalue) =>
        case Assign(lvalue, rvalue) =>
        
        case While(cond, stats) => 
        case If(cond, ifStat, elseStat) =>
        case Scope(stats) =>
        
        case Return(expr) =>
        case Free(expr) =>
        case Read(lvalue) =>
        case Exit(expr) =>
        case Print(expr) =>
        case Println(expr) =>

        case Skip =>
    }

    def checkExpression(expr: Expr, currentScope: SymbolTable): Unit = expr match {
        case Var(v) =>
        case ArrayVal(v, exprs) =>

        case Ord(x) => 
        case Chr(x) =>
        case Len(x) =>
        case Not(x) =>
        case Neg(x) =>

        case Mul(x, y) =>
        case Mod(x, y) =>
        case Add(x, y) =>
        case Sub(x, y) =>
        case Div(x, y) =>
        
        case And(x, y) =>
        case Or(x, y) =>
            
        case GrtEql(x, y) =>
        case LessEql(x, y) =>
        case Less(x, y) =>
        case Grt(x, y) =>
                    
        case NotEql(x, y) =>
        case Eql(x, y) =>
        
        case StrVal(x) =>
        case BoolVal(x) =>
        case IntVal(x) =>
        case CharVal(x) =>
        case PairVal =>
    }

    def getResult: Either[String, Node] = {
        if (errList.isEmpty) {
            Right(prog)
        } else {
            Left(generateErrors)
        }
    }

    def generateErrors: String = {
        s"Semantic check failed:\n${errList.mkString("\n")}"
    }
}

class SymbolTable(val parent: Option[SymbolTable] = None) {
    var table: HashMap[String, Type] = parent match {
        case None => HashMap()
        case Some(value) => value.table.clone() // shallow copy
    }
    
    def addAll(xs: IterableOnce[(String, Type)]) = 
        table.addAll(xs)

    def addOne(name: String, declType: Type) =
        table.addOne(name, declType)

    def contains(name: String) =
        table.contains(name)
    
    def typeof(name: String) = table.get(name)

    // maybe add some flatten method so we can easily get all the info
    // in the global symbol table
}
