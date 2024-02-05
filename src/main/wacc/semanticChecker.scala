package wacc

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

object semanticChecker {
    def verify(result: Either[String, Node]): Either[String, Node] = result.flatMap(_ match {
        case prog: Prog => Analyser(prog).getResult
        case _          => Left("Invalid AST type for top-level verification")
    })
}

case class Analyser(val prog: Prog) {
    var errList: ListBuffer[String]                    = ListBuffer.empty
    var globalTable                                    = new SymbolTable();
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
        func.params.foreach { param =>
            if (!funcSymbolTable.contains(param.name))
                funcSymbolTable.addOne(param.name, param.declType)
            else
                errList.addOne(s"Scope error: Parameter ${param.name} has already been declared in function")
        }
        func.stats.foreach(checkStatement(_, funcSymbolTable))
    }

    def checkStatement(stat: Stat, currentScope: SymbolTable): Unit = stat match {
        case AssignNew(t, ident, rvalue) =>
        case Assign(lvalue, rvalue)      =>

        case While(cond, stats)         =>
        case If(cond, ifStat, elseStat) =>
        case Scope(stats)               =>

        case Return(expr)  =>
        case Free(expr)    =>
        case Read(lvalue)  =>
        case Exit(expr)    =>
        case Print(expr)   =>
        case Println(expr) =>

        case Skip =>
    }

    def checkExpression(expr: Expr, currentScope: SymbolTable): Option[Type] = expr match {
        case Var(v) =>
            currentScope.typeof(v) match {
                case None =>
                    errList.addOne(s"Variable ${v} is not in scope")
                    None
                case x => x
            }

        case ArrayVal(v, exprs) => ???

        case Ord(x) => checkUnaryExpression(x, CharType, IntType, currentScope)

        case Chr(x) => checkUnaryExpression(x, IntType, CharType, currentScope)

        case Len(x) => ???

        case Not(x) => checkUnaryExpression(x, BoolType, BoolType, currentScope)

        case Neg(x) => checkUnaryExpression(x, IntType, IntType, currentScope)

        case Mul(x, y) => checkBinExpression(x, y, IntType, currentScope)
        case Mod(x, y) => checkBinExpression(x, y, IntType, currentScope)
        case Add(x, y) => checkBinExpression(x, y, IntType, currentScope)
        case Sub(x, y) => checkBinExpression(x, y, IntType, currentScope)
        case Div(x, y) => checkBinExpression(x, y, IntType, currentScope)

        case And(x, y) => checkBinExpression(x, y, BoolType, currentScope)
        case Or(x, y)  => checkBinExpression(x, y, BoolType, currentScope)

        case GrtEql(x, y)  => checkInequality(x, y, currentScope)
        case LessEql(x, y) => checkInequality(x, y, currentScope)
        case Less(x, y)    => checkInequality(x, y, currentScope)
        case Grt(x, y)     => checkInequality(x, y, currentScope)

        case NotEql(x, y) => ???
        case Eql(x, y)    => ???

        case StrVal(x)  => Some(StringType)
        case BoolVal(x) => Some(BoolType)
        case IntVal(x)  => Some(IntType)
        case CharVal(x) => Some(CharType)
        case PairVal    => ???
    }

    private def checkBinExpression(x: Expr, y: Expr, expectedType: Type, currentScope: SymbolTable): Option[Type] = {
        def checkRightExpression(y: Expr, expectedType: Type, currentScope: SymbolTable): Option[Type] = checkExpression(y, currentScope) match {
            case Some(expectedType) => Some(expectedType)
            case None               => Some(expectedType)
            case otherwise =>
              errList.addOne(s"Expected type: ${typeFinder(expectedType)}, found ${typeFinder(otherwise.get)}")
              Some(expectedType)
        }

        checkExpression(x, currentScope) match {
            case Some(expectedType) => checkRightExpression(y, expectedType, currentScope)
            case None => checkRightExpression(y, expectedType, currentScope)
            case otherwise =>
              errList.addOne(s"Expected type: ${typeFinder(expectedType)}, found ${typeFinder(otherwise.get)}")
              checkRightExpression(y, expectedType, currentScope)
        }
    }

    private def checkUnaryExpression(x: Expr, expectedType: Type, returnType: Type, currentScope: SymbolTable) = checkExpression(x, currentScope) match {
      case Some(expectedType) => Some(returnType)
      case None => Some(returnType)
      case otherwise => errList.addOne(s"Expected type: ${typeFinder(expectedType)}, found ${typeFinder(otherwise.get)}")
        Some(returnType)
    }

    private def checkInequality(x: Expr, y: Expr, currentScope: SymbolTable): Option[Type] = {
      def checkRightExpression(y: Expr, currentScope: SymbolTable): Option[Type] = checkExpression(y, currentScope) match {
        case Some(IntType) | Some(CharType) => Some(BoolType)
        case None => Some(BoolType)
        case otherwise => errList.addOne(s"Expected type: char or int, found ${typeFinder(otherwise.get)}")
          Some(BoolType) 
      }
      
      checkExpression(x, currentScope) match {
        case Some(IntType) | Some(CharType) => checkRightExpression(y, currentScope)
        case None => checkRightExpression(y, currentScope)
        case otherwise => errList.addOne(s"Expected type: char or int, found ${typeFinder(otherwise.get)}")
          checkRightExpression(y, currentScope)
      }
    }

    private def typeFinder(givenType: Type) = givenType match {
      case IntType => "int"
      case BoolType => "bool"
      case CharType => "char"
      case StringType => "string"
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
    var table: HashMap[String, Type] = HashMap()

    def addAll(xs: IterableOnce[(String, Type)]) =
        table.addAll(xs)

    def addOne(name: String, declType: Type): Unit =
        table.addOne(name, declType)

    def contains(name: String): Boolean =
        table.contains(name) || parent.exists(_.contains(name))

    def typeof(name: String): Option[Type] = table
        .get(name)
        .orElse(parent.flatMap(_.typeof(name)))

    // maybe add some flatten method so we can easily get all the info
    // in the global symbol table
}
