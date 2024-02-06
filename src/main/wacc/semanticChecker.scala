package wacc

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions


object semanticChecker {
    def verify(result: Either[String, Node]): Either[String, Node] = result.flatMap(_ match {
        case prog: Prog => new Analyser(prog).getResult
        case _ => Left("Invalid AST type for semantic verification")
    })
}

class Analyser(val prog: Prog) {
    var errList: ListBuffer[String] = ListBuffer.empty
    var globalTable = new SymbolTable();
    var funcTable: HashMap[String, (Type, List[Type])] = HashMap.empty


    checkProgram()

    def checkProgram() = {
        prog.funcs.foreach { func =>
            if (funcTable.contains(func.name)) {
                errList.addOne(s"${func.name} already exists, no support for overloaded functions.")
            }
            funcTable.addOne((func.name, (func.retType, func.params.map(_.declType))))
        }

        prog.funcs.foreach(checkFunction(_))
        prog.stats.foreach(checkStatement(_, NoneType)(globalTable))
    }

    def matchesType(actual: Type, expected: Type): Type = 
        matchesType(actual, List(expected))

    def matchesType(actual: Type, expected: List[Type]): Type = 
        if (expected.exists(actual reducesTo _))
            actual
        else {
            errList.addOne(s"Type error: Expected: ${expected.mkString(", ")} but received $actual instead")
            NoneType
        }

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
        func.stats.foreach(checkStatement(_, func.retType)(funcSymbolTable))
    }

    def checkStatement(stat: Stat, expectedType: Type)(implicit currentScope: SymbolTable): Unit = stat match {
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

    def checkExpression(expr: Expr)(implicit currentScope: SymbolTable): Option[Type] = expr match {
        case Var(v) =>
            currentScope.typeof(v) match {
                case None =>
                    errList.addOne(s"Variable ${v} is not in scope")
                    None
                case x => x
            }

        case ArrayVal(v, exprs) => ???

        case Ord(x) => checkUnaryExpression(x, CharType, IntType)

        case Chr(x) => checkUnaryExpression(x, IntType, CharType)

        case Len(x) => ???

        case Not(x) => checkUnaryExpression(x, BoolType, BoolType)

        case Neg(x) => checkUnaryExpression(x, IntType, IntType)

        case LogicalOp(x, y) => checkBinExpression(x, y, BoolType)

        case ComparisonOp(x, y) => checkInequality(x, y)

        case ArithmeticOp(x, y) => checkBinExpression(x, y, IntType)


        case EqualityOp(x, y) => ???

        case StrVal(x)  => Some(StringType)
        case BoolVal(x) => Some(BoolType)
        case IntVal(x)  => Some(IntType)
        case CharVal(x) => Some(CharType)
        case PairVal    => Some(AnyType)

        case _ => None // should not happen, metals is bugging
    }

    def checkRightExpression(y: Expr, expectedType: Type)(implicit currentScope: SymbolTable): Option[Type] = 
        checkExpression(y) match {
            case Some(expectedType) => Some(expectedType)
            case None               => Some(expectedType)
            case otherwise =>
                errList.addOne(s"Expected type: $expectedType}, found ${otherwise.get}")
                Some(expectedType)
        }

    private def checkBinExpression(x: Expr, y: Expr, expectedType: Type)(implicit currentScope: SymbolTable): Option[Type] = {


        checkExpression(x) match {
            case Some(expectedType) => checkRightExpression(y, expectedType)
            case None => checkRightExpression(y, expectedType)
            case otherwise =>
              errList.addOne(s"Expected type: $expectedType, found ${otherwise.get}")
              checkRightExpression(y, expectedType)
        }
    }

    private def checkUnaryExpression(x: Expr, expectedType: Type, returnType: Type)(implicit currentScope: SymbolTable) = checkExpression(x) match {
      case Some(expectedType) => Some(returnType)
      case None => Some(returnType)
      case otherwise => errList.addOne(s"Expected type: $expectedType, found ${otherwise.get}")
        Some(returnType)
    }

    def checkRightExpression(y: Expr)(implicit currentScope: SymbolTable): Option[Type] = checkExpression(y) match {
        case Some(IntType) | Some(CharType) => Some(BoolType)
        case None => Some(BoolType)
        case otherwise => errList.addOne(s"Expected type: char or int, found ${otherwise.get}")
            Some(BoolType) 
    }

    private def checkInequality(x: Expr, y: Expr)(implicit currentScope: SymbolTable): Option[Type] = {

      
      checkExpression(x) match {
        case Some(IntType) | Some(CharType) => checkRightExpression(y)
        case None => checkRightExpression(y)
        case otherwise => errList.addOne(s"Expected type: char or int, found ${otherwise.get}")
          checkRightExpression(y)
      }
    }

    def getResult: Either[String, Node] = {
        Right(prog) // :)
        // if (errList.isEmpty) {
        //     Right(prog)
        // } else {
        //     Left(generateErrors)
        // }
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
