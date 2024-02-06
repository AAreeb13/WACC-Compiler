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

    def checkVar(v: Var)(implicit currentScope: SymbolTable) = 
        currentScope.typeof(v.v).getOrElse {
            errList.addOne(s"Scope error: Variable ${v.v} has not been declared in this scope")
            NoneType
        }

    def checkPair(pairElem: PairElem)(implicit currentScope: SymbolTable): Type = ???

    def checkArray(arrayElem: ArrayVal)(implicit currentScope: SymbolTable): Type = ???


    def checkExpression(expr: Expr)(implicit currentScope: SymbolTable): Type = expr match {
        case arrayElem: ArrayVal => checkArray(arrayElem)
        case variable: Var => checkVar(variable)

        case Ord(x) =>
            matchesType(checkExpression(x), CharType)
            IntType

        case ArithmeticOp(x, y) =>
            matchesType(checkExpression(x), IntType)
            matchesType(checkExpression(y), IntType)
            IntType

        case ComparisonOp(x, y) =>
            val lhsType = checkExpression(x)
            matchesType(lhsType, List(IntType, CharType))
            matchesType(checkExpression(y), lhsType)
            BoolType

        case EqualityOp(x, y) =>
            matchesType(checkExpression(x), checkExpression(y))
            BoolType

        case LogicalOp(x, y) =>
            matchesType(checkExpression(x), BoolType)
            matchesType(checkExpression(y), BoolType)
            BoolType
        
        case Neg(x) =>
            matchesType(checkExpression(x), IntType)
            IntType

        case Not(x) =>
            matchesType(checkExpression(x), BoolType)
            BoolType
        
        case Len(x) => 
            checkExpression(x) match {
                case arr: ArrayType => IntType
                case other =>
                    errList.addOne(s"Type error: Len can only take in an array type but $other was given")
                    NoneType
            }

        case Chr(x) =>
            matchesType(checkExpression(x), IntType)
            CharType

        case BoolVal(x) => BoolType
        case CharVal(x) => CharType
        case StrVal(x) => StringType
        case PairVal => AnyType
        case IntVal(x) => IntType

        case _ => errList.addOne("unknown error"); NoneType // should not happen, metals is bugging
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
