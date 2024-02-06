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

    def checkStatement(stat: Stat, expectedType: Type)(implicit currentScope: SymbolTable): Unit = {
        stat match {
            case While(cond, stats) => 
                matchesType(checkExpression(cond), BoolType)
                val childScope = new SymbolTable(Some(currentScope))
                stats.foreach(checkStatement(_, expectedType)(childScope))

            case Assign(lvalue, rvalue) => matchesType(checkLValue(lvalue), checkRValue(rvalue))
            case Free(expr) => checkExpression(expr) match {
                    case ArrayType(_) | AnyType | PairType(_, _) =>
                    case other => errList.addOne(s"Type error: Expected an array or pair type but got $other instead")
                }

            case Print(expr) => checkExpression(expr)

            case Exit(expr) => matchesType(checkExpression(expr), IntType)

            case Scope(stats) =>
                val childScope = new SymbolTable(Some(currentScope))
                stats.foreach(checkStatement(_, expectedType)(childScope))

            case Skip =>

            case If(cond, ifStats, elseStats) =>
                matchesType(checkExpression(cond), BoolType)
            
                val ifChild = new SymbolTable(Some(currentScope))
                ifStats.foreach(checkStatement(_, expectedType)(ifChild))
                val elseChild = new SymbolTable(Some(currentScope))
                elseStats.foreach(checkStatement(_, expectedType)(elseChild))

            case Println(expr) => checkExpression(expr)
            case Return(expr) => expectedType match {
                case NoneType => errList.addOne(s"Return placement error: Return outside function is not allowed")
                case retType => matchesType(checkExpression(expr), retType)
            }
                
            case Read(lvalue) => matchesType(checkLValue(lvalue), List[Type](IntType, CharType))

            case AssignNew(declType, ident, rvalue) =>
                matchesType(checkRValue(rvalue), declType)
                if (!currentScope.contains(ident))
                    currentScope.addOne(ident, declType)
                else
                    errList.addOne(s"Scope error: Variable $ident has already been declared in this scope")
        }
    }

    def checkLValue(lvalue: LValue)(implicit currentScope: SymbolTable): Type = lvalue match {
        case arrayElem: ArrayVal => checkArray(arrayElem)
        case pairElem: PairElem => checkPair(pairElem)
        case variable: Var => checkVar(variable)
    }
    
    def checkRValue(rvalue: RValue)(implicit currentScope: SymbolTable): Type = rvalue match {
        case ArrayLiteral(exprs) => 
            exprs.map(checkExpression(_)).fold(AnyType)((acc, expType) =>
                if  (acc reducesTo expType) expType
                else if (expType reducesTo acc) acc
                else NoneType
            )
        case pairElem: PairElem => checkPair(pairElem)
        case PairCons(fst, snd) => PairType(checkExpression(fst), checkExpression(snd))
        case FuncCall(ident, args) => funcTable.get(ident) match {
            case None =>
                errList.addOne(s"Undefined function error: Function $ident has not been defined")
                args.foreach(checkExpression(_)) // scope check only
                NoneType
            case Some((retType, paramTypes)) =>
                if (paramTypes.size != args.size) {
                    errList.addOne(s"Function call error: Wrong number of arguments provided to function $ident." +
                                   s"unexpected ${args.size}. expected ${paramTypes.size}")
                    args.foreach(checkExpression(_))
                    NoneType
                } else { 
                    args.zip(paramTypes).foreach{ case (arg, paramType) => 
                        matchesType(checkExpression(arg), paramType)
                    }
                    retType
                }
            }
        case expr: Expr => checkExpression(expr)
    }

    def checkVar(v: Var)(implicit currentScope: SymbolTable) = 
        currentScope.typeof(v.v).getOrElse {
            errList.addOne(s"Scope error: Variable ${v.v} has not been declared in this scope")
            NoneType
        }

    def checkPair(pairElem: PairElem)(implicit currentScope: SymbolTable): Type = {
        // maybe propagate PairType up instead of noneType?
        pairElem.lvalue match {
            case variable: Var => checkVar(variable) match {
                case NoneType => NoneType // inner error, do nothing
                case ErasedPair => AnyType // handle later
                case PairType(t1, t2) => pairElem match {
                    case fst: FstPair => t1
                    case snd: SndPair => t2
                }
                case other =>
                    errList.addOne(s"Type error: Variable ${variable.v} has type $other when a pair was expected")
                    NoneType
            }

            case arrayVal: ArrayVal => checkArray(arrayVal) match {
                case NoneType => NoneType // inner error, do nothing
                case ErasedPair => AnyType // handle later
                case PairType(t1, t2) => pairElem match {
                    case fst: FstPair => t1
                    case snd: SndPair => t2
                }
                case other =>
                    errList.addOne(s"Type error: Variable ${arrayVal.v} has type $other when a pair was expected")
                    NoneType
            }

            case pairElem: PairElem => checkPair(pairElem) match {
                //case innerPair: PairType => ErasedPair // pair erasure?
                //case other => other
                case PairType(_, _) | ErasedPair => ErasedPair
                case NoneType => NoneType // inner error
                case other =>
                    errList.addOne(s"Type error: Nested pair has type $other when an inner pair was expected")
                    NoneType
            }
        } 
    }

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
