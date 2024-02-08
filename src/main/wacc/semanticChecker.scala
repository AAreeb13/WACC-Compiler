package wacc

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions
import wacc.Implicits._

object semanticChecker {
    def verify(result: Either[String, Node]): Either[String, Node] = result.flatMap(_ match {
        case prog: Prog => new Analyser(prog).getResult
        case _ => Left("Invalid AST type for semantic verification")
    })
}

class Analyser(val prog: Prog) {
    var errList: ListBuffer[String] = ListBuffer.empty
    var globalTable = new SymbolTable();
    var funcTable: HashMap[String, (SemType, List[SemType])] = HashMap.empty

    checkProgram()

    def checkProgram() = {
        prog.funcs.foreach { func =>
            if (funcTable.contains(func.name)) {
                errList.addOne(s"${func.name} already exists, no support for overloaded functions.")
            }
            funcTable.addOne((func.name, (func.retType, func.params.map(_.declType))))
        }

        prog.funcs.foreach(checkFunction(_))
        prog.stats.foreach(checkStatement(_, SemNone)(globalTable))
    }

    def matchesType(actual: SemType, expected: SemType): SemType = 
        matchesType(actual, List(expected))

    def matchesType(actual: SemType, expected: List[SemType]): SemType = 
        if (expected.exists(actual reducesTo _))
            actual
        else {
            errList.addOne(s"Type error: Expected: ${expected.mkString(", ")} but received $actual instead")
            SemNone
        }

    def checkFunction(func: Func): Unit = {
        // create new child table
        val funcArgSymbolTable = new SymbolTable()

        // add parameters to the table
        func.params.foreach{ param =>
            if (!funcArgSymbolTable.contains(param.name)) 
                funcArgSymbolTable.addOne(param.name, param.declType)
            else 
                errList.addOne(s"Scope error: Parameter ${param.name} has already been declared in function")
        }
        
        val funcBodySymbolTable = new SymbolTable(Some(funcArgSymbolTable))

        func.stats.foreach(checkStatement(_, func.retType)(funcBodySymbolTable))
    }

    def checkStatement(stat: Stat, expectedType: SemType)(implicit currentScope: SymbolTable): Unit = {
        stat match {
            case While(cond, stats) => 
                matchesType(checkExpression(cond), SemBool)
                val childScope = new SymbolTable(Some(currentScope))
                stats.foreach(checkStatement(_, expectedType)(childScope))

            case Assign(lvalue, rvalue) => 
                // im really sorry this is super bad way of doing it but only 1 more test case left
                (checkLValue(lvalue), checkRValue(rvalue)) match {
                    case (SemUnknown, SemUnknown) => 
                        errList.addOne(s"Attempting to exchange values between pairs of unknown types. Pair exchange is only legal when the type of at least one of the sides is known or specified")
                    case (lvalType, rvalType) => matchesType(lvalType, rvalType)
                }

            case Free(expr) => checkExpression(expr) match {
                    case SemArray(_) | SemNull | SemPair(_, _) =>
                    case other => errList.addOne(s"Type error: Expected an array or pair type but got $other instead")
                }

            case Print(expr) => checkExpression(expr)

            case Exit(expr) => matchesType(checkExpression(expr), SemInt)

            case Scope(stats) =>
                val childScope = new SymbolTable(Some(currentScope))
                stats.foreach(checkStatement(_, expectedType)(childScope))

            case Skip =>

            case If(cond, ifStats, elseStats) =>
                matchesType(checkExpression(cond), SemBool)
            
                val ifChild = new SymbolTable(Some(currentScope))
                ifStats.foreach(checkStatement(_, expectedType)(ifChild))
                val elseChild = new SymbolTable(Some(currentScope))
                elseStats.foreach(checkStatement(_, expectedType)(elseChild))

            case Println(expr) => checkExpression(expr)
            case Return(expr) => expectedType match {
                case SemNone => errList.addOne(s"Return placement error: Return outside function is not allowed")
                case retType => matchesType(checkExpression(expr), retType)
            }
                
            case Read(lvalue) => checkLValue(lvalue) match {
                case SemUnknown => errList.addOne(s"Attempting to read from unknown type. Reading from a nested pair extraction is not legal due to pair erasure")
                case other => matchesType(checkLValue(lvalue), List[SemType](SemInt, SemChar))
            }

            case AssignNew(declType, ident, rvalue) =>
                matchesType(checkRValue(rvalue), declType)
                if (!currentScope.containsInCurrent(ident))
                    currentScope.addOne(ident, declType)
                else
                    errList.addOne(s"Scope error: Variable $ident has already been declared in this scope")
        }
    }

    def checkLValue(lvalue: LValue)(implicit currentScope: SymbolTable): SemType = lvalue match {
        case arrayElem: ArrayVal => checkArray(arrayElem)
        case pairElem: PairElem => checkPair(pairElem)
        case variable: Var => checkVar(variable)
    }
    
    def checkRValue(rvalue: RValue)(implicit currentScope: SymbolTable): SemType = rvalue match {
        case ArrayLiteral(exprs) => 
            exprs.map(checkExpression(_)).fold(SemAny) {
                case (acc, expType) if (acc reducesTo expType) => expType
                case (acc, expType) if (expType reducesTo acc) => acc
                case _ => SemNone
            } match {
                case SemNone => SemNone
                case other => SemArray(other)
            }
            
        case pairElem: PairElem => checkPair(pairElem)
        case PairCons(fst, snd) => 
            val fstType = checkExpression(fst) match {
                case SemPair(_, _) => SemErasedPair
                case other => other
            }

            val sndType = checkExpression(snd) match {
                case SemPair(_, _) => SemErasedPair
                case other => other
            }
            SemPair(fstType, sndType)

        case FuncCall(ident, args) => funcTable.get(ident) match {
            case None =>
                errList.addOne(s"Undefined function error: Function $ident has not been defined")
                args.foreach(checkExpression(_)) // scope check only
                SemNone
            case Some((retType, paramTypes)) =>
                if (paramTypes.size != args.size) {
                    errList.addOne(s"Function call error: Wrong number of arguments provided to function $ident." +
                                   s"unexpected ${args.size}. expected ${paramTypes.size}")
                    args.foreach(checkExpression(_))
                    SemNone
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
            SemNone
        }

    def checkPair(pairElem: PairElem)(implicit currentScope: SymbolTable): SemType = {
        // maybe propagate SemPair up instead of SemNone?
        pairElem.lvalue match {
            case variable: Var => checkVar(variable) match {
                case SemNone => SemNone // inner error, do nothing
                //case SemErasedPair => SemAny // shouldn't happen since variables always have full type info
                case SemPair(t1, t2) => pairElem match {
                    case fst: FstPair => t1
                    case snd: SndPair => t2
                }
                case other =>
                    errList.addOne(s"Type error: Variable ${variable.v} has type $other when a pair was expected")
                    SemNone
            }

            case arrayVal: ArrayVal => checkArray(arrayVal) match {
                case SemNone => SemNone // inner error, do nothing
                //case SemErasedPair => SemAny // again shouldn't happen
                case SemPair(t1, t2) => pairElem match {
                    case fst: FstPair => t1
                    case snd: SndPair => t2
                }
                case other =>
                    errList.addOne(s"Type error: Variable ${arrayVal.v} has type $other when a pair was expected")
                    SemNone
            }

            case pairElem: PairElem => checkPair(pairElem) match {
                //case innerPair: SemPair => SemErasedPair // pair erasure?
                //case other => other
                case SemPair(_, _) => SemErasedPair
                case SemErasedPair => SemUnknown
                case SemNone => SemNone // inner error
                case other =>
                    errList.addOne(s"Type error: Nested pair has type $other when an inner pair was expected")
                    SemNone
            }
        } 
    }

    def checkArray(arrayElem: ArrayVal)(implicit currentScope: SymbolTable): SemType = {
            arrayElem.exprs.foreach(e => matchesType(checkExpression(e), SemInt))
            currentScope.typeof(arrayElem.v) match {
                case None => 
                    errList.addOne(s"Scope error: Variable ${arrayElem.v} has not been declared in this scope")
                    SemNone
                case Some(declType) => declType match {
                    case arrType: SemArray => arrType.unfold(arrayElem.exprs.size) match {
                        case None =>
                            errList.addOne(s"Type error: array ${arrayElem.v} has type $arrType with dimension ${arrType.dimensions} but dimenesion ${arrayElem.exprs.size} was provided")
                            SemNone
                        case Some(innerType) =>
                            innerType
                    }
                    case other => 
                        errList.addOne(s"Type error: Variable ${arrayElem.v} has type $other when an array was expected")
                        SemNone
                }
            }
    }

    def checkExpression(expr: Expr)(implicit currentScope: SymbolTable): SemType = expr match {
        case arrayElem: ArrayVal => checkArray(arrayElem)
        case variable: Var => checkVar(variable)

        case Ord(x) =>
            matchesType(checkExpression(x), SemChar)
            SemInt

        case ArithmeticOp(x, y) =>
            matchesType(checkExpression(x), SemInt)
            matchesType(checkExpression(y), SemInt)
            SemInt

        case ComparisonOp(x, y) =>
            val lhsType = checkExpression(x)
            matchesType(lhsType, List(SemInt, SemChar))
            matchesType(checkExpression(y), lhsType)
            SemBool

        case EqualityOp(x, y) =>
            matchesType(checkExpression(x), checkExpression(y))
            SemBool

        case LogicalOp(x, y) =>
            matchesType(checkExpression(x), SemBool)
            matchesType(checkExpression(y), SemBool)
            SemBool
        
        case Neg(x) =>
            matchesType(checkExpression(x), SemInt)
            SemInt

        case Not(x) =>
            matchesType(checkExpression(x), SemBool)
            SemBool
        
        case Len(x) => 
            checkExpression(x) match {
                case arr: SemArray => SemInt
                case other =>
                    errList.addOne(s"Type error: Len can only take in an array type but $other was given")
                    SemNone
            }

        case Chr(x) =>
            matchesType(checkExpression(x), SemInt)
            SemChar

        case BoolVal(x) => SemBool
        case CharVal(x) => SemChar
        case StrVal(x) => SemString
        case PairVal => SemNull
        case IntVal(x) => SemInt

        case _ => errList.addOne("unknown error"); SemNone // should not happen, metals is bugging
    }

    def getResult: Either[String, Node] = {
        //Right(prog) // :)
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
    var table: HashMap[String, SemType] = HashMap()

    def addAll(xs: IterableOnce[(String, SemType)]) =
        table.addAll(xs)

    def addOne(name: String, declType: SemType): Unit =
        table.addOne(name, declType)

    def contains(name: String): Boolean =
        table.contains(name) || parent.exists(_.contains(name))

    def containsInCurrent(name: String): Boolean =
        table.contains(name)

    def typeof(name: String): Option[SemType] = table
        .get(name)
        .orElse(parent.flatMap(_.typeof(name)))

    // maybe add some flatten method so we can easily get all the info
    // in the global symbol table
}
