package wacc

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

import implicits._
import ast._
import semAst._

/**
  * Takes an AST and optionally a semanticError collector and performs semantic analysis on it
  */
object semanticChecker {
    /**
      * 
      *
      * @param result The AST that should be tested. Should be a syntactically valid AST with Prog as the top-level node
      * @param errorCollector Semantic error builder that contains file info if passed.
      * @return Either a success if semantically valid or errors as string otherwise
      */
    def verify(result: Either[String, Node], errorCollector: Option[SemanticErrorCollector] = None): Either[String, SemanticInfo] = result.flatMap(_ match {
        case prog: Prog => new Analyser(prog, errorCollector).getResult
        case _ => Left("Invalid AST type for semantic verification")
    })

    // Function Table 
    //      := (return type, parameter types, node itself)
    //type FuncInfo = (SemType, List[SemType], Func)
    case class FuncInfo(returnType: SemType, paramTypes: List[SemType], func: Func)
    type FuncTable = HashMap[String, FuncInfo]

    /**
     * The class that performs semantic analysis on each node in the AST
     *
     * @param prog The AST itself
     * @param errorCollectorOption The error collector from verify. Creates a new one if not provided
     */
    class Analyser(val prog: Prog, errorCollectorOption: Option[SemanticErrorCollector]) {
        // Provides formatted errors, a new instance is created if one doesn't exist
        val errorCollector = errorCollectorOption.getOrElse(new SemanticErrorCollector)

        // Global symbol table
        var mainScope = new SymbolTable()
        var topLevelScopes: ListBuffer[SymbolTable] = ListBuffer(mainScope)
        var funcTable: FuncTable = HashMap.empty

        checkProgram()

        /**
         * Additional Constructor which generates function map and adds to statements to global symbol table
         */
        def checkProgram(): Unit = {
            prog.scope = mainScope
            // Maps each function to an entry in funcTable
            prog.funcs.foreach { func =>
                // Throw an error if two function with same names exist
                if (funcTable.contains(func.name)) {
                    val prevFunc = funcTable.get(func.name).get.func
                    errorCollector.addError(func, RedefinedFuncError(func, Some(prevFunc)), "No support for overloaded functions")
                }
                // Add latest entry to table
                funcTable.addOne((func.name, FuncInfo(func.retType, func.params.map(_.declType), func)))
            }

            // Checks each statment in global scope and also in each statement
            prog.funcs.foreach(checkFunction(_))
            prog.stats.foreach(checkStatement(_, SemNone)(mainScope))
        }

        /**
         * Matches type given type with expected and checks for a "reduction"
         * i.e. if the expected can be reduced to the actual type
         * This method matches against a single type
         */
        def matchesType(actual: SemType, expected: SemType)(implicit node: Node): SemType = 
            matchesType(actual, List(expected))

        /**
         * Matches the actual against a range of types
         * An implicit node is taken for error information
         */
        def matchesType(actual: SemType, expected: List[SemType])(implicit node: Node): SemType = 
            if (expected.exists(actual reducesTo _))
                actual
            else {
                if (actual != SemNone) {
                    errorCollector.addError(node, TypeError(actual.toString, expected.mkString(", ")))
                }
                SemNone
            }

        /**
         * Semantic checking for a function
         */
        def checkFunction(func: Func): Unit = {
            // A new symbol table for the function is created and added to the list
            val funcArgSymbolTable = new SymbolTable()
            func.scope = funcArgSymbolTable
            topLevelScopes.addOne(funcArgSymbolTable)


            // Parameters are added to the table, also making sure duplicates are not created
            func.params.foreach{ param =>
                if (!funcArgSymbolTable.contains(param.name)) 
                    funcArgSymbolTable.addOne(param.name, param.declType)(param)
                else {
                    errorCollector.addError(param, RedeclaredVarError(param, funcArgSymbolTable.nodeof(param.name)))
                }
            }
            
            // Function body can declare new variables with same name as args
            // so a new scope is needed
            val funcBodySymbolTable = new SymbolTable(Some(funcArgSymbolTable))

            // Each statement in the function is semantically checked
            // The return type is passed in so that the "Return" statment can match against its expected type
            func.stats.foreach(checkStatement(_, func.retType)(funcBodySymbolTable))
        }

        /**
         * Semantic verification for statements
         *
         * @param stat Statement that is to be checked
         * @param expectedType Expected return type
         * @param currentScope Symbol Table for current scope
         */
        def checkStatement(stat: Stat, expectedType: SemType)(implicit currentScope: SymbolTable): Unit = {
            // To pass implicitly into error statements
            implicit val node: Node = stat
            stat match {
                case While(cond, stats) => 
                    // Checks that condition is a boolean type
                    matchesType(checkExpression(cond), SemBool)(cond)
                    val childScope = new SymbolTable(Some(currentScope))
                    stats.foreach(checkStatement(_, expectedType)(childScope))

                case Assign(lvalue, rvalue) => 
                    // Checks if rvalue can be reduced to the lvalue
                    (checkLValue(lvalue), checkRValue(rvalue)) match {
                        // Although matchesType handles this, we want to provide explicity specialised error
                        case (SemUnknown, SemUnknown) => 
                            errorCollector.addError(stat, SpecialError("Type Error", "Attempting to exchange values between pairs of unknown types\n" +
                            "Pair exchange is only legal when the type of at least one of the sides is known or specified"))
                        case (lvalType, rvalType) => matchesType(lvalType, rvalType)
                    }

                case f@Free(expr) => checkExpression(expr) match {
                        // Expression must be any of "null", array or pair
                        case SemArray(_) => f.isArray = true
                        case SemNull | SemPair(_, _) =>
                        case other =>
                            errorCollector.addError(expr, TypeError(s"$other", "1-dimensional array or pair type"))
                    }

                case p@Print(expr) => p.enclosingType = checkExpression(expr)
                case p@Println(expr) => p.enclosingType = checkExpression(expr)
                case Skip() => // do nothing

                // Exit must be an integer
                case Exit(expr) => matchesType(checkExpression(expr), SemInt)(expr)

                case Scope(stats) =>
                    val childScope = new SymbolTable(Some(currentScope))
                    stats.foreach(checkStatement(_, expectedType)(childScope))

                

                case If(cond, ifStats, elseStats) =>
                    matchesType(checkExpression(cond), SemBool)
                
                    val ifChild = new SymbolTable(Some(currentScope))
                    ifStats.foreach(checkStatement(_, expectedType)(ifChild))

                    val elseChild = new SymbolTable(Some(currentScope))
                    elseStats.foreach(checkStatement(_, expectedType)(elseChild))

                case Return(expr) => expectedType match {
                    case SemNone => 
                        // If no expected type is provided we know that it cannot have been in a function
                        errorCollector.addError(stat, SpecialError("Return placement error", "Return outside function is not allowed" ))
                    case retType => matchesType(checkExpression(expr), retType)
                }
                    
                case r@Read(lvalue) => checkLValue(lvalue) match {
                    case SemUnknown => 
                        errorCollector.addError(lvalue, SpecialError("Type Error", "Attempting to read from unknown type. Reading from a nested pair extraction is not legal due to pair erasure"))
                    case other => r.enclosingType = matchesType(checkLValue(lvalue), List[SemType](SemInt, SemChar))
                }

                case assignNew@AssignNew(declType, ident, rvalue) =>
                    // Checks for type matching and also that it is not already contained in current scope of symbol table
                    matchesType(checkRValue(rvalue), declType)(rvalue)
                    //if (rvalue.isInstanceOf[ArrayLiteral]) rvalue.asInstanceOf[ArrayLiteral].enclosingType = declType  // I DONT THINK THIS IS A GOOD WAY TO DO IT
                    if (!currentScope.containsInCurrent(ident))
                        currentScope.addOne(ident, declType)
                    else {
                        errorCollector.addError(stat, RedeclaredVarError(assignNew, currentScope.nodeof(ident)))
                    }
            }
        }

        /**
         * Semantic checks for lvalues
         * @return The type of the lvalue (SemNone if it doesn't exist)
         */
        def checkLValue(lvalue: LValue)(implicit currentScope: SymbolTable): SemType = lvalue match {
            case arrayElem: ArrayVal => checkArray(arrayElem)
            case pairElem: PairElem => checkPair(pairElem)
            case variable: Var => checkVar(variable)
        }
        
        /**
         * Semantic checks for rvalues
         * @return The type of the lvalue (SemNone if it doesn't exist)
         */
        def checkRValue(rvalue: RValue)(implicit currentScope: SymbolTable): SemType = {
            implicit val node: Node = rvalue
            rvalue match {
                case arrayLit@ArrayLiteral(exprs) => 
                    // Semantic check each expression then perform a fold to gain type ancestor
                    exprs.map(checkExpression(_)).fold(SemAny) {
                        case (acc, expType) if (acc reducesTo expType) => expType
                        case (acc, expType) if (expType reducesTo acc) => acc
                        case (acc, other) if (acc != SemNone) => 
                            errorCollector.addError(rvalue, TypeError(other.toString, acc.toString))
                            SemNone
                        case _ => SemNone
                    } match {
                        // In case of an error, return SemNone to signify otherwise return array type
                        case SemNone => SemNone
                        case other =>
                            arrayLit.enclosingType = other
                            SemArray(other)
                    }
                    
                case pairElem: PairElem => checkPair(pairElem)

                case PairCons(fst, snd) =>
                    // If a nested pair type is made, pair erasure is performed (may not be best idea)
                    val fstType = checkExpression(fst) match {
                        case SemPair(_, _) => SemErasedPair
                        case other => other
                    }

                    val sndType = checkExpression(snd) match {
                        case SemPair(_, _) => SemErasedPair
                        case other => other
                    }
                    SemPair(fstType, sndType)

                case funcCall@FuncCall(ident, args) => funcTable.get(ident) match {
                    case None =>
                        // If function doesn't exist then create error and perform scope check on arguments
                        errorCollector.addError(rvalue, UndefinedFuncError(ident))
                        args.foreach(checkExpression(_))
                        SemNone
                    case Some(funcInfo@FuncInfo(retType, paramTypes, func)) =>
                        // Check that argument size is the same
                        if (paramTypes.size != args.size) {
                            errorCollector.addError(rvalue, FuncArgumentSizeError(ident, args.size, paramTypes.size))
                            args.foreach(checkExpression(_))
                            SemNone
                        } else { 
                            // Check that types match for the arguments and parameters
                            args.zip(paramTypes).foreach{ case (arg, paramType) => 
                                matchesType(checkExpression(arg), paramType)
                            }
                            funcCall.funcInfo = funcInfo
                            retType
                        }
                    }

                case expr: Expr => checkExpression(expr)
            }
        }
        /**
         * Semantic checks for variables
         * @return The type of the variable (SemNone if it doesn't exist)
         */
        def checkVar(v: Var)(implicit currentScope: SymbolTable) = 
            // Return the type of the variable if it exists, otherwise throw an error
            currentScope.typeof(v.v).getOrElse {
                errorCollector.addError(v, UndeclaredVarError(v))
                SemNone
            }
        
        /**
         * Semantic checks for pair elems
         * @return The type of the pair elem (SemNone if it doesn't exist)
         */
        def checkPair(pairElem: PairElem)(implicit currentScope: SymbolTable): SemType = {
            // maybe propagate SemPair up instead of SemNone?
            pairElem.lvalue match {
                case pairElem: PairElem => checkPair(pairElem) match {
                    // Checks that a nested pair elem matches the type requirements
                    case SemPair(_, _) => SemErasedPair // this probably shouldn't happen either
                    case SemErasedPair => SemUnknown
                    case SemUnknown => SemUnknown
                    case SemNone => SemNone // inner error
                    case other =>
                        errorCollector.addError(pairElem, TypeError(other.toString, "an inner pair"))
                        SemNone
                }

                case nonPairElem => {
                    // Get the type of the lvalue
                    val innerType = nonPairElem match {
                        case variable: Var => checkVar(variable)
                        case arrayVal: ArrayVal => checkArray(arrayVal)
                        case _ => ??? // shutup metals
                    }
                    
                    // Check that the type of the inner expression is a pair, otherwise report an error
                    innerType match {
                        case SemNone => SemNone // inner error, do nothing
                        //case SemErasedPair => SemAny // shouldn't happen since variables always have full type info
                        case SemPair(t1, t2) => pairElem match {
                            case fst: FstPair => t1
                            case snd: SndPair => t2
                        }
                        case other =>
                            errorCollector.addError(pairElem, TypeError(other.toString, "a pair"))
                            SemNone
                    }
                }
            } 
        }

        /**
         * Semantic checks for array elems
         * @return The type of the array elem (SemNone if it doesn't exist)
         */
        def checkArray(arrayElem: ArrayVal)(implicit currentScope: SymbolTable): SemType = {
            // Checks each array element index has overall type integer
            arrayElem.exprs.foreach(e => matchesType(checkExpression(e), SemInt)(e))
            currentScope.typeof(arrayElem.v) match {
                case None => 
                    errorCollector.addError(arrayElem, UndeclaredVarError(arrayElem))
                    SemNone
                case Some(declType) => declType match { // how to do efficient arity check? calling dimensions before would still do the same, does it need to be stored?
                    // Check provided dimensions are not greater than that of the actual array type (using arrayType.unfold)
                    case arrType: SemArray => arrType.unfold(arrayElem.exprs.size) match {
                        case None =>
                            errorCollector.addError(arrayElem, TypeError(s"dimenesion ${arrayElem.exprs.size}", s"dimension ${arrType.dimensions}"))
                            SemNone
                        case Some(innerType) =>
                            arrayElem.enclosingType = innerType
                            innerType
                    }
                    case other => 
                        // Check that the variable is actually of type array
                        errorCollector.addError(arrayElem, TypeError(other.toString, "array type"))
                        SemNone
                }
            }
        }

        /**
         * Semantic checks for expressions
         * @return The type of the expression (SemNone if it doesn't exist)
         */
        def checkExpression(expr: Expr)(implicit currentScope: SymbolTable): SemType = {
            implicit val node: Node = expr
            expr match {
                case arrayElem: ArrayVal => checkArray(arrayElem)
                case variable: Var => checkVar(variable)

                case Ord(x) =>
                    // Takes in a char and returns an int
                    matchesType(checkExpression(x), SemChar)
                    SemInt

                case ArithmeticOp(x, y) =>
                    // Takes in two ints and returns an int
                    matchesType(checkExpression(x), SemInt)
                    matchesType(checkExpression(y), SemInt)
                    SemInt

                case ComparisonOp(x, y) =>
                    // Takes in either a char or int and returns bool
                    val lhsType = checkExpression(x)
                    matchesType(lhsType, List(SemInt, SemChar))
                    // rhsType must match lhs
                    matchesType(checkExpression(y), lhsType)
                    SemBool

                case EqualityOp(x, y) => (checkExpression(x), checkExpression(y)) match {
                    // mainly for case where a is char[] and b is string and vice versa etc.
                    case (a, b) if ((a reducesTo b) || (b reducesTo a))  =>
                    case (a, b) => matchesType(a, b) // technically redundant but does error stuff for us
                }
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
                    // Takes in an array and returns int
                    checkExpression(x) match {
                        case arr: SemArray => SemInt
                        case other =>
                            errorCollector.addError(expr, TypeError(other.toString, "array type"))
                            SemNone
                    }

                case Chr(x) =>
                    matchesType(checkExpression(x), SemInt)
                    SemChar

                case BoolVal(x) => SemBool
                case CharVal(x) => SemChar
                case StrVal(x) => SemString
                case PairVal() => SemNull
                case IntVal(x) => SemInt

                case _ => SemNone // should not happen, metals is bugging
            }
        }

        def getResult: Either[String, SemanticInfo] = {
            if (errorCollector.containsError) {
                Right(SemanticInfo(prog, topLevelScopes.toList, funcTable))
            } else {
                Left(errorCollector.formatErrors)
            }
        }
    }

    case class SemanticInfo(ast: Prog, symbolTables: List[SymbolTable], funcTable: FuncTable)
}

