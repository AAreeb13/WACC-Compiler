package wacc.unit.errors

import wacc.astFactory._
import wacc._
import wacc.Implicits._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.Inside._

class SemanticErrorTest extends AnyFlatSpec {

    /*
    begin
        while (1) do
            skip
        done
    end
    */
    "Type errors" should "match the expected type" in {
        val exampleAST = Prog(List(),List(While(IntVal(1),List(Skip))))
        inside(semanticChecker.verify(Right(exampleAST))) {
            case Left(err) => err should include (TypeError(SemInt, SemBool).formatError)
        }
    }

    /*
    begin
        int x = 1;
        int x = 2
    end
    */
    "Scope Errors" should "catch redeclared variable errors" in {
        val exampleAST = Prog(List(),List(AssignNew(IntType,"x",IntVal(1)), AssignNew(IntType,"x",IntVal(2))))
        inside(semanticChecker.verify(Right(exampleAST))) {
            case Left(err) => err should include (RedeclaredVarError(IntType, Some(IntType)).formatError)
        }
    }

    /*
    begin
        int x = 1 ;
        y = 2
    end 
   */   
    it should "catch undeclared variable errors" in {
        val exampleAST = Prog(List(),List(AssignNew(IntType,"x",IntVal(1)), Assign(Var("y"),IntVal(2))))
        inside(semanticChecker.verify(Right(exampleAST))) {
            case Left(err) => err should include (UndeclaredVarError(Var("y")).formatError)
        }
    }

    /*
    begin
        int x = call f()
    end
   */
    "FunctionError" should "catch undefined function errors" in {
        val exampleAST = Prog(List(),List(AssignNew(IntType,"x",FuncCall("f",List()))))
        inside (semanticChecker.verify(Right(exampleAST))) {
            case Left(err) => err should include (UndefinedFuncError("f").formatError)
        }
    }
    /*
    begin
        int f(int x) is
            return 0
        end
        int f(int x) is
            return 1
        end
        skip
    end
    */
    it should "catch redefinitions of functions" in {
        val exampleAST = Prog(List(Func(IntType,"f",List(Param(IntType,"x")),List(Return(IntVal(0)))),
                                Func(IntType,"f",List(Param(IntType,"x")),List(Return(IntVal(1))))),
                                    List(Skip))
        inside (semanticChecker.verify(Right(exampleAST))) {
            case Left(err) => err should include (RedefinedFuncError(Func(IntType, "f", List(Param(IntType,"x")),
             List(Return(IntVal(0)))), 
             Some(Func(IntType,"f",List(Param(IntType,"x")),
             List(Return(IntVal(1)))))).formatError)
        }
    }

    /*
    begin
        int f(int x) is
            return 0
        end
        int x = call f() ;
        skip
    end
    */
    it should "catch calls to functions with incorrect number of arguments" in {
        val exampleAST = Prog(List(Func(IntType,"f",List(Param(IntType,"x")),List(Return(IntVal(0))))),
                                List(AssignNew(IntType,"x",FuncCall("f",List())),
                                    Skip))
        inside (semanticChecker.verify(Right(exampleAST))) {
            case Left(err) => err should include (FuncArgumentSizeError("f", 0, 1).formatError)
        }
    }

    /*
    begin
        int f(int x) is
            return 0
        end
        return 'c' ;
        skip
    end
    */
    "Specialised Errors" should "catch return statements outside functions" in {
        val exampleAST = Prog(List(Func(IntType,"f",List(Param(IntType,"x")),List(Return(IntVal(0))))),
                            List(Return(CharVal('c')),
                                 Skip))

        inside (semanticChecker.verify(Right(exampleAST))) {
            case Left(err) => err should include (SpecialError("Return placement error", "Return outside function is not allowed" ).formatError)
        }
    }

    /*
    begin
        int f(int x) is
            return 0
        end
        pair(pair, int) x = newpair (null, 3) ;
        read fst fst x ;
        skip
    end
    */
    it should "catch reading from unknown types" in {
        val exampleAST = Prog(List(),
                                List(AssignNew(PairType(ErasedPair,IntType),"x",PairCons(PairVal,IntVal(3))),
                                    Read(FstPair(FstPair(Var("x")))),
                                        Skip))
        inside (semanticChecker.verify(Right(exampleAST))) {
            case Left(err) => err should include (SpecialError("Type Error",
                     "Attempting to read from unknown type. Reading from a nested pair extraction is not legal due to pair erasure").formatError)
        }
    }
}
