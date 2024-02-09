package wacc.unit.semantic

import wacc._
import astFactory._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class functionTest extends AnyFlatSpec {
    "functions" should "not contain duplicate parameters" in {
        /*
        begin
            int f(int x, char x) is
                return 0
            end
            skip
        end
         */
        semanticChecker.verify(Right(
            Prog(List(Func(IntType, "f", List(Param(IntType, "x"), Param(CharType, "x")), List(Return(IntVal(0))))), List(Skip))
        )) shouldBe a [Left[_, _]]
    }

    it should "be able to contain variables in body with same name as parameters" in {
        /*
        begin 
            string f(int x) is 
                string x = "hello world";
                return x
            end
            skip
        end
         */
        semanticChecker.verify(Right(
            Prog(List(Func(StringType,"f",List(Param(IntType,"x")),List(AssignNew(StringType,"x",StrVal("hello world")), Return(Var("x"))))),List(Skip))
        )) shouldBe a [Right[_, _]]
    }

    it should "fail for undeclared variables" in {
        /*
        begin 
            int f(int x) is 
                return y
            end
            skip
        end
         */
        semanticChecker.verify(Right(
            Prog(List(Func(IntType,"f",List(Param(IntType,"x")),List(Return(Var("y"))))),List(Skip))
        )) shouldBe a [Left[_, _]]
    }

    it should "not be overloaded (i.e with same name)" in {
        /*
        begin
            int f(int x) is
                return 0
            end
            int f(int x, int y) is
                return 0
            end
            skip
        end
         */
        semanticChecker.verify(Right(
            Prog(List(Func(IntType,"f",List(Param(IntType,"x")),List(Return(IntVal(0)))), Func(IntType,"f",List(Param(IntType,"x"), Param(IntType,"y")),List(Return(IntVal(0))))),List(Skip))
        )) shouldBe a [Left[_, _]]
    }

    it should "succeed otherwise" in {
        /*
        begin
            int f(int x) is
                return 0
            end
            skip
        end
         */
        semanticChecker.verify(Right(
            Prog(List(Func(IntType, "f", List(Param(IntType, "x")), List(Return(IntVal(0))))), List(Skip))
        )) shouldBe a [Right[_, _]]
    }

    ///////////// FUNCTION CALLS /////////////

    "function calls" should "be able to be call functions outside of scope" in {
        /*
        begin 
            int f() is 
                int x = call g(2);
                return x
            end
            int g(int x) is
                return x
            end    
            skip
        end
         */
        semanticChecker.verify(Right(
            Prog(List(Func(IntType,"f",List(),List(AssignNew(IntType,"x",FuncCall("g",List(IntVal(2)))), Return(Var("x")))), Func(IntType,"g",List(Param(IntType,"x")),List(Return(Var("x"))))),List(Skip))
        )) shouldBe a [Right[_, _]]

        /*
        begin 
            int f() is 
                int x = call f();
                return x        # unreachable - runtime error
            end
            skip
        end
         */
        semanticChecker.verify(Right(
            Prog(List(Func(IntType,"f",List(),List(AssignNew(IntType,"x",FuncCall("f",List())), Return(Var("x"))))),List(Skip))
        )) shouldBe a [Right[_, _]]
    }

    it should "fail for undefined functions" in {
        /*
        begin
            int x = call f(2)
        end
         */
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",FuncCall("f",List(IntVal(2))))))
        )) shouldBe a [Left[_, _]]
    }

    it should "fail for functions with different arity" in {
        /*
        begin 
            int f() is 
                return 1
            end
            int x = call f(2)
        end
         */
        semanticChecker.verify(Right(
            Prog(List(Func(IntType,"f",List(),List(Return(IntVal(1))))),List(AssignNew(IntType,"x",FuncCall("f",List(IntVal(2))))))
        )) shouldBe a [Left[_, _]]
    }

    it should "fail for functions with different argument types" in {
        /*
        begin 
            int f(char c) is 
                return 1
            end
            int x = call f(2)
        end
         */
        semanticChecker.verify(Right(
            Prog(List(Func(IntType,"f",List(Param(CharType,"c")),List(Return(IntVal(1))))),List(AssignNew(IntType,"x",FuncCall("f",List(IntVal(2))))))
        )) shouldBe a [Left[_, _]]
    }

    it should "succeed otherwise" in {
        /*
        begin 
            int f(int x) is 
                return x
            end
            int x = call f(2)
        end
         */
        semanticChecker.verify(Right(
            Prog(List(Func(IntType,"f",List(Param(IntType,"x")),List(Return(Var("x"))))),List(AssignNew(IntType,"x",FuncCall("f",List(IntVal(2))))))
        )) shouldBe a [Right[_, _]]
    }

}
