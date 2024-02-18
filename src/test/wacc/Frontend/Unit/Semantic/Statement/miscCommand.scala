package wacc.unit.semantic.statement

import wacc._
import astFactory._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class CommandTest extends AnyFlatSpec {
    /////////// READ TESTS ///////////

    /*
    begin
        read x
    end
    */
    "read statements" should "fail for undeclared lvalues" in {
        semanticChecker.verify(Right(
            Prog(List(),List(Read(Var("x"))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        string x = "hello";
        read x
    end
    */
    it should "fail for lvalues that are not char/int type" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(StringType,"x",StrVal("hello")), Read(Var("x"))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        pair(pair, pair) x = newpair(null, null)
        read fst fst x
    end
    */
    it should "fail for nested pair extracted lvalues" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(PairType(ErasedPair,ErasedPair),"x",PairCons(PairVal,PairVal)), Read(FstPair(FstPair(Var("x"))))))
        )) shouldBe a [Left[_, _]]
    }

    /////////// FREE TESTS ///////////

    "free statements" should "fail for undeclared variables" in {
        /*
        begin
            free x
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Free(Var("x"))))
        )) shouldBe a [Left[_, _]]
    }


    it should "fail for non-pair and array types" in {
        /*
        begin
            free 2
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Free(IntVal(2))))
        )) shouldBe a [Left[_, _]]
        
        /*
        begin
            int x = 1;
            free x
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",IntVal(1)), Free(Var("x"))))
        )) shouldBe a [Left[_, _]]
    }

    it should "succeed for null types" in {
        /*
        begin
            free null
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Free(PairVal)))
        )) shouldBe a [Right[_, _]]
    }

    it should "succeed for array types" in {
        /*
        begin
            int[] x = [];
            free x
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(ArrayType(IntType),"x",ArrayLiteral(List())), Free(Var("x"))))
        )) shouldBe a [Right[_, _]]
    }
    
    it should "succeed for pair types" in {
        /*
        begin
            pair(int, int) p = null;
            free p
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(PairType(IntType,IntType),"p",PairVal), Free(Var("p"))))
        )) shouldBe a [Right[_, _]]
    }

    /////////// PRINT/PRINTLN TESTS ///////////

    "print and println statements" should "fail for undeclared variables" in {
        /*
        begin
            print x
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Print(Var("x"))))
        )) shouldBe a [Left[_, _]]
        
        /*
        begin
            println y
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Println(Var("y"))))
        )) shouldBe a [Left[_, _]]
    }

    it should "succeed otherwise" in {
        /*
        begin
            print 2
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Print(IntVal(2))))
        )) shouldBe a [Right[_, _]]
        
        /*
        begin
            pair(pair, pair) y = null;
            println y
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(PairType(ErasedPair,ErasedPair),"y",PairVal), Println(Var("y"))))
        )) shouldBe a [Right[_, _]]
    }

    /////////// RETURN TESTS ///////////

    "return statements" should "fail for undeclared variables" in {
        /*
        begin
            int foo() is
                return x
            end
            skip
        end
        */
        semanticChecker.verify(Right(
            Prog(List(Func(IntType,"foo",List(),List(Return(Var("x"))))),List(Skip))
        )) shouldBe a [Left[_, _]]
    }

    it should "fail for mismatched type" in {
        /*
        begin
            int foo() is
                return "hello world"
            end
            skip
        end
        */
        semanticChecker.verify(Right(
            Prog(List(Func(IntType,"foo",List(),List(Return(StrVal("hello world"))))),List(Skip))
        )) shouldBe a [Left[_, _]]
    }

    it should "fail outside of function scope" in {
        /*
        begin
            return 1
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Return(IntVal(1))))
        )) shouldBe a [Left[_, _]]

        /*
        begin
            skip;
            begin
                return 3;
                println "hello"
            end;
            skip
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Skip, Scope(List(Return(IntVal(3)), Println(StrVal("hello")))), Skip))
        )) shouldBe a [Left[_, _]]
    }

    it should "succeed otherwise" in {
        /*
        begin
            int foo() is
                return 1
            end
            skip
        end
        */
        semanticChecker.verify(Right(
            Prog(List(Func(IntType,"foo",List(),List(Return(IntVal(1))))),List(Skip))
        )) shouldBe a [Right[_, _]]

        /*
        begin
            int foo(int x) is
                return x
            end
            skip
        end
        */
        semanticChecker.verify(Right(
            Prog(List(Func(IntType,"foo",List(Param(IntType, "x")),List(Return(Var("x"))))),List(Skip))
        )) shouldBe a [Right[_, _]]
    }

    /////////// EXIT TESTS ///////////

    "exit statements" should "fail for undeclared variables" in {
        /*
        begin
            exit x
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Exit(Var("x"))))
        )) shouldBe a [Left[_, _]]
    }

    it should "fail for non-integer types" in {
        /*
        begin
            exit 'a'
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Exit(CharVal('a'))))
        )) shouldBe a [Left[_, _]]
    }

    it should "succeed otherwise" in {
        /*
        begin
            exit 1
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Exit(IntVal(1))))
        )) shouldBe a [Right[_, _]]

        /*
        begin
            int x = -1;
            exit x
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",IntVal(-1)), Exit(Var("x"))))
        )) shouldBe a [Right[_, _]]
    }
}
