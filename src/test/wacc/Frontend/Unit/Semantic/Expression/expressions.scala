package wacc.unit.semantic.expression

import wacc._
import astFactory._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ExpressionTest extends AnyFlatSpec {
    /////////////// BINARY EXPRESSIONS ///////////////
    "arithmetic operators" should "fail if operands are not integral types" in {
        /*
        begin
            print 'c' + 'c'
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Print(Add(CharVal('c'),CharVal('c')))))
        )) shouldBe a [Left[_, _]]

        /*
        begin
            int x = 1;
            print 1 * ("oops" % 2)
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",IntVal(1)), Print(Mul(IntVal(1),Mod(StrVal("oops"),IntVal(2))))))
        )) shouldBe a [Left[_, _]]
    }

    it should "succeed otherwise" in {
        /*
        begin
            int x = 3;
            print 1 * x
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",IntVal(3)), Print(Mul(IntVal(1),Var("x")))))
        )) shouldBe a [Right[_, _]]
    }

    it should "also return an integer" in {
        /*
        begin
            int x = 3;
            print (1 * x) - x
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",IntVal(3)), Print(Sub(Mul(IntVal(1),Var("x")),Var("x")))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        int x = 1;
        print x > "string"
    end
    */
    "comparison operators" should "fail if both operands are not char or int types" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",IntVal(1)), Print(Grt(Var("x"),StrVal("string")))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        print 'a' <= 1
    end
    */
    it should "also fail if both operands are not the same type" in {
        semanticChecker.verify(Right(
            Prog(List(),List(Print(LessEql(CharVal('a'),IntVal(1)))))
        )) shouldBe a [Left[_, _]]
    }

    it should "succeed otherwise" in {
        /*
        begin
            print 'a' <= 'c'
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Print(LessEql(CharVal('a'),CharVal('c')))))
        )) shouldBe a [Right[_, _]]

        /*
        begin
            print 1 > 2
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Print(Grt(IntVal(1),IntVal(2)))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        while (1 <= 1) do skip done
    end
    */
    it should "also return a boolean" in {
        semanticChecker.verify(Right(
            Prog(List(),List(While(LessEql(IntVal(1),IntVal(1)),List(Skip))))
        )) shouldBe a [Right[_, _]]
    }

    "logical operators" should "fail if operands are not boolean types" in {
        /*
        begin
            print true || '1'
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Print(Or(BoolVal(true),CharVal('1')))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        print true && true
    end
    */
    it should "succeed otherwise" in {
        semanticChecker.verify(Right(
            Prog(List(),List(Print(And(BoolVal(true),BoolVal(true)))))
        )) shouldBe a [Right[_, _]]
    }
    
    /*
    begin
        print false || true && true
    end
    */
    it should "also return a boolean" in {
        semanticChecker.verify(Right(
            Prog(List(),List(Print(Or(BoolVal(false),And(BoolVal(true),BoolVal(true))))))
        )) shouldBe a [Right[_, _]]
    }

    "equality operators" should "fail if operands are not matching types" in {
        /*
        begin
            print true == 'a'
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Print(Eql(BoolVal(true),CharVal('a')))))
        )) shouldBe a [Left[_, _]]

        /*
        begin
            int x = 1;
            print x != null
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",IntVal(1)), Print(NotEql(Var("x"),PairVal))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        print true && true
    end
    */
    it should "succeed otherwise" in {
        semanticChecker.verify(Right(
            Prog(List(),List(Print(And(BoolVal(true),BoolVal(true)))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        while (1 == 1) do skip done
    end
    */
    it should "also return a boolean" in {
        semanticChecker.verify(Right(
            Prog(List(),List(While(Eql(IntVal(1),IntVal(1)),List(Skip))))
        )) shouldBe a [Right[_, _]]
    }

    /////////////// UNARY EXPRESSIONS ///////////////

    /*
    begin
        char x = 'a';
        print -x
    end
    */
    "arithmetic negation" should "fail if operand is not an integral type" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(CharType,"x",CharVal('a')), Print(Neg(Var("x")))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        int x = 1;
        print -x
    end
    */
    it should "succeed otherwise" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",IntVal(1)), Print(Neg(Var("x")))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        int x = 1;
        print --x
    end
    */
    it should "also return an integer" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",IntVal(1)), Print(Neg(Neg(Var("x"))))))
        )) shouldBe a [Right[_, _]]
    }


    /*
    begin
        char x = 'a';
        print !x
    end
    */

    "logical negation" should "fail if operand is not a boolean type" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(CharType,"x",CharVal('a')), Print(Not(Var("x")))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        bool x = true;
        print !x
    end
    */
    it should "succeed otherwise" in {

        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(BoolType,"x",BoolVal(true)), Print(Not(Var("x")))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        bool x = true;
        print !!x
    end
    */
    it should "also return a boolean" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(BoolType,"x",BoolVal(true)), Print(Not(Not(Var("x"))))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        char x = 'a';
        print len x
    end
    */
    "len operator" should "fail if operand is not an array type" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(CharType,"x",CharVal('a')), Print(Len(Var("x")))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        char[] x = ['a'];
        print len(x)
    end
    */
    it should "succeed otherwise" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(ArrayType(CharType),"x",ArrayLiteral(List(CharVal('a')))), Print(Len(Var("x")))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        char[] x = ['a'];
        exit len(x)
    end
    */
    it should "also return an integer" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(ArrayType(CharType),"x",ArrayLiteral(List(CharVal('a')))), Exit(Len(Var("x")))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        int x = 1;
        print ord(x)
    end
    */
    "ord operator" should "fail if operand is not a char type" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",IntVal(1)), Print(Ord(Var("x")))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        char x = 'a';
        print ord(x)
    end
    */
    it should "succeed otherwise" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(CharType,"x",CharVal('a')), Print(Ord(Var("x")))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        char x = 'a';
        exit ord(x)
    end
    */
    it should "also return an integer" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(CharType,"x",CharVal('a')), Exit(Ord(Var("x")))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        char x = 'a';
        print chr x
    end
    */
    "chr operator" should "fail if operand is not an integral type" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(CharType,"x",CharVal('a')), Print(Chr(Var("x")))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        int x = 1;
        print chr x
    end
    */
    it should "succeed otherwise" in {

        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",IntVal(1)), Print(Chr(Var("x")))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        int x = 1;
        print chr x < 'a'
    end
    */
    it should "also return a char" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",IntVal(1)), Print(Less(Chr(Var("x")),CharVal('a')))))
        )) shouldBe a [Right[_, _]]
    }
}
