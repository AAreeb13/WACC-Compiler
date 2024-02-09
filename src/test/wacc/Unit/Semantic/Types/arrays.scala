package wacc.unit.semantic.types

import wacc._
import astFactory._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ArrayTest extends AnyFlatSpec {
    ////////////// ARRAY DECLARATION //////////////
    /*
    begin
        int[] x = 4
    end
    */
    "array declarations" should "fail for invalid types" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(ArrayType(IntType),"x",IntVal(4))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        int[] x = [1, 2, 3, 4]
    end
    */
    it should "succeed for array literals with same type" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(ArrayType(IntType),"x",ArrayLiteral(List(IntVal(1), IntVal(2), IntVal(3), IntVal(4))))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        int[] x = []
    end
    */
    it should "succeed for empty array literal" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(ArrayType(IntType),"x",ArrayLiteral(List()))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        int[] x = [1, 2, 3, 4];
        x[0] = [5, 6, 7]
    end
    */
    "array values" should "fail for invalid type on rhs" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(ArrayType(IntType),"x",ArrayLiteral(List(IntVal(1), IntVal(2), IntVal(3), IntVal(4)))), Assign(ArrayVal("x",List(IntVal(0))),ArrayLiteral(List(IntVal(5), IntVal(6), IntVal(7))))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        int[] x = [1, 2, 3, 4];
        char y = x[0]
    end
    */
    it should "fail for invalid type on lhs" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(ArrayType(IntType),"x",ArrayLiteral(List(IntVal(1), IntVal(2), IntVal(3), IntVal(4)))), AssignNew(CharType,"y",ArrayVal("x",List(IntVal(0))))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        int[] x = [1, 2, 3, 4];
        int y = a[0];
        a[1] = y + 1
    end
    */
    it should "match otherwise" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(ArrayType(IntType),"a",ArrayLiteral(List(IntVal(1), IntVal(2), IntVal(3), IntVal(4)))), AssignNew(IntType,"y",ArrayVal("a",List(IntVal(0)))), Assign(ArrayVal("a",List(IntVal(1))),Add(Var("y"),IntVal(1)))))
        )) shouldBe a [Right[_, _]]
    }

    ////////////// WEAKENING //////////////

    /*
    begin
        string x = ['h', 'e', 'l', 'l', 'o']
    end
    */
    "char arrays" should "be able to weaken to strings" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(StringType,"x",ArrayLiteral(List(CharVal('h'), CharVal('e'), CharVal('l'), CharVal('l'), CharVal('o'))))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        char[] x = "hello"
    end
    */
    "strings" should "not be able to weaken to char arrays" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(ArrayType(CharType),"x",StrVal("hello"))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        char[] x = ['n', 'o'];
        char[] y = ['y', 'e', 's'];
        char[][] z = [x, y];
        string[] w = z
    end
    */
    "array of char arrays (2d)" should "not be able to weaken to string arrays" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(ArrayType(CharType),"x",ArrayLiteral(List(CharVal('n'), CharVal('o')))), AssignNew(ArrayType(CharType),"y",ArrayLiteral(List(CharVal('y'), CharVal('e'), CharVal('s')))), AssignNew(ArrayType(ArrayType(CharType)),"z",ArrayLiteral(List(Var("x"), Var("y")))), AssignNew(ArrayType(StringType),"w",Var("z"))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        char[] x = ['n', 'o'];
        char[] y = ['y', 'e', 's'];
        string z = "maybe";
        string[] w = [x, y, z]
    end
    */
    "arrays literals assignments" should "have type of ancestor of all types in literal" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(ArrayType(CharType),"x",ArrayLiteral(List(CharVal('n'), CharVal('o')))), AssignNew(ArrayType(CharType),"y",ArrayLiteral(List(CharVal('y'), CharVal('e'), CharVal('s')))), AssignNew(StringType,"z",StrVal("maybe")), AssignNew(ArrayType(StringType),"w",ArrayLiteral(List(Var("x"), Var("y"), Var("z"))))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        char[] x = ['n', 'o'];
        char[] y = ['y', 'e', 's'];
        string z = "maybe";
        int a = 1;
        string[] w = [x, y, z, a]
    end
    */
    it should "fail for invalid type compatibility" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(ArrayType(CharType),"x",ArrayLiteral(List(CharVal('n'), CharVal('o')))), AssignNew(ArrayType(CharType),"y",ArrayLiteral(List(CharVal('y'), CharVal('e'), CharVal('s')))), AssignNew(StringType,"z",StrVal("maybe")), AssignNew(IntType,"a",IntVal(1)), AssignNew(ArrayType(StringType),"w",ArrayLiteral(List(Var("x"), Var("y"), Var("z"), Var("a"))))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        char[] x = ['n', 'o'];
        char[] y = ['y', 'e', 's'];
        string z = "maybe";
        string[] w = [x, y, z, a]
    end
    */
    it should "fail for scope error" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(ArrayType(CharType),"x",ArrayLiteral(List(CharVal('n'), CharVal('o')))), AssignNew(ArrayType(CharType),"y",ArrayLiteral(List(CharVal('y'), CharVal('e'), CharVal('s')))), AssignNew(StringType,"z",StrVal("maybe")), AssignNew(ArrayType(StringType),"w",ArrayLiteral(List(Var("x"), Var("y"), Var("z"), Var("a"))))))
        )) shouldBe a [Left[_, _]]
    }
}
