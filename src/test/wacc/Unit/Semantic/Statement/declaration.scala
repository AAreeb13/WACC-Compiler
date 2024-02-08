package wacc.unit.semantic

import wacc._
import astFactory._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class DeclarationTest extends AnyFlatSpec {
    /*
    begin
        int x = 1;
        int x = 3
    end
    */
    "declaration statements" should "fail for redeclaring existing variables in current scope" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",IntVal(1)), AssignNew(IntType,"x",IntVal(3))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        int x = y
    end
    */
    it should "fail for undeclared variables in rhs" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",Var("y"))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        int x = x
    end
    */
    it should "fail referencing itself in rhs" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType, "x", Var("x"))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        int x = 1;
        int y = x
    end
    */
    it should "succeed otherwise" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType, "x",IntVal(1)), AssignNew(IntType, "y",Var("x"))))
        )) shouldBe a [Right[_, _]]
    }
}
