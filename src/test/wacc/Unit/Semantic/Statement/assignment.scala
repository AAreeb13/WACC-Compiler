package wacc.unit.semantic.statement

import wacc._
import astFactory._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class AssignmentTest extends AnyFlatSpec {
    /*
    begin
        x = 1
    end
    */
    "Assignments" should "fail for undeclared variables in lhs" in {
        semanticChecker.verify(Right(
            Prog(List(),List(Assign(Var("x"),IntVal(1))))        
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        x = y
    end
    */
    it should "fail for undeclared variables in rhs" in {
        semanticChecker.verify(Right(
            Prog(List(),List(Assign(Var("x"),Var("y"))))       
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        int x = 1;
        x = 'a'
    end
    */
    it should "fail for type mismatch" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",IntVal(1)), Assign(Var("x"),CharVal('a'))))     
        )) shouldBe a [Left[_, _]]
    }


    /*
    begin
        int x = 1;
        x = 3
    end
    */
    it should "succeeed otherwise" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",IntVal(1)), Assign(Var("x"),IntVal(3))))         
        )) shouldBe a [Right[_, _]]
    }
}