package wacc.unit.semantic.statement

import wacc._
import astFactory._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ScopeTest extends AnyFlatSpec {

    ////////////// SCOPE //////////////

    "scoped statements" should "be able to access variables in parent scopes" in {
        /*
        begin
            int x = 1;
            while (true) do
                println x
            done
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(IntType,"x",IntVal(1)), While(BoolVal(true),List(Println(Var("x"))))))
        )) shouldBe a [Right[_, _]]
    }

    it should "be able to redeclare a variable with same name in parent scope" in {
        /*
        begin
            bool x = true;
            begin
                int x = 4;
                print x
            end
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(BoolType,"x",BoolVal(true)), Scope(List(AssignNew(IntType,"x",IntVal(4)), Print(Var("x"))))))
        )) shouldBe a [Right[_, _]]
    }

    it should "not be able to redeclare a variable with same name in current scope" in {
        /*
        begin
            begin
                bool x = true;
                int x = 4;
                print x
            end
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(Scope(List(AssignNew(BoolType,"x",BoolVal(true)), AssignNew(IntType,"x",IntVal(4)), Print(Var("x"))))))
        )) shouldBe a [Left[_, _]]
    }

    it should "not be able to reference a variable with same name in sibling scopes" in {
        /*
        begin
            while (true) do
                int x = 4
            done;
            
            begin
                print x
            end
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(While(BoolVal(true),List(AssignNew(IntType,"x",IntVal(4)))), Scope(List(Print(Var("x"))))))
        )) shouldBe a [Left[_, _]]
    }
}
