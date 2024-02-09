package wacc.unit.semantic.statement

import wacc._
import astFactory._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class ConditionalTest extends AnyFlatSpec {
    ////////////// IF/WHILE //////////////

    "conditional statements" must "have a boolean expression" in {
        /*
        begin
            if (true) then
                skip
            else
                skip
            fi 
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(If(BoolVal(true),List(Skip),List(Skip))))
        )) shouldBe a [Right[_, _]]

        /*
        begin
            while (1) do
                skip
            done
        end
        */
        semanticChecker.verify(Right(
            Prog(List(),List(While(IntVal(1),List(Skip))))
        )) shouldBe a [Left[_, _]]
    }
}
