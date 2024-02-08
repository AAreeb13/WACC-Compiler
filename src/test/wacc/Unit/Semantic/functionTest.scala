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

    it should "not be overloaded" in {
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

}
