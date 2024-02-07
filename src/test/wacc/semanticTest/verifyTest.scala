package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class verifyTest extends AnyFlatSpec {
    "verify" should "only check programs" in {
        semanticChecker.verify(Right(
            Prog(List.empty, List.empty)
        )) shouldBe a [Right[_, _]]
    }

    it should "fail otherwise" in {
        semanticChecker.verify(Right(
            Skip
        )) shouldBe a [Left[_, _]]
    }
}