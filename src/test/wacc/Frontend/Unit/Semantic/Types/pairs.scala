package wacc.unit.semantic.types

import wacc._
import astFactory._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class PairTest extends AnyFlatSpec {
    /////////////// DECLARATIONS ///////////////
    /*
    begin
        pair(int, int) x = newpair("hello", "world")
    end
    */
    "pair declarations" should "fail for type mismatch in rhs" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(PairType(IntType,IntType),"x",PairCons(StrVal("hello"),StrVal("world")))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        pair(int, int) x = null
    end
    */
    it should "succeed with pair literals on rhs" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(PairType(IntType,IntType),"x",PairVal)))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        pair(int, pair) x = newpair(1, null)
    end
    */
    "nested pair declarations" should "succeed with pair literals in newpair" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(PairType(IntType,ErasedPair),"x",PairCons(IntVal(1),PairVal))))
        )) shouldBe a [Right[_, _]]
    }

    /////////////// ELEMENT EXTRACTION ///////////////

    /*
    begin
        pair(int, char) p = null;
        fst p = "hello"
    end
    */
    "pair element extraction" should "fail with mismatched type in lhs" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(PairType(IntType,CharType),"p",PairVal), Assign(FstPair(Var("p")),StrVal("hello"))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        pair(int, char) p = null;
        int x = snd p
    end
    */
    it should "fail with mismatched type in rhs" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(PairType(IntType,CharType),"p",PairVal), AssignNew(IntType,"x",SndPair(Var("p")))))
        )) shouldBe a [Left[_, _]]
    }

    /*
    begin
        pair(int, int) p = null;
        fst p = snd p
    end
    */
    it should "succeed otherwise" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(PairType(IntType,IntType),"p",PairVal), Assign(FstPair(Var("p")),SndPair(Var("p")))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        pair(int, pair) x = newpair(1, null);
        fst snd x = 2
    end
    */
    "nested pair extractions" should "be able to match any known type in lhs" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(PairType(IntType,ErasedPair),"x",PairCons(IntVal(1),PairVal)), Assign(FstPair(SndPair(Var("x"))),IntVal(2))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        pair(int, pair) x = newpair(1, null);
        int y = snd snd x
    end
    */
    it should "be able to match any known type in rhs" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(PairType(IntType,ErasedPair),"x",PairCons(IntVal(1),PairVal)), AssignNew(IntType,"y",SndPair(SndPair(Var("x"))))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        pair(int, pair) x = null;
        fst snd x = snd snd x
    end
    */
    it should "fail for unknown types (i.e another nested pair extraction) on both sides" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(PairType(IntType,ErasedPair),"x",PairVal), Assign(FstPair(SndPair(Var("x"))),SndPair(SndPair(Var("x"))))))
        )) shouldBe a [Left[_, _]]
    }

    /////////////// WEAKENING/COERCION ///////////////

    /*
    begin
        pair(string, string) p = null;
        pair(int, pair) q = newpair(1, p);
        pair(char, char) r = snd q
    end
    */
    "pairs" should "be able to weaken to erased pairs" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(PairType(StringType,StringType),"p",PairVal), AssignNew(PairType(IntType,ErasedPair),"q",PairCons(IntVal(1),Var("p"))), AssignNew(PairType(CharType,CharType),"r",SndPair(Var("q")))))
        )) shouldBe a [Right[_, _]]
    }

    /*
    begin
        pair(string, string) p = null;
        pair(int, pair) q = newpair(1, p);
        pair(char, char) r = null;
        snd q = r
    end
    */
    "erased pairs" should "be able to weaken to non-erased pairs" in {
        semanticChecker.verify(Right(
            Prog(List(),List(AssignNew(PairType(StringType,StringType),"p",PairVal), AssignNew(PairType(IntType,ErasedPair),"q",PairCons(IntVal(1),Var("p"))), AssignNew(PairType(CharType,CharType),"r",PairVal), Assign(SndPair(Var("q")),Var("r"))))
        )) shouldBe a [Right[_, _]]
    }
}
