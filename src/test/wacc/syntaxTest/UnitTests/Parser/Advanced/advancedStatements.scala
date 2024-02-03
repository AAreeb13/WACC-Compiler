package wacc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.Lexer
import parsley.token.descriptions._
import parsley.{Result, Success, Failure}
import org.scalactic.Bool

class advancedParserStatementTest extends AnyFlatSpec {
    val statParser = lexer.fully(parser.stmt)
    
    // While tests

    "while loop with multiple assignments" should "match stmt" in {
        statParser.parse("""
        while n > 0 do 
            int save = f0;
            f0 = f1;
            f1 = save + f1;
            n = n - 1
        done
        """) shouldBe
        Success(
            While(
                Grt(Var("n"), IntVal(0)),
                List(
                    AssignNew(IntType, "save", Var("f0")),
                    Assign(Var("f0"), Var("f1")),
                    Assign(Var("f1"), Add(Var("save"), Var("f1"))),
                    Assign(Var("n"), Sub(Var("n"), IntVal(1)))
                )
            )
        )
    }

    "while loop with multiple statements" should "match stmt" in {
        statParser.parse("""
        while i < 20 do
            print f0;
            print ", ";
            save = f0;
            f0 = f1;
            f1 = save + f1;
            i = i + 1
        done
        """) shouldBe
        Success(
            While(
                Less(Var("i"), IntVal(20)),
                List(
                    Print(Var("f0")),
                    Print(StrVal(", ")),
                    Assign(Var("save"), Var("f0")),
                    Assign(Var("f0"), Var("f1")),
                    Assign(Var("f1"), Add(Var("save"), Var("f1"))),
                    Assign(Var("i"), Add(Var("i"), IntVal(1)))
                )
            )
        )
    }

    "while loop with arithmetic" should "match stmt" in {
        statParser.parse("""
        while (y > 0 || x > 0) do
            x = x - 1;
            y = y - 1;
            i = i + 1
        done
        """) shouldBe
        Success(
            While(
                Or(Grt(Var("y"), IntVal(0)), Grt(Var("x"), IntVal(0))),
                List(
                    Assign(Var("x"), Sub(Var("x"), IntVal(1))),
                    Assign(Var("y"), Sub(Var("y"), IntVal(1))),
                    Assign(Var("i"), Add(Var("i"), IntVal(1)))
                )
            )
        )
    }



    // If and Else Tests

    "if-else statement" should "match stmt" in {
        statParser.parse("""
        if a == 13 then
            println "correct"
        else
            println "incorrect"
        fi
        """) shouldBe
        Success(
            If(
                Eql(Var("a"), IntVal(13)),
                List(
                    Println(StrVal("correct"))
                ),
                List(
                    Println(StrVal("incorrect"))
                )
            )
        )
    }

    "nested if-else statement" should "match stmt" in {
        statParser.parse("""
        if a == 13 then
            if b == 4 then 
                a = 4
            else
                b = 4
            fi
        else
            a = 10
        fi""") shouldBe
        Success(
            If(
                Eql(Var("a"), IntVal(13)),
                List(
                    If(
                        Eql(Var("b"), IntVal(4)),
                        List(
                            Assign(Var("a"), IntVal(4))
                        ),
                        List(
                            Assign(Var("b"), IntVal(4))
                        )
                    )
                ),
                List(
                    Assign(Var("a"), IntVal(10))
                )
            )
        )
    }

    "while loop with nested if-else statement" should "match stmt" in {
        statParser.parse("""
        while n > 0 do
            if a == 13 then
                if b == 4 then
                    a = 4
                else
                    b = 4
                fi
            else
                println "incorrect"
            fi
        done
        """) shouldBe
        Success(
            While(
                Grt(Var("n"), IntVal(0)),
                List(
                    If(
                        Eql(Var("a"), IntVal(13)),
                        List(
                            If(
                                Eql(Var("b"), IntVal(4)),
                                List(
                                    Assign(Var("a"), IntVal(4))
                                ),
                                List(
                                    Assign(Var("b"), IntVal(4))
                                )
                            )
                        ),
                        List(
                            Println(StrVal("incorrect"))
                        )
                    )
                )
            )
        )
    }

    "while loop with nested while loop and if-else statement" should "match stmt" in {
        statParser.parse("""
        while n > 0 do
            if a == 13 then 
                while a > 0 do
                    a = a - 1
                done
            else
                a = 13
            fi
        done
        """) shouldBe
        Success(
            While(
                Grt(Var("n"), IntVal(0)),
                List(
                    If(
                        Eql(Var("a"), IntVal(13)),
                        List(
                            While(
                                Grt(Var("a"), IntVal(0)),
                                List(
                                    Assign(Var("a"), Sub(Var("a"), IntVal(1)))
                                )
                            )
                        ),
                        List(
                            Assign(Var("a"), IntVal(13))
                        )
                    )
                )
            )
        )
    }

    
}