package wacc

import scala.language.implicitConversions
import ast._

object Implicits {
    implicit def syntaxToSemanticType(t: Type): SemType = t match {
        case IntType() => SemInt
        case CharType() => SemChar
        case StringType() => SemString
        case BoolType() => SemBool
        case ErasedPair() => SemErasedPair
        case ArrayType(t) => SemArray(syntaxToSemanticType(t))
        case PairType(t1, t2) => SemPair(syntaxToSemanticType(t1), syntaxToSemanticType(t2))
        case _ => SemNone
    }
}

sealed trait SemNode

sealed trait SemType extends SemNode {
    def reducesTo(otherType: SemType): Boolean = (this, otherType) match {
        // (rhs, lhs)

        case (SemNone, _) | (_, SemNone) => false // errors will always be errors
        case (SemAny, _) | (_, SemAny) => true // anything can be anyting

        case (SemInt, SemInt) | // base types always match
             (SemChar, SemChar) | 
             (SemBool, SemBool) | 
             (SemString, SemString) => true

        case (SemArray(SemNull), SemArray(SemPair(_, _))) => true
        case (SemArray(SemNull), SemArray(SemErasedPair)) => true

        case (SemArray(SemAny), SemArray(_)) => true // empty arrays can be any array type
        case (SemArray(a1), SemArray(a2)) => a1 == a2 // array equality
        case (SemArray(SemChar), SemString) => true // string weakening

        case (SemPair(SemNull, SemNull), SemPair(l1, r1)) => true
        case (SemPair(SemNull, r1), SemPair(l2, r2)) => r1 == r2
        case (SemPair(l1, SemNull), SemPair(l2, r2)) => l1 == l2

        case (SemPair(l1, r1), SemPair(SemNull, SemNull)) => true
        case (SemPair(l1, r1), SemPair(SemNull, r2)) => r1 == r2
        case (SemPair(l1, r1), SemPair(l2, SemNull)) => l1 == l2


        case (SemNull, SemPair(_, _)) | // null on pairs
             (SemNull, SemErasedPair) |
             (SemPair(_, _), SemNull) |
             (SemErasedPair, SemNull) => true

        case (SemNull, SemNull) => true

        case (SemUnknown, SemUnknown) => false
        case (SemUnknown, _) => true
        case (_, SemUnknown) => true

        case (SemPair(l1, r1), SemPair(l2, r2)) => (l1 == l2) && (r1 == r2)
        case (SemErasedPair, SemPair(_, _)) |
             (SemPair(_, _), SemErasedPair) |
             (SemErasedPair, SemErasedPair) => true
            
        case _ => false

        // case (SemNone, _) | (_, NoneType) => false
        // case (AnyType, _) | (_, AnyType) => true
        // case (IntType, IntType) | 
        //      (CharType, CharType) | 
        //      (BoolType, BoolType) | 
        //      (StringType, StringType) => true

        // case (ArrayType(a1), ArrayType(a2)) => a1 == a2
        // case (ArrayType(CharType), StringType) => true

        // case (PairType(AnyType, AnyType), PairType(l1, r1)) => true
        // case (PairType(AnyType, r1), PairType(l2, r2)) => r1 == r2
        // case (PairType(l1, AnyType), PairType(l2, r2)) => l1 == l2

        // case (PairType(l1, r1), PairType(AnyType, AnyType)) => true
        // case (PairType(l1, r1), PairType(AnyType, r2)) => r1 == r2
        // case (PairType(l1, r1), PairType(l2, AnyType)) => l1 == l2

        // case (PairType(l1, r1), PairType(l2, r2)) => (l1 == l2) && (r1 == r2)
        // case (ErasedPair, PairType(_, _)) | (PairType(_, _), ErasedPair) => true

        // case (ErasedPair, ErasedPair) => false  // last bullet point on pair coercion
        // case _ => false
    }
}

sealed trait SemBaseType extends SemType

case object SemInt extends SemBaseType  {
    override def toString = "int"
}

case object SemBool extends SemBaseType  {
    override def toString = "bool"
}

case object SemChar extends SemBaseType {
    override def toString = "char"
}

case object SemString extends SemBaseType {
    override def toString = "string"
}

case object SemNull extends SemBaseType {
    override def toString = "null"
}

// null keyword for pairval

case object SemAny extends SemBaseType  {
    override def toString = "any"
}
// any type

case object SemUnknown extends SemBaseType
// nested pair extraction type

case object SemNone extends SemBaseType
// error type

case class SemPair(t1: SemType, t2: SemType) extends SemType {
    override def toString = s"pair(${t1.toString}, ${t2.toString})"
}

case object SemErasedPair extends SemType {
    override def toString = "pair"
}

case class SemArray(t: SemType) extends SemType  {
    override def toString = s"${t.toString}[]"

    def dimensions: Int = t match {
        case arrType: SemArray => 1 + arrType.dimensions
        case _ => 1
    }

    def unfold(levels: Int): Option[SemType] = (levels, t) match {
        case (1, t) => Some(t)
        case (n, arr: SemArray) => arr.unfold(n-1)
        case _ => None
    }
}

