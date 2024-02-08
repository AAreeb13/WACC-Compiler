package wacc

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

        case (SemArray(a1), SemArray(a2)) => a1 == a2 // array equality
        case (SemArray(SemChar), SemString) => true // string weakening

        case (SemNull, SemPair(_, _)) | // null on pairs
             (SemNull, SemErasedPair) => true

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

sealed trait SemBaseType

case object SemInt extends SemBaseType
case object SemBool extends SemBaseType
case object SemChar extends SemBaseType
case object SemString extends SemBaseType

case object SemNull extends SemBaseType
// null keyword for pairvla

case object SemAny extends SemType
// any type

case object SemUnknown extends SemType
// nested pair extraction type

case object SemNone extends SemType
// error type

case class SemPair(t1: SemType, t2: SemType) extends SemType
case object SemErasedPair extends SemType

case class SemArray(t: SemType) extends SemType

