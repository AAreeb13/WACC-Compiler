package wacc

import scala.collection.mutable.HashMap

object semanticChecker {
    def verify(result: Either[String, Node]): Either[String, Node] = result.flatMap(_ match {
        case prog: Prog =>
            // Semantic checks will go here
            var globalTable = new SymbolTable();
            return result
        case _ => Left("Invalid AST type for top-level verification")
    })
}

class SymbolTable(val parent: Option[SymbolTable] = None) {
    var table: HashMap[String, Type] = parent match {
        case None => HashMap()
        case Some(value) => value.table.clone() // shallow copy
    }
    
    def addAll(xs: IterableOnce[(String, Type)]) = 
        table.addAll(xs)

    def addOne(name: String, declType: Type) =
        table.addOne(name, declType)

    def contains(name: String) =
        table.contains(name)
    
    def typeof(name: String) = table.get(name)

    // maybe add some flatten method so we can easily get all the info
    // in the global symbol table
}
