package wacc

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

object semanticChecker {
    def verify(result: Either[String, Node]): Either[String, Node] = result.flatMap(_ match {
        case prog: Prog => Analyser(prog).getResult
        case _ => Left("Invalid AST type for top-level verification")
    })
}

case class Analyser(val prog: Prog) {
    var errList: ListBuffer[String] = ListBuffer.empty
    var globalTable = new SymbolTable();
    var funcTable: HashMap[String, (Type, List[Type])] = HashMap.empty

    prog.funcs.foreach { func =>
        funcTable.addOne((func.name, (func.retType, func.params.map(_.declType))))
    }

    prog.funcs.foreach(checkFunction(_))
    prog.stats.foreach(checkStatement(_, globalTable))

    def checkFunction(func: Func): Unit = {
        // create new child table
        val funcSymbolTable = new SymbolTable()

        // add parameters to the table
        func.params.foreach{ param =>
                funcSymbolTable.addOne(param.name, param.declType)
        func.stats.foreach(checkStatement(_, funcSymbolTable))
    }

    def checkStatement(stat: Stat, currentScope: SymbolTable): Unit = {

    }

    def getResult: Either[String, Node] = {
        if (errList.isEmpty) {
            Right(prog)
        } else {
            Left(generateErrors)
        }
    }

    def generateErrors: String = {
        "A semantic error occured"
    }
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
