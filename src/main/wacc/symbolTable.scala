package wacc


import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import ast._
import asmIR._

/**
  * Doubly linked symbol table class that keeps track of parent and child nodes 
  *
  * @param parent Optional ST for parent scopes
  */
class SymbolTable(val parent: Option[SymbolTable] = None) {
    //  (ident, (semantic type, reference to original node))
    var table: HashMap[String, (SemType, Node, Option[Operand])] = HashMap()
    var children: ListBuffer[SymbolTable] = ListBuffer.empty
    var currentScopeOffset: Int = 0

    // add child to parent so we don't need to explictly do this
    if (parent.isDefined) parent.get.addChild(this)

    private def addChild(childTable: SymbolTable) =
        children.addOne(childTable)

    // Add an entry to the symbol table
    def addOne(name: String, declType: SemType)(implicit node: Node): Unit = {
        table.addOne(name, (declType, node, None))
        updateScopeSize(declType)
        node.scope = this
    }

    def get(ident: String): Option[(SemType, Node, Option[Operand])] = {
        table.get(ident).orElse(parent.flatMap(_.get(ident)))
    }

    def previousLocation(ident: String): Option[Operand] = {
        table.get(ident) match {
            case Some((_, _, Some(location))) => Some(location)
            case _ => parent.flatMap(_.previousLocation(ident))
        }
    }

    private def updateScopeSize(declType: SemType): Unit = {
        val size = declType match {
            case SemInt => 4
            case SemBool => 1
            case SemChar => 1
            case _ => 0
        }
        currentScopeOffset += size
    }

    def updateStackLocation(name: String, location: Option[Operand]): Unit = {
        val oldEntry = table.get(name).get
        table.addOne(name, (oldEntry._1, oldEntry._2, location))
    }

    // Check for existence in this and all parent scopes
    def contains(name: String): Boolean =
        table.contains(name) || parent.exists(_.contains(name))

    // Check for existence only in the current scope
    def containsInCurrent(name: String): Boolean =
        table.contains(name)

    // Gets the type of the identifier (if it exists) in this scope
    // or parent scopes
    def typeof(name: String): Option[SemType] = table
        .get(name)
        .map(_._1)
        .orElse(parent.flatMap(_.typeof(name)))

    // Gets the node of the identifier (if it exists) in this scope
    // or parent scopes
    def nodeof(name: String): Option[Node] = table
        .get(name)
        .map(_._2)
        .orElse(parent.flatMap(_.nodeof(name)))
}
