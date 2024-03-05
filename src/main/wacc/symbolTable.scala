package wacc

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import ast.Node
import semAst.SemType
import implicits._

/**
  * Doubly linked symbol table class that keeps track of parent and child nodes 
  *
  * @param parent Optional ST for parent scopes
  */
class SymbolTable(val parent: Option[SymbolTable] = None) {
    //  (ident, (semantic type, reference to original node))
    var table: HashMap[String, (SemType, Node)] = HashMap.empty
    var locationTable: HashMap[String, IR.Location] = HashMap.empty
    var children: ListBuffer[SymbolTable] = ListBuffer.empty
    var scopeSize = 0

    def getScopeSize(): Int = scopeSize

    // add child to parent so we don't need to explictly do this
    if (parent.isDefined) parent.get.addChild(this)

    private def addChild(childTable: SymbolTable) =
        children.addOne(childTable)

    // Add an entry to the symbol table
    def addOne(name: String, declType: SemType)(implicit node: Node): Unit = {
        table.addOne(name, (declType, node))
        scopeSize += sizeToInt(semanticToSize(declType))
        node.scope = this
    }

    def getLocation(name: String): Option[IR.Location] = 
        locationTable.get(name).orElse(parent.flatMap(_.getLocation(name)))

    def hasLocation(name: String): Boolean =
        locationTable.contains(name) || parent.exists(_.hasLocation(name))

    def hasLocationInCurrent(name: String): Boolean = {
        locationTable.contains(name)
    }

    def updateLocation(name: String, location: IR.Location): Unit = {
        if (hasLocation(name)) {
            locationTable.update(name, location)
        } else {
            locationTable.addOne((name, location))
        }
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