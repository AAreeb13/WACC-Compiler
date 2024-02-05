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
      if (funcTable.contains(func.name)) {
          errList.addOne(s"${func.name} already exists, no support for overloaded functions.")
      }
      funcTable.addOne((func.name, (func.retType, func.params.map(_.declType))))
  }

  prog.funcs.foreach(checkFunction(_))
  prog.stats.foreach(checkStatement(_, globalTable))

  def checkFunction(func: Func): Unit = {
      // create new child table
      val funcSymbolTable = new SymbolTable()

      // add parameters to the table
      func.params.foreach{ param =>
          if (!funcSymbolTable.contains(param.name)) 
              funcSymbolTable.addOne(param.name, param.declType)
          else 
              errList.addOne(s"Scope error: Parameter ${param.name} has already been declared in function")
      }
      func.stats.foreach(checkStatement(_, funcSymbolTable))
  }

  def checkStatement(stat: Stat, currentScope: SymbolTable): Unit = stat match {
      case AssignNew(t, ident, rvalue) =>
      case Assign(lvalue, rvalue) => 
      
      case While(cond, stats) => 
      case If(cond, ifStat, elseStat) =>
      case Scope(stats) =>
      
      case Return(expr) =>
      case Free(expr) =>
      case Read(lvalue) =>
      case Exit(expr) =>
      case Print(expr) =>
      case Println(expr) =>

      case Skip =>
  }

  def checkExpression(expr: Expr, currentScope: SymbolTable): Option[Type] = expr match {
      case Var(v) => currentScope.typeof(v) match {
          case None => errList.addOne(s"Scope error: Variable ${v} has not been previously declared")
              None
          case x => x
      }

      case ArrayVal(v, exprs) => ???

      case Ord(x) => ???
      case Chr(x) => ???
      case Len(x) => ???
      case Not(x) => ???
      case Neg(x) => ???

      case Mul(x, y) => checkBinExpression(x, y, IntType, currentScope)
      case Mod(x, y) => checkBinExpression(x, y, IntType, currentScope)
      case Add(x, y) => checkBinExpression(x, y, IntType, currentScope)
      case Sub(x, y) => checkBinExpression(x, y, IntType, currentScope)
      case Div(x, y) => checkBinExpression(x, y, IntType, currentScope)
      
      case And(x, y) => checkBinExpression(x, y, BoolType, currentScope)
      case Or(x, y) => checkBinExpression(x, y, BoolType, currentScope)
          
      case GrtEql(x, y) => checkBinExpression(x, y, IntType, currentScope)
      case LessEql(x, y) => checkBinExpression(x, y, IntType, currentScope)
      case Less(x, y) => checkBinExpression(x, y, IntType, currentScope)
      case Grt(x, y) => checkBinExpression(x, y, IntType, currentScope)
                  
      case NotEql(x, y) => ???
      case Eql(x, y) => ???
      
      case StrVal(x) => Some(StringType)
      case BoolVal(x) => Some(BoolType)
      case IntVal(x) => Some(IntType)
      case CharVal(x) => Some(CharType)
      case PairVal => ???
  }

  private def checkBinExpression(x: Expr, y: Expr, expectedType: Type, currentScope: SymbolTable): Option[Type] = {
      def handleNoneCase(expectedType: Type): Option[Type] = expectedType match {
          case IntType => errList.addOne(s"Expected type: int")
              None
          case BoolType => errList.addOne(s"Expected type: bool")
              None
      }
      
      checkExpression(x, currentScope) match {
        case Some(expectedType) => checkExpression(y, currentScope) match {
            case Some(expectedType) => Some(expectedType)
            case None => handleNoneCase(expectedType)
        }
        case _ => handleNoneCase(expectedType)
    } 
  }


  def getResult: Either[String, Node] = {
      if (errList.isEmpty) {
          Right(prog)
      } else {
          Left(generateErrors)
      }
  }

  def generateErrors: String = {
      s"Semantic check failed:\n${errList.mkString("\n")}"
  }
}

class SymbolTable(val parent: Option[SymbolTable] = None) {
  var table: HashMap[String, Type] = HashMap()
  
  def addAll(xs: IterableOnce[(String, Type)]) = 
      table.addAll(xs)

  def addOne(name: String, declType: Type): Unit =
      table.addOne(name, declType)

  def contains(name: String): Boolean =
      table.contains(name) || parent.exists(_.contains(name))
  
  def typeof(name: String): Option[Type] 
      = table
      .get(name)
      .orElse(parent.flatMap(_.typeof(name)))

  // maybe add some flatten method so we can easily get all the info
  // in the global symbol table
}
