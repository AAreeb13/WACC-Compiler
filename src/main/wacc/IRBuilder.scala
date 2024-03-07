package wacc

import scala.collection.mutable.ListBuffer
import IR._
import scala.collection.mutable.HashMap

class IRBuilder {
    var tagList: ListBuffer[Tag] = ListBuffer.empty
    var stringConstants: ListBuffer[StringLabel] = ListBuffer.empty
    var instrMap: HashMap[FuncLabel, ListBuffer[Line]] = HashMap.empty

    def addInstruction(instr: Line)(implicit funcLabel: FuncLabel): Unit = 
        if (instrMap.contains(funcLabel)) {
            instrMap(funcLabel).append(instr)
        } else {
            instrMap.addOne((funcLabel, ListBuffer(instr)))
        }

    def addInstructions(instrs: List[Line])(implicit funcLabel: FuncLabel): Unit = 
        instrs.foreach(addInstruction(_))

    def addTag(tag: Tag): Unit = 
        tagList += tag

    def addTag(tags: List[Tag]): Unit = 
        tagList ++= tags

    def addStringConstant(stringDecl: StringLabel): Unit = 
        stringConstants += stringDecl

    def addStringConstants(stringDecls: List[StringLabel]): Unit = 
        stringConstants ++= stringDecls

    def toList(): List[Line] = {
        stringConstants.toList :::
        instrMap.toList.flatMap{ case (func, instrs) => func :: instrs.toList }
    }

    def <<(instr: Line)(implicit funcLabel: FuncLabel): Unit = addInstruction(instr)
    def <<(instrs: List[Line])(implicit funcLabel: FuncLabel): Unit = addInstructions(instrs)
    def <<(tag: Tag): Unit = addTag(tag)
    def <<(tags: List[Tag]): Unit = addTag(tags)
    def <<(stringDecl: StringLabel): Unit = addStringConstant(stringDecl)
    //def <<(stringDecls: List[StringLabel]): Unit = addStringConstants(stringDecls)
}
