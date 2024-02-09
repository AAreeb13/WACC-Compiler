package wacc

object astFactory {
    def IntType = ast.IntType.default
    def CharType = ast.CharType.default
    def BoolType = ast.BoolType.default
    def StringType = ast.StringType.default
    def ErasedPair = ast.ErasedPair.default

    def ArrayType(t: ast.Type) = ast.ArrayType.default(t)
    def PairType(t1: ast.Type, t2: ast.Type)  = ast.PairType.default(t1, t2)

    def Prog(funcs: List[ast.Func], stats: List[ast.Stat]) = ast.Prog.default(funcs, stats)

    def Func(retType: ast.Type, name: String, params: List[ast.Param], stats: List[ast.Stat]) = ast.Func.default((retType, name), params, stats)

    def Param(declType: ast.Type, name: String) = ast.Param.default(declType, name)

    def Skip = ast.Skip.default
    def AssignNew(t: ast.Type, ident: String, rvalue: ast.RValue) = ast.AssignNew.default(t, ident, rvalue)
    def Assign(lvalue: ast.LValue, rvalue: ast.RValue) = ast.Assign.default(lvalue, rvalue)
    def Read(lvalue: ast.LValue) = ast.Read.default(lvalue)
    def Free(expr: ast.Expr) = ast.Free.default(expr)
    def Return(expr: ast.Expr) = ast.Return.default(expr)
    def Exit(expr: ast.Expr) = ast.Exit.default(expr)
    def Print(expr: ast.Expr) = ast.Print.default(expr)
    def Println(expr: ast.Expr) = ast.Println.default(expr)
    def If(cond: ast.Expr, ifStat: List[ast.Stat], elseStat: List[ast.Stat]) = ast.If.default(cond, ifStat, elseStat)
    def While(cond: ast.Expr, stats: List[ast.Stat]) = ast.While.default(cond, stats)
    def Scope(stats: List[ast.Stat]) = ast.Scope.default(stats)

    def FstPair(lvalue: ast.LValue) = ast.FstPair.default(lvalue)
    def SndPair(lvalue: ast.LValue) = ast.SndPair.default(lvalue)

    def ArrayLiteral(exprs: List[ast.Expr]) = ast.ArrayLiteral.default(exprs)
    def PairCons(fst: ast.Expr, snd: ast.Expr) = ast.PairCons.default(fst, snd)
    def FuncCall(ident: String, args: List[ast.Expr]) = ast.FuncCall.default(ident, args)

    def IntVal(x: BigInt) = ast.IntVal.default(x)
    def CharVal(x: Char) = ast.CharVal.default(x)
    def BoolVal(x: Boolean) = ast.BoolVal.default(x)
    def StrVal(x: String) = ast.StrVal.default(x)
    def PairVal = ast.PairVal.default
    def Var(v: String) = ast.Var.default(v)
    def ArrayVal(v: String, exprs: List[ast.Expr]) = ast.ArrayVal.default(v, exprs)

    def Mul(x: ast.Expr, y: ast.Expr) = ast.Mul.default(x, y)
    def Div(x: ast.Expr, y: ast.Expr) = ast.Div.default(x, y)
    def Mod(x: ast.Expr, y: ast.Expr) = ast.Mod.default(x, y)
    def Add(x: ast.Expr, y: ast.Expr) = ast.Add.default(x, y)
    def Sub(x: ast.Expr, y: ast.Expr) = ast.Sub.default(x, y)

    def Grt(x: ast.Expr, y: ast.Expr) = ast.Grt.default(x, y)
    def GrtEql(x: ast.Expr, y: ast.Expr) = ast.GrtEql.default(x, y)
    def Less(x: ast.Expr, y: ast.Expr) = ast.Less.default(x, y)
    def LessEql(x: ast.Expr, y: ast.Expr) = ast.LessEql.default(x, y)

    def Eql(x: ast.Expr, y: ast.Expr) = ast.Eql.default(x, y)
    def NotEql(x: ast.Expr, y: ast.Expr) = ast.NotEql.default(x, y)

    def And(x: ast.Expr, y: ast.Expr) = ast.And.default(x, y)
    def Or(x: ast.Expr, y: ast.Expr) = ast.Or.default(x, y)

    def Not(x: ast.Expr) = ast.Not.default(x)
    def Neg(x: ast.Expr) = ast.Neg.default(x)
    def Len(x: ast.Expr) = ast.Len.default(x)
    def Ord(x: ast.Expr) = ast.Ord.default(x)
    def Chr(x: ast.Expr) = ast.Chr.default(x)
}
