
data ByteCode = Bc [String]
data ExeStack = E0 | Stk Int ExeStack
type VarTable = [(String, Int)] 


runByteCode :: ByteCode -> Int
runByteCode bc = exeByteCode bc [] E0


exeByteCode :: ByteCode -> VarTable -> ExeStack -> Int
exeByteCode (Bc []) _ E0 = 0
exeByteCode (Bc []) _ (Stk top _) = top
exeByteCode (Bc ("RETURN_VALUE":_)) _ (Stk top _) = top 
exeByteCode (Bc (inst:tail)) var stk = exeByteCode (Bc tail) nvar nstk 
  where (nvar,nstk) = exeInstruction inst var stk


exeInstruction :: String -> VarTable -> ExeStack -> (VarTable,ExeStack)
exeInstruction "MULTIPLY" var (Stk top1 (Stk top2 stk)) = (var, Stk (top1*top2) stk)
exeInstruction "ADD" var (Stk top1 (Stk top2 stk)) = (var, Stk (top1+top2) stk)
exeInstruction inst var stk = 
  let
    [cmd, par] = words inst
    (top,nstk) = popES stk
  in case cmd of
    "LOAD_VAL"  -> (var, (Stk (read par :: Int) stk))
    "WRITE_VAR" -> (changeVar var (par,top), nstk)
    "READ_VAR"  -> (var, (Stk (readVar var par) stk))   


popES :: ExeStack -> (Int,ExeStack)
popES E0 = error "retirada em uma pilha vazia"
popES (Stk top stk) = (top,stk)


changeVar :: VarTable -> (String,Int) -> VarTable
changeVar [] varValue = [varValue]
changeVar ((vr,vl):tail) (var,value)
  | vr == var = ((vr,value):tail) 
  | otherwise = (vr,vl) : changeVar tail (var,value) 


readVar :: VarTable -> String -> Int
readVar [] _ = error "Variavel nao existe"
readVar ((vr,vl):tail) var
  | vr == var = vl 
  | otherwise = readVar tail var
