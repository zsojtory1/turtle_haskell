module Interp (eval) where
import Ast
import Instruction

-- An environment maps variables to values. This allows you to look up
-- what a variable stands for when you use it (for example, after let-binding,
-- or using a parameter)
type Env = [(Variable, Value)]

-- TurtleState is a Haskell record which contains the state you will need when
-- evaluating. It contains the environment, the definition map, and the list of
-- instructions that you are building up.
data TurtleState = TS {
    env :: Env,
    definitions :: [(DefName, Definition)],
    instructions :: [Instruction]
}

-- Constructs an empty state, given a definition map
emptyState :: [(DefName, Definition)] -> TurtleState
emptyState defs = TS { env = [], definitions = defs, instructions = [] }

{- Exercise 2 -}
evalUnOp :: UnaryOp -> Value -> Value
evalUnOp Neg (VInt i) = (VInt (negate i))
evalUnOp Not (VInt i) = (VInt (negate i))
evalUnOp Neg (VBool b) = (VBool (not b))
evalUnOp Not (VBool b) = (VBool (not b))
evalUnOp Neg VUnit = VUnit
evalUnOp Not VUnit = VUnit

evalBinOp :: BinaryOp -> Value -> Value -> Value
evalBinOp Add (VInt i1) (VInt i2) = VInt (i1 + i2)
evalBinOp Sub (VInt i1) (VInt i2) = VInt (i1 - i2)
evalBinOp Mul (VInt i1) (VInt i2) = VInt (i1 * i2)
evalBinOp Div (VInt i1) (VInt i2) = VInt (div i1 i2)
evalBinOp Eq (VInt i1) (VInt i2) = VBool (i1 == i2)
evalBinOp Eq (VBool b1) (VBool b2) = VBool (b1 == b2)
evalBinOp Neq (VInt i1) (VInt i2) = VBool (i1 /= i2)
evalBinOp Neq (VBool b1) (VBool b2) = VBool (b1 /= b2)
evalBinOp Greater (VInt i1) (VInt i2) = VBool (i1 > i2)
evalBinOp Less (VInt i1) (VInt i2) = VBool (i1 < i2)
evalBinOp GreaterEq (VInt i1) (VInt i2) = VBool (i1 >= i2)
evalBinOp LessEq (VInt i1) (VInt i2) = VBool (i1 <= i2)
evalBinOp And (VBool b1) (VBool b2) = VBool (b1 && b2)
evalBinOp Or (VBool b1) (VBool b2) = VBool (b1 || b2)
evalBinOp _ _ _ = undefined

{- Exercise 3 -}
addInstruction :: TurtleState -> Instruction -> TurtleState
addInstruction st i = st { instructions =  (instructions st) ++ [i] }

bind :: TurtleState -> Variable -> Value -> TurtleState
bind st var val = st { env = (var, val):(env st) }

lookupVar :: TurtleState -> Variable -> Value
lookupVar st var = getMaybeVal (lookup var (env st))

getMaybeVal :: Maybe a -> a
getMaybeVal x = 
    case x of
        Nothing -> undefined
        Just val -> val

lookupDef :: TurtleState -> DefName -> Definition
lookupDef st defName = getMaybeVal (lookup defName (definitions st))

{- Exercise 4 -}
evalExpr :: TurtleState -> Expr -> (Value, TurtleState)
evalExpr st (EInt i) = (VInt i, st)
evalExpr st (EUnit) = (VUnit, st)
evalExpr st (EBool b) = (VBool b, st)
evalExpr st (EVar v) = (lookupVar st v, st)
evalExpr st (EUnOp u e) = (val, st2)
    where
        (v, st2) = evalExpr st e
        val = evalUnOp u v
evalExpr st (EBinOp b e1 e2) = (val, st3)
    where
        (v1, st2) = evalExpr st e1
        (v2, st3) = evalExpr st2 e2
        val = evalBinOp b v1 v2
evalExpr st (EApp def args) = (vRes, defSt3)
    where
        (params, returnType, body) = lookupDef st def
        a = evalArgs st args
        e = constructEnv (params, returnType, body) a
        defSt = TS { env = e, definitions = definitions st, instructions = [] }
        (vRes, defSt2) = (evalExpr defSt body)
        defSt3 = addMultipleInstructions st (instructions defSt2)
evalExpr st (EIf cond e1 e2) = 
    if (fst (evalExpr st cond)) == VBool True 
        then evalExpr st e1 
        else evalExpr st e2
evalExpr st (EWhile cond e1) = 
    if (fst (evalExpr st cond)) == VBool True 
        then evalExpr st (EWhile cond e1) 
        else evalExpr st e1
evalExpr st (EChangeColor c) = (VUnit, st2)
    where
        st2 = addInstruction st (IChangeColor c)
evalExpr st (EMove d e) = (VUnit, st3)
    where
        (v, st2) = evalExpr st e
        i = getInt v
        i2 = checkDirection i d
        st3 = addInstruction st2 (IMove i2)
evalExpr st (ERotate d e) = (VUnit, st3)
    where
        (v, st2) = evalExpr st e
        i = getInt v
        i2 = checkRotateDirection i d
        st3 = addInstruction st2 (IRotate i2)
evalExpr st EPenDown = (VUnit, st2)
    where
        st2 = addInstruction st (IPenDown)
evalExpr st EPenUp = (VUnit, st2)
    where
        st2 = addInstruction st (IPenUp)
evalExpr st (ELet x e1 e2) = res
    where
        (v, st2) = evalExpr st e1
        st3 = bind st2 x v
        res = evalExpr st3 e2
evalExpr st (ESeq e1 e2) = res
    where
        (_, st2) = evalExpr st e1
        res = evalExpr st2 e2

addMultipleInstructions :: TurtleState -> [Instruction] -> TurtleState
addMultipleInstructions st [] = st
addMultipleInstructions st (x:xs) = addMultipleInstructions st2 xs
    where
        st2 = addInstruction st x

checkDirection :: Int -> Direction -> Int
checkDirection i d = if d == Backward then -i else i

checkRotateDirection :: Int -> RotateDirection -> Int
checkRotateDirection i d = if d == RotateLeft then -i else i

getInt :: Value -> Int
getInt (VInt i) = i
getInt _ = undefined

constructEnv :: Definition -> [(Value, TurtleState)] -> Env
constructEnv (x:xs, r, b) (y:ys) = (fst (x), fst (y)):(constructEnv (xs, r, b) ys)
constructEnv _ _ = []

evalArgs :: TurtleState -> [Expr] -> [(Value, TurtleState)]
evalArgs st (x:xs) = (evalExpr st x):(evalArgs st xs)
evalArgs _ [] = []

-- External function
eval :: Program -> (Value, [Instruction])
eval (defs, e) = (v, instructions st2)
    where
        st = emptyState defs
        (v, st2) = evalExpr st e