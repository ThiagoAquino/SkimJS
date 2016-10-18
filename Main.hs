import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad hiding (empty)
import Control.Applicative hiding (empty)
import Data.Map as Map
import Debug.Trace
import Value
--import Data.IntMap.Strict as Strict (intersection, difference)
--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (StringLit word) = return $ String word
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    stateLookup env var -- crashes if the variable doesn't exist
    e <- evalExpr env expr
    setVar var e

-- eval Lists
evalExpr env (ArrayLit []) = return (List [])
evalExpr env (ArrayLit (a:as)) = do
    head <- evalExpr env a
    List tail <- evalExpr env (ArrayLit as)
    return (List (head:tail))

--
evalExpr env (BracketRef expr i) = do
    list <- evalExpr env expr
    case list of
        (List l) -> do
            index <- evalExpr env i
            getFromIndex env list index

-- chamada de função(CallExpr que recebe um nome e parametro) do tipo "lista.head(); e imprime a cabeça de lista" (também usado para demais funcoes)
evalExpr env (CallExpr expr args) = do
    res <- evalExpr env (expr)
    case res of
        (Error _) -> error ("Funcao nao declarada: " ++ (show expr))

-- Lista Tiago
        (Function (Id "head") _ _) -> do
            list <- evalExpr env (head args)
            case list of
                (List []) -> error ("Lista vazia.")
                (List (x:xs)) -> return x
        (Function (Id "tail") _ _) -> do
            list <- evalExpr env (head args)
            case list of
                (List []) -> error ("Lista vazia.")
                (List (x:xs)) -> return (List xs)
        (Function (Id "concat") _ _) -> do
            case args of
                [] -> return (List [])
                (x:xs) -> do
                    (List list) <- evalExpr env x
                    (List fim) <- evalExpr env (CallExpr expr xs)
                    return (List (list++fim))

---------------------------------------------
--
-- Function--
--
        (Function name argsName stmts) -> ST $ \s ->
            let (ST f) = mapM (evalExpr env) args
                (ST t) = auxiliar1 env (BlockStmt stmts)
                (_, apenasLocal) = t s
                (ST x) = auxiliar apenasLocal (BlockStmt stmts)
                (_, autoGlobal) = x s
                (params, _) = f s
                parametros = fromList (zip (Prelude.map (\(Id a) -> a) argsName) (params))
                local = union parametros s
                (ST g) = evalStmt env (BlockStmt stmts)
                (val, estadoFinal) = g local
            in do
            case val of
                (Return ret) -> (ret, union (intersection (difference estadoFinal parametros) autoGlobal) (intersection autoGlobal estadoFinal))
                _ -> (val, union (intersection (difference estadoFinal parametros) autoGlobal) (intersection autoGlobal estadoFinal))


-- Lista Eduardo
{-        (DotRef listName (Id "head")) -> do                         -- funcao head() que retorna a cabeça da lista avaliada evalList
            (List evalList) <- evalExpr env listName
            return (head(evalList))
        (DotRef listName (Id "tail")) -> do                         -- uncao tail() que retorna a tail da lista avaliada evalList
            (List evalList) <- evalExpr env listName
            return (List (tail(evalList)))
        (DotRef listName (Id "concat")) -> do                       -- funcao list.concat(x) que que concatena list++x
            (List evalList) <- evalExpr env listName
            argsList <- evalExpr env (head args)                    -- so pega o primeiro parametro ja que args pode ser uma lista de expression (so concatena uma lista por vez)
            case argsList of
                (List list) -> return (List (evalList ++ list))     -- args é uma lista
                value -> return (List (evalList ++ [value]))        -- args é um valor x e nao uma lista
-}
--------------------------------------------------------------------

auxiliar :: StateT -> Statement -> StateTransformer Value
auxiliar env (BlockStmt []) = return Nil
auxiliar env (BlockStmt ((ExprStmt (AssignExpr OpAssign (LVar var) expr)):xs)) = do
    v <- stateLookup env var
    case v of
    -- Variable not defined :( we'll create it!
        (Error _) -> do
            evalStmt env (VarDeclStmt [(VarDecl (Id var) (Nothing))])
            auxiliar env (BlockStmt xs)
        _ -> auxiliar env (BlockStmt xs)
auxiliar env (BlockStmt (x:xs)) = do
    case x of
        (IfStmt expr ifBlock elseBlock) -> do
            auxiliar env ifBlock
            auxiliar env elseBlock
            auxiliar env (BlockStmt xs)
        (IfSingleStmt expr ifBlock) -> do
            auxiliar env ifBlock
            auxiliar env (BlockStmt xs)
        (ForStmt initialize expr1 expr2 stmt) -> do
            case initialize of
                (ExprInit e) -> do
                    auxiliar env (BlockStmt [ExprStmt e])
                    auxiliar env stmt
                    auxiliar env (BlockStmt xs)
                _ -> do
                    auxiliar env stmt
                    auxiliar env (BlockStmt xs)
        (ExprStmt (CallExpr nameExp args)) -> do
            res <- evalExpr env (nameExp)
            case res of
                (Error _) -> auxiliar env (BlockStmt xs)
                (Function name argsName stmts) -> do
                    auxiliar env (BlockStmt stmts)
                    auxiliar env (BlockStmt xs)
        _ -> auxiliar env (BlockStmt xs)

auxiliar1 :: StateT -> Statement -> StateTransformer Value
auxiliar1 env (BlockStmt []) = return Nil
auxiliar1 env (VarDeclStmt []) = return Nil
auxiliar1 env (VarDeclStmt (decl:ds)) = do
    varDecl env decl
    auxiliar1 env (VarDeclStmt ds)
auxiliar1 env (BlockStmt (x:xs)) = do
    case x of
        (IfStmt expr ifBlock elseBlock) -> do
            auxiliar1 env ifBlock
            auxiliar1 env elseBlock
            auxiliar1 env (BlockStmt xs)
        (IfSingleStmt expr ifBlock) -> do
            auxiliar1 env ifBlock
            auxiliar1 env (BlockStmt xs)
        (ForStmt initialize expr1 expr2 stmt) -> do
            auxiliar1 env stmt
            auxiliar1 env (BlockStmt xs)
        (VarDeclStmt (y:ys)) -> do
            varDecl env y
            auxiliar1 env (BlockStmt xs)
        (ExprStmt (CallExpr nameExp args)) -> do
            res <- evalExpr env (nameExp)
            case res of
                (Error _) -> auxiliar1 env (BlockStmt xs)
                (Function name argsName stmts) -> do
                    auxiliar1 env (BlockStmt stmts)
                    auxiliar1 env (BlockStmt xs)
        _ -> auxiliar1 env (BlockStmt xs)

listVarDecl :: [Id] -> [Expression] -> [VarDecl]
listVarDecl (x:xs) (y:ys) = (VarDecl x (Just y)):(listVarDecl xs ys)
listVarDecl [] [] = []

evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr
--
-- BreakStmt--
--
evalStmt env (BreakStmt tipo) = return Break
--
-- ReturnStmt--
--
evalStmt env (ReturnStmt expression) = do
      case expression of
         (Nothing) -> return (Return Nil)
         (Just expr) -> do
            exprEval <- evalExpr env expr
            return exprEval
--
-- ContinueStmt--
--
evalStmt env (ContinueStmt Nothing) = return Continue;
--
-- WhileStmt--
--
evalStmt env (WhileStmt expressao whileBock) = do
    val <- evalExpr env expressao                                               -- Avalia a expressão
    case val of
        (Bool b) -> if b then do                                                -- Se a expressão for booleana
                        ehbreak <- evalStmt env whileBock                       -- Avalia se o comando é um break
                        case ehbreak of
                            Break -> return Nil                                 -- Se for, para a execução do programa
                            _ -> evalStmt env (WhileStmt expressao whileBock)   -- Se for qualqer outra coisa, executa e chama o bloco novamente
                    else return Nil

--
-- DoWhileStmt--
--
evalStmt env (DoWhileStmt dowhileBock expressao) = do
    ehbreak <- evalStmt env dowhileBock
    case ehbreak of
            Break -> return Nil
            _ -> do evalStmt env dowhileBock
                    val <- evalExpr env expressao                                               -- Avalia a expressão
                    case val of
                        (Bool b) -> if b then do
                                        evalStmt env (DoWhileStmt dowhileBock expressao)
                                    else return Nil
--
-- Chamada da função--
--
evalStmt env (FunctionStmt name args body) = funcDecl env (name, args, body)


--
-- BlocoStmt(x:xs)--
--
evalStmt env (BlockStmt (a:[])) = evalStmt env a
evalStmt env (BlockStmt (a:as)) =
    case a of
        (BreakStmt _) -> evalStmt env a
        (ReturnStmt _) -> evalStmt env a
        _ -> do evalStmt env a
                evalStmt env (BlockStmt as)

--
-- IfSingleStmt--
--
evalStmt env (IfSingleStmt expr ifBlock) = do
    ret <- evalExpr env expr
    case ret of
        (Bool b) -> if b == True then evalStmt env ifBlock else evalStmt env EmptyStmt
        error@(Error _) -> return error

--
-- IfStmt--
--
evalStmt env (IfStmt expr ifBlock elseBlock) = do
    ret <- evalExpr env expr
    case ret of
        (Bool b) -> if b then evalStmt env ifBlock else evalStmt env elseBlock
        error@(Error _) -> return error
--
-- ForStmt--
--
evalStmt env(ForStmt inicio expressao incremento forBlock) = do
    evalInit env inicio
    case expressao of
        (Just a) -> do
            val <- evalExpr env a
            case val of
                (Bool b) -> do
                    if b then do
                        eval <- evalStmt env forBlock
                        case eval of
                            Break -> return Nil
                            _ -> do
                                case incremento of

                                    (Just expr) -> do
                                                evalExpr env expr
                                                evalStmt env (ForStmt NoInit expressao incremento forBlock)
                                    (Nothing) -> evalStmt env (ForStmt NoInit expressao incremento forBlock)


                    else evalStmt env EmptyStmt
        (Nothing) -> do
            eval <- evalStmt env forBlock
            case eval of
                Break -> return Nil
                _ -> do
                    case incremento of
                        (Just expr) -> evalExpr env expr
                        (Nothing) -> return Nil
                    evalStmt env (ForStmt NoInit expressao incremento forBlock)
--
-- FOR INIT--
--
evalInit env (NoInit) = return Nil
evalInit env (VarInit a) = (evalStmt env (VarDeclStmt a))
evalInit env (ExprInit b) = (evalExpr env b)


-- x = lista[i]
getFromIndex env (List []) (Int i) = error ("Null Point Exception KKKK")
getFromIndex env (List (x:xs)) (Int i) | i == 0 = return x
                                       | otherwise = getFromIndex (env) (List xs) (Int (i-1))
--
---------------------------------------------------------------------------------------
--

-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ Prelude.map (evalStmt env) stmts

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2
infixOp env OpEq   (List v1) (List v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (List v1) (List v2) = return $ Bool $ v1 /= v2

--
-- Environment and auxiliary functions
--

environment :: Map String Value
environment = insert "concat" (Function (Id "concat") [(Id "list1"), (Id "list2")] []) $ insert "tail" (Function (Id "tail") [Id "list"] []) $ insert "head" (Function (Id "head") [Id "list"] []) empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    case Map.lookup var (union s env) of
        Nothing -> error $ "Variable " ++ show var ++ " not defiend."
        Just val -> (val, s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

funcDecl :: StateT -> (Id, [Id], [Statement]) -> StateTransformer Value
funcDecl env ((Id id), args, stmts) = do
     setFunc id (Function (Id id) args stmts)

setFunc :: String -> Value -> StateTransformer Value
setFunc name desc = ST $ \s -> (desc, insert name desc s)

--
-- Types and boilerplate
--

type StateT = Map String Value
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, defs) =
    show val ++ "\n" ++ show (toList $ union defs environment) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f Map.empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
