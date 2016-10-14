import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty)
import Debug.Trace
import Value

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

-- chamada de função(CallExpr que recebe um nome e parametro) do tipo "lista.head(); e imprime a cabeça de lista"
evalExpr env (CallExpr expr args) = do
    case expr of
        (DotRef listName (Id "head")) -> do                         -- funcao head() que retorna a cabeça da lista avaliada evalList
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
        _ -> error "Falta Continuar outras funcoes (nao sei)"       -- outros casos em que nao sao funcoes reservadas de lista


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
evalStmt env (ReturnStmt Nothing) = return Nil
evalStmt env (ReturnStmt (Just expr)) = evalExpr env expr
--
-- ContinueStmt--
--
evalStmt env (ContinueStmt Nothing) = return Continue;

--
-----------------------BLOCO WHILE--------------------------------------------------
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
---------------------------------------------------------------------------------------
--

--
-----------------------BLOCO DOWHILE--------------------------------------------------
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
--------------------------------BLOCO RETURN-------------------------------------------------------
--

evalStmt env (ReturnStmt expression) = do
     case expression of
        (Nothing) -> return Nil
        (Just expr) -> do
            exprEval <- evalExpr env expr
            return exprEval

--
--------------------------------BLOCO FUNCAO-------------------------------------------------------
--

evalStmt env (FunctionStmt name args body) = do
    funcDecl env (name, args, body)
--
------------------------  Bloco IF ----------------------------------------------
--

-----------------BlockStmt (x:xs)--------------------------

evalStmt env (BlockStmt (a:[])) = evalStmt env a
evalStmt env (BlockStmt (a:as)) =
    case a of
        (BreakStmt _) -> evalStmt env a
        (ReturnStmt _) -> evalStmt env a
        _ -> do evalStmt env a
                evalStmt env (BlockStmt as)

---------------------BLOCO IF------------------------------
evalStmt env (IfSingleStmt expr ifBlock) = do
    ret <- evalExpr env expr
    case ret of
        Bool b -> if b == True then evalStmt env ifBlock else evalStmt env EmptyStmt

--------------------BLOCO IF/ELSE------------------------
evalStmt env (IfStmt expr ifBlock elseBlock) = do
    ret <- evalExpr env expr
    case ret of
        (Bool b) -> if b then evalStmt env ifBlock else evalStmt env elseBlock

--
---------------------------------------------------------------------------------------
--

--
---------------------------------------BLOCO FOR---------------------------------------
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



------------------------FORINIT----------------------------
evalInit env (NoInit) = return Nil
evalInit env (VarInit a) = (evalStmt env (VarDeclStmt a))
evalInit env (ExprInit b) = (evalExpr env b)

--
---------------------------------------------------------------------------------------
--




-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts

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
environment = Map.empty

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
