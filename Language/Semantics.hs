module Language.Semantics where

import Language.Syntax
import Language.ListAssoc

-- Asignación de valores para las variables enteras
type StateI = ListAssoc VarName Int
-- Asignación de valores para las variables booleanas
type StateB = ListAssoc VarName Bool

defaultIntValue :: Int
defaultIntValue = 0

defaultBoolValue :: Bool
defaultBoolValue = True

-- Tipo que representa la continuación de un paso de ejecución.
-- Ésta puede ser: Falta ejecutar una sentencia (ToExec), o ya no hay nada por
-- ejecutar (Finish).
data Continuation = ToExec Statement
                  | Finish


-- El estado consta del valor de las variables enteras y las booleanas
type State = (StateI,StateB)


evalIExpr :: IntExpr -> StateI -> Int
evalIExpr = undefined

-- Para evaluar las expresiones booleanas
-- necesitamos también el estado de variables enteras
-- porque en Equal y Less tenemos subexpresiones enteras.
evalBExpr :: BoolExpr -> State -> Bool
evalBExpr = undefined

-- Evaluar un paso de ejecución en un programa.
evalStep :: Statement -> State -> (State , Continuation)
evalStep = undefined
