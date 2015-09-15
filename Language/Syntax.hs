module Language.Syntax where

{- En el lenguaje de programación
   tendremos dos tipos: Booleanos y Enteros.
-}
data Type = BoolT | IntT
    deriving Show

{- Variables con su tipo. -}

type VarName = String
    
data Var = Var String Type
    
{- Expresiones enteras -}
data IntExpr = ConstI Int              -- Constantes
             | VI VarName              -- Variables enteras
             | Neg IntExpr             -- Negación
             | Plus IntExpr IntExpr    -- Suma
             | Prod IntExpr IntExpr    -- Producto
             | Div IntExpr IntExpr     -- División
             | Mod IntExpr IntExpr     -- Módulo

data BoolExpr = ConstB Bool             -- Constantes
              | VB VarName             -- Variables booleanas
              | And BoolExpr BoolExpr  -- Conjunción
              | Or BoolExpr BoolExpr   -- Disjunción
              | Not BoolExpr           -- Negación
              | Equal IntExpr IntExpr    -- Menor o igual
              | Less IntExpr IntExpr   -- Menor estricto
             
-- Las expresiones podrán ser enteras o booleanas
data Expr = IExpr IntExpr 
          | BExpr BoolExpr

{- 
    Sentencias del lenguaje
-}
data Statement = Skip                      -- No hacer nada
               | Assign Var Expr           -- Asignación de variable
               | Seq Statement Statement   -- Secuencia
               | If [(BoolExpr,Statement)] -- Condicional
               | Do BoolExpr Statement     -- Ciclo
