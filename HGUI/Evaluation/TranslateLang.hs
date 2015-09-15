{-# LANGUAGE OverloadedStrings #-}
module HGUI.Evaluation.TranslateLang where

import HGUI.ExtendedLang
import Hal.Lang

import qualified Language.Syntax as ASyntax
import qualified Language.Semantics as ASem

import qualified Data.Map as M
import Data.Text ( unpack )

type VarToId = M.Map ASyntax.VarName Identifier


-- Traduce del lenguaje del proyecto al ExtendedLanguage
syntaxToBE :: ASyntax.BoolExpr -> VarToId -> BExp
syntaxToBE (ASyntax.ConstB b) _  = BCon b
syntaxToBE (ASyntax.VB v)     m  = BoolId (lookupVar v m)
syntaxToBE (ASyntax.And b1 b2) m = 
    let (b1',b2') = (syntaxToBE b1 m, syntaxToBE b2 m)
    in
        BBOp And b1' b2'
syntaxToBE (ASyntax.Or b1 b2) m =
    let (b1',b2') = (syntaxToBE b1 m, syntaxToBE b2 m)
    in
        BBOp Or b1' b2'
syntaxToBE (ASyntax.Not b) m =
    let b' = syntaxToBE b m
    in
        BUOp Not b'
syntaxToBE (ASyntax.Equal e1 e2) m =
    let (e1',e2') = (syntaxToIE e1 m, syntaxToIE e2 m)
    in
        BRel Equal e1' e2'
syntaxToBE (ASyntax.Less e1 e2) m =
    let (e1',e2') = (syntaxToIE e1 m, syntaxToIE e2 m)
    in
        BRel Lt e1' e2'
        
syntaxToIE :: ASyntax.IntExpr -> VarToId -> Exp
syntaxToIE (ASyntax.ConstI i) m = ICon i
syntaxToIE (ASyntax.VI v)     m = IntId (lookupVar v m)
syntaxToIE (ASyntax.Neg e) m    = 
    let e' = syntaxToIE e m
    in
        IBOp Substr (ICon 0) e'
syntaxToIE (ASyntax.Plus e1 e2) m =
    let (e1',e2') = (syntaxToIE e1 m, syntaxToIE e2 m)
    in
        IBOp Plus e1' e2'
syntaxToIE (ASyntax.Prod e1 e2) m =
    let (e1',e2') = (syntaxToIE e1 m, syntaxToIE e2 m)
    in
        IBOp Times e1' e2'
syntaxToIE (ASyntax.Div e1 e2) m =
    let (e1',e2') = (syntaxToIE e1 m, syntaxToIE e2 m)
    in
        IBOp Div e1' e2'
syntaxToIE (ASyntax.Mod e1 e2) m =
    let (e1',e2') = (syntaxToIE e1 m, syntaxToIE e2 m)
    in
        IBOp Mod e1' e2'
      
lookupVar :: ASyntax.VarName -> VarToId -> Identifier
lookupVar v m = 
    maybe (error $ "La variable " ++ v ++ " no existe.")
          id (M.lookup v m)
      
      
-- Chequea que el comando que se devuelve desde el proyecto
-- corresponda con el que deberia ser.
syntaxToEC :: ASyntax.Statement -> ExtComm -> VarToId -> ExtComm
syntaxToEC ASyntax.Skip (ExtSkip p) m = ExtSkip p
syntaxToEC (ASyntax.Assign (ASyntax.Var v ASyntax.BoolT) (ASyntax.BExpr e)) 
           (ExtBAssig p i be) m = 
    ExtBAssig p (lookupVar v m) (syntaxToBE e m)
syntaxToEC (ASyntax.Assign (ASyntax.Var v ASyntax.IntT) (ASyntax.IExpr e)) 
           (ExtIAssig p _ _) m = 
    ExtIAssig p (lookupVar v m) (syntaxToIE e m)
syntaxToEC (ASyntax.Seq s1 s2) (ExtSeq s1' s2') m =
    ExtSeq (syntaxToEC s1 s1' m) (syntaxToEC s2 s2' m)
syntaxToEC (ASyntax.Do be s) (ExtDo p f be' s') m =
    ExtDo p f (syntaxToBE be m) (syntaxToEC s s' m)
syntaxToEC _ _ _ = error ("La definición de continuación de ejecución "
                        ++ "no es la correcta.")


-- Traducción del lenguaje de Hal al del proyecto                        
beToSyntax :: BExp -> ASyntax.BoolExpr
beToSyntax (BoolId i)  = ASyntax.VB (unpack $ idName i)
beToSyntax (BCon b)    = ASyntax.ConstB b
beToSyntax (BBOp op b1 b2) = 
    case op of
         And -> ASyntax.And (beToSyntax b1) (beToSyntax b2)
         Or  -> ASyntax.Or (beToSyntax b1) (beToSyntax b2)
beToSyntax (BUOp Not b) = ASyntax.Not (beToSyntax b)
beToSyntax (BRel rel e1 e2) =
    case rel of
         Equal -> ASyntax.Equal (ieToSyntax e1) (ieToSyntax e2)
         Lt    -> ASyntax.Less (ieToSyntax e1) (ieToSyntax e2)

ieToSyntax :: Exp -> ASyntax.IntExpr
ieToSyntax (IntId i) = ASyntax.VI (unpack $ idName i)
ieToSyntax (ICon e)  = ASyntax.ConstI e
ieToSyntax (IBOp op e1 e2) =
    let sop = case op of
                  Plus   -> ASyntax.Plus 
                  Times  -> ASyntax.Prod
                  Substr -> (\ie1 -> ASyntax.Plus ie1 . ASyntax.Neg)
                  Div    -> ASyntax.Div
                  Mod    -> ASyntax.Mod
    in
        sop (ieToSyntax e1) (ieToSyntax e2)
                     
                     
ecToSyntax :: ExtComm -> ASyntax.Statement
ecToSyntax (ExtSkip _) = ASyntax.Skip
ecToSyntax (ExtBAssig _ i e) = 
    ASyntax.Assign (ASyntax.Var vari ASyntax.BoolT) (ASyntax.BExpr $ beToSyntax e)
    where vari = unpack $ idName i
ecToSyntax (ExtIAssig _ i e) = 
    ASyntax.Assign (ASyntax.Var vari ASyntax.IntT) (ASyntax.IExpr $ ieToSyntax e)
    where vari = unpack $ idName i
ecToSyntax (ExtDo _ _ be s) =
    ASyntax.Do (beToSyntax be) (ecToSyntax s)
ecToSyntax _ = error "No existe esta sentencia en la sintaxis del proyecto"
            