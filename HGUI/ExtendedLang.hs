-- | Módulo de la sintáxis extendida del lenguaje imperativo simple con anotaciones (LISA).
-- En esta versión guardamos ademas la información sobre los números de lineas
-- de los comandos.
{-# Language GADTs #-}
module HGUI.ExtendedLang where

import Hal.Lang

import qualified Equ.Expr as Equ ( Expr(..) )
import qualified Equ.PreExpr as PreEqu ( PreExpr'(Con) )
import qualified Equ.Theories.FOL as TheoEqu ( folTrue )


import Text.Parsec.Pos

data CommPos = CommPos { begin :: SourcePos
                       , end   :: SourcePos
                       }
    deriving (Eq,Show)


true :: Equ.Expr
true = Equ.Expr $ PreEqu.Con TheoEqu.folTrue

makeCommPos :: SourcePos -> SourcePos -> CommPos
makeCommPos = CommPos

initPos :: CommPos
initPos = CommPos (initialPos "") (initialPos "")

takeCommLine :: ExtComm -> Int
takeCommLine (ExtSkip   pos)       = sourceLine $ begin pos
takeCommLine (ExtAbort  pos)       = sourceLine $ begin pos
takeCommLine (ExtPre    pos _)     = sourceLine $ begin pos
takeCommLine (ExtAssert pos _)     = sourceLine $ begin pos
takeCommLine (ExtIf     pos _)     = sourceLine $ begin pos
takeCommLine (ExtIAssig pos _ _)   = sourceLine $ begin pos
takeCommLine (ExtBAssig pos _ _)   = sourceLine $ begin pos
takeCommLine (ExtDo     pos _ _ _) = sourceLine $ begin pos
takeCommLine (ExtSeq c _)         = takeCommLine c

getCommLines :: ExtComm -> [Int]
getCommLines (ExtSeq c c')        = getCommLines c ++ getCommLines c'
getCommLines cif@(ExtIf _ cs)     = takeCommLine cif :
                                    (concat $ map (getCommLines . snd) cs)
getCommLines cdo@(ExtDo _ _ _ c)  = takeCommLine cdo : getCommLines c
getCommLines c                    = [takeCommLine c]

-- Los terminos que representan los comandos con la información extra
-- sobre en que linea se encuentran.
data ExtComm where
    ExtSkip   :: CommPos -> ExtComm
    ExtAbort  :: CommPos -> ExtComm
    
    ExtPre    :: CommPos -> FormFun -> ExtComm
    ExtAssert :: CommPos -> FormFun -> ExtComm
    
    ExtIf     :: CommPos -> [(BExp,ExtComm)] -> ExtComm
    
    ExtIAssig :: CommPos -> Identifier -> Exp -> ExtComm
    ExtBAssig :: CommPos -> Identifier -> BExp -> ExtComm
    
    ExtDo     :: CommPos -> FormFun -> BExp -> ExtComm -> ExtComm
    ExtSeq    :: ExtComm -> ExtComm -> ExtComm
    deriving Show

data ExtProgram where
    ExtProg :: LIdentifier -> ExtComm -> ExtProgram
