-- | Parser del Lenguaje extendido de LISA.
module HGUI.Parser where

import qualified Data.Map as M
import Control.Applicative ( (<$>) )

-- Imports Parsec
import Text.Parsec 

-- Imports de Equ
import qualified Equ.Parser as PEqu

-- Imports de Hal
import Hal.Parser

-- Imports de Hal-Gui
import HGUI.ExtendedLang

-- *** Comandos.
-- |Comando simples.
extSingle :: String -> (CommPos -> ExtComm) -> ParserH ExtComm
extSingle s ec = try $ do
              st <- getParserState
              let initp = statePos st
              _ <- sym s
              st' <- getParserState
              let endp = statePos st'

              _ <- semip
              return $ ec $ makeCommPos initp endp

-- | Skip
extSkip :: ParserH ExtComm
extSkip = extSingle "skip" ExtSkip

-- | Abort
extAbort :: ParserH ExtComm
extAbort = extSingle "abort" ExtAbort

-- |Asignación.
extAssignInt :: ParserH ExtComm
extAssignInt = try $ do
               st <- getParserState
               let initp = statePos st
               
               acc <- pintvar  
               oper ":="
               iexp <- intexp 
               
               st' <- getParserState
               let endp = statePos st'

               _ <- semip
               return $ ExtIAssig (makeCommPos initp endp) acc iexp

extAssignBool :: ParserH ExtComm
extAssignBool = try $ do
                st <- getParserState
                let initp = statePos st
                
                acc <- pboolvar
                oper ":="
                bexp <- boolexp 
                
                st' <- getParserState
                let endp = statePos st'
                
                _ <- semip
                return $ ExtBAssig (makeCommPos initp endp) acc bexp

-- |Condicional.      
extIfthen :: ParserH ExtComm
extIfthen = try $ do
            keyword "if" 

            inib <- statePos <$> getParserState
            b <- (boolexp <?> "Expresión booleana")
            endb <- statePos <$> getParserState

            keyword "->"
            c <- extComm

            cs <- many (try $ keyword "|" >>
                              statePos <$> getParserState >>= \inibi ->
                              boolexp >>= \bi ->
                              statePos <$> getParserState >>= \endbi ->
                              keyword "->" >>
                              extComm >>= \ci ->
                              return (makeCommPos inibi endbi, bi,ci)
                       )
            
            keyword "fi"
            
            return $ ExtIf (makeCommPos inib endb) 
                           ((makeCommPos inib endb,b,c):cs)

-- | Assert
extAssert :: ParserH ExtComm
extAssert = try $ do
            st <- getParserState
            let initp = statePos st
            
            f <- formfun
            
            st' <- getParserState
            let endp = statePos st'
            return $ ExtAssert (makeCommPos initp endp) f

-- | Do - While
extWhile :: ParserH ExtComm
extWhile = try $ do
           whites
           keyword "do"
           
           inib <- statePos <$> getParserState
           b <- boolexp
           endb <- statePos <$> getParserState
           
           keyword "->"
           c <- extComm
           keyword "od"
           
           return (ExtDo (makeCommPos inib endb) true b c)

-- | Comandos del lenguaje LISA
extComms :: ParserH ExtComm
extComms = choice $ map try
           [ extSkip
           , extAbort
           , extAssignInt
           , extAssignBool
           , extAssert
           , extIfthen
           , extWhile
           ]

extComm :: ParserH ExtComm
extComm = try $ whites >> many1 extComms >>= return . foldl1 ExtSeq

-- | Un programa consta de declaraciones de variables, una precondición,
--   un comando y una postcondición 
extProgram :: ParserH ExtProgram
extProgram = varinputs >>
             vardefs >>
             extComm >>= \c ->
             eof >>
             M.elems . pvars . stateUser <$> getParserState >>= \vars ->
             return $ ExtProg vars c

-- | Función principal de parseo desde String
parseExtPrgFromString :: String -> Either ParseError ExtProgram
parseExtPrgFromString = runParser extProgram initSt "" 
    where initSt = PHalState { lvars = M.empty
                             , pvars = M.empty
                             , equPState = PEqu.initPExprState PEqu.UnusedParen
                             }

parseExtPrgFromFile :: FilePath -> IO ExtProgram
parseExtPrgFromFile f = 
    readFile f >>= return . parseExtPrgFromString >>=
    either (error . show) return

parseExtPrgFromFile' :: FilePath -> IO (Either ParseError ExtProgram)
parseExtPrgFromFile' f = readFile f >>= return . parseExtPrgFromString 
