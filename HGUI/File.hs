module HGUI.File where

import Graphics.UI.Gtk hiding (get)

import Control.Monad.Trans.RWS
import Control.Monad.IO.Class
import Control.Monad

import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Foldable as F
import System.FilePath.Posix
import Data.Maybe (fromMaybe,isJust,fromJust)
import Data.Text hiding (take,init,drop)

import HGUI.GState
import HGUI.TextPage
import HGUI.Utils

import Hal.Parser(parseFromString)

import Lens.Family

-- | En general, salvo aclaración, un archivo en este contexto es directamente
-- un campo de texto con su respectivo nombre en la interfaz.

-- | Crea un campo de texto al realizar una carga por archivo.
createNewFileFromLoad :: Maybe TextFilePath -> Maybe String -> 
                         GuiMonad ()
createNewFileFromLoad mfp mcode = getHGState >>= \st -> ask >>= \content ->
        do
        unless (st ^. (gHalTextPage . isSave)) saveFile
        updateHGState ((<~) gHalTextPage (HalTextPage mfp True))
        createTextPage mcode

-- | Crea un nuevo archivo en blanco.
createNewFile :: GuiMonad ()
createNewFile = createNewFileFromLoad Nothing Nothing

genProofObligations :: GuiMonad ()
genProofObligations = return ()

-- | Función para cargar un archivo.
openFile :: GuiMonad ()
openFile = ask >>= \ct -> get >>= \st ->
           io $ dialogLoad "Cargar programa" halFileFilter (openFile' ct st) >>
            return ()
    where
        openFile' :: HGReader -> HGStateRef -> Maybe TextFilePath ->
                     Maybe String -> IO ()
        openFile' content st mfp mcode = 
            evalRWST (createNewFileFromLoad mfp mcode) content st >> 
            return ()

-- | Dialogo general para la carga de archivos.
dialogLoad :: String -> (FileChooserDialog -> IO ()) -> 
              (Maybe TextFilePath -> Maybe String -> IO ()) -> 
              IO Bool
dialogLoad label fileFilter action = do
    dialog <- fileChooserDialogNew (Just label) 
                                    Nothing 
                                    FileChooserActionOpen
                                    [ ("Cargar",ResponseAccept)
                                    , ("Cancelar",ResponseCancel)]

    fileFilter dialog 
    response <- dialogRun dialog
    
    case response of
        ResponseAccept -> do
            selected <- fileChooserGetFilename dialog
            F.mapM_ (\filepath -> 
                    readFile filepath >>= \code ->
                    action (Just $ pack filepath) (Just code) >>
                    widgetDestroy dialog) selected 
            return True
        _ -> widgetDestroy dialog >> return False

-- | Generador de filtros para la carga y guardado de archivos.
setFileFilter :: FileChooserClass f => f -> [String] -> String -> IO ()
setFileFilter fChooser patterns title = do
                                hsfilt <- fileFilterNew
                                mapM_ (fileFilterAddPattern hsfilt) patterns
                                fileFilterSetName hsfilt title
                                fileChooserAddFilter fChooser hsfilt    

-- | Guardado directo de un archivo.
saveFile :: GuiMonad ()
saveFile = getHGState >>= \st -> ask >>= \content ->
        case st ^. (gHalTextPage . fileName) of
            Nothing -> return ()
            Just fp -> do
                       code <- getCode
                       save (unpack fp) code
    where
        save:: FilePath -> String -> GuiMonad ()
        save filepath code = io $ writeFile filepath code

-- | Guardado en, de un archivo.
saveAtFile :: GuiMonad ()
saveAtFile = getHGState >>= \st -> ask >>= \content ->
             do
             let nFile = maybe "" (takeBaseName . unpack) (st ^. (gHalTextPage . fileName))
             code <- getCode
             
             mfp <- saveDialog "Guardar programa" (nFile++".lisa") halFileFilter code
             when (isJust mfp) (updateFL $ fromJust mfp)
             return ()
    where
        updateFL :: FilePath -> GuiMonad ()
        updateFL fp = updateHGState ((<~) gHalTextPage (HalTextPage (Just $ pack fp) True))

-- | Dialogo general para guardar un archivo.
saveDialog :: String -> String -> (FileChooserDialog -> IO ()) -> 
              String -> GuiMonad (Maybe FilePath)
saveDialog label filename fileFilter serialItem = do
        dialog <- io $ fileChooserDialogNew (Just label) 
                                            Nothing 
                                            FileChooserActionSave 
                                            [ ("Guardar",ResponseAccept)
                                            , ("Cancelar",ResponseCancel)
                                            ]
        
        io $ fileChooserSetCurrentName dialog filename
        io $ fileFilter dialog
        response <- io $ dialogRun dialog

        case response of
            ResponseAccept -> io (fileChooserGetFilename dialog) >>= 
                              \fp -> F.mapM_ save fp >> 
                              io (widgetDestroy dialog) >> 
                              return fp
            _ -> io (widgetDestroy dialog) >> return Nothing
    where
        save:: FilePath -> GuiMonad ()
        save filepath = io $ writeFile filepath serialItem

-- | Filtro de programas de fun.
halFileFilter :: (FileChooserClass f, MonadIO m) => f -> m ()
halFileFilter dialog = io $ setFileFilter dialog ["*.lisa"] "Programa de hal"


compile :: GuiMonad ()
compile = get >>= \ref -> ask >>= \content ->
        do
        code <- getCode
        case parseFromString code of
            Left er -> return ()
            Right prg -> updateHGState ((<~) gHalPrg (Just prg))


