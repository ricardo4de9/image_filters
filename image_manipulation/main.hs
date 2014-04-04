module Main where

import Graphics.GD 
import qualified Graphics.Filters.GD as Filter
import Graphics.UI.Gtk
import System.FilePath.Posix (takeExtension)

main = do
    initGUI
    builder <- builderNew
    builderAddFromFile builder "pf.glade"
    mainWindow <- builderGetObject builder castToWindow "Proyecto Final - CI4251"
    
    --  Asociamos la imagen con un archivo
    imagenW <- builderGetObject builder castToImage "image1"

    -- Asignandole funcionalidad a cada boton
    boton1 <- builderGetObject builder castToButton "button1"
    boton2 <- builderGetObject builder castToButton "button2"
    boton3 <- builderGetObject builder castToButton "button3"

    botonOpen   <- builderGetObject builder castToImageMenuItem "imagemenuitem2"
    botonSaveAs <- builderGetObject builder castToImageMenuItem "imagemenuitem4"
    botonQuit   <- builderGetObject builder castToImageMenuItem "imagemenuitem5"

    on botonOpen menuItemActivate $ do
        archivo <- openOpenFileDialog mainWindow
        case archivo of
            Nothing   -> return ()
            Just a    -> do
               putStrLn $ "El archivo de imagen es :" ++ a
               let ext = takeExtension a
               image <- case ext of 
                   ".jpeg"   -> loadJpegFile a
                   ".png"    -> loadPngFile a
                   ".gif"    -> loadGifFile a
                   otherwise -> undefined
               case image of 
                   undefined -> return ()
                   otherwise -> do
                      imageSetFromFile imagenW a
                      --  Configuracion de los botones   
                      configurarBoton botonSaveAs image imagenW mainWindow ext boton1 negative
                      {-configurarBoton botonSaveAs image imagenW mainWindow ext boton2 otra_funcion-}
                      {-configurarBoton botonSaveAs image imagenW mainWindow ext boton3 otra_funcion_mas-}
                      return ()

    on botonQuit menuItemActivate $ widgetDestroy mainWindow
    onDestroy mainWindow mainQuit
    widgetShowAll mainWindow
    mainGUI

configurarBoton bsa image imagenW mw ext boton filtro =
    onClicked boton $ do
    nuevaImagen <- filtro image imagenW ext
    on bsa menuItemActivate $ do openSaveAsFileDialog mw nuevaImagen 
    return ()

{-openOpenFileDialog ::  Window -> IO FilePath-}
openOpenFileDialog parentWindow = do
    dialog <- fileChooserDialogNew
                (Just $ "Elija una imagen para editar:")             
                (Just parentWindow)                     
                FileChooserActionOpen   
                [("Abrir"   , ResponseAccept)
                ,("Cancelar" , ResponseCancel)]  
    widgetShow dialog
    response <- dialogRun dialog
    widgetHide dialog
    case response of
       ResponseAccept -> do
          Just fileName <- fileChooserGetFilename dialog
          putStrLn $ "Se va a editar el archivo: " ++ show fileName
          return $ Just fileName
       ResponseCancel -> do
          putStrLn "Seleccion de archivo cancelada"
          return Nothing
       ResponseDeleteEvent -> do
          putStrLn "Cerrada ventana de seleccion de imagen"
          return Nothing

openSaveAsFileDialog parentWindow imagen = do
    dialog <- fileChooserDialogNew
                (Just $ "Guardar imagen como:")             
                (Just parentWindow)                     
                FileChooserActionSave   
                [("Guardar Imagen"   , ResponseAccept)
                ,("Cancelar" , ResponseCancel)]  
    widgetShow dialog
    response <- dialogRun dialog
    case response of
       ResponseAccept -> do nwf <- fileChooserGetFilename dialog
                            case nwf of 
                                Nothing -> putStrLn "Nothing"
                                Just path -> do
                                    putStrLn $ "El path de la imagen es :\n " ++ path
                                    (funcionGuardado path) path imagen
       ResponseUser 100  -> putStrLn "Presiono el boton de respaldo"
       ResponseDeleteEvent -> putStrLn "Cerrada la ventana de Guardar Imagen"
    widgetHide dialog

funcionGuardado ext = case (takeExtension ext) of
                        ".jpeg" -> saveJpegFile 95
                        ".png"  -> savePngFile
                        ".gif"  -> saveGifFile

--   Funciones de manipulacion de imagenes provistas por la libreria imagefilters
negative imagen imagenWidget ext = do
    putStrLn "Negative"
    Filter.negative imagen
    let guardar = funcionGuardado ext
    guardar ("/tmp/imagen_temporal" ++ ext) imagen
    imageSetFromFile imagenWidget ("/tmp/imagen_temporal" ++ ext)
    return imagen
            
{-convolverFilas imagen imagenWidget = do-}
    {-putStrLn "Convolver filas"-}
    {-writeImage "/tmp/imagen_temporal" (HIP.convolveRows [1, -1] imagen)-}
    {-imageSetFromFile imagen "/tmp/imagen_temporal"-}

{-convolverColumnas imagen imagenWidget= do-}
    {-putStrLn "Convolver columnas"-}
    {-writeImage "/tmp/imagen_temporal" (HIP.convolveCols [1, -1] imagen)-}
    {-imageSetFromFile imagen "/tmp/imagen_temporal"-}
