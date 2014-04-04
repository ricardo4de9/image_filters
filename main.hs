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
    boton4 <- builderGetObject builder castToButton "button4"
    boton5 <- builderGetObject builder castToButton "button5"
    boton6 <- builderGetObject builder castToButton "button6"
    boton7 <- builderGetObject builder castToButton "button7"
    boton8 <- builderGetObject builder castToButton "button8"
    boton9 <- builderGetObject builder castToButton "button9"

    botonOpen   <- builderGetObject builder castToImageMenuItem "imagemenuitem2"
    botonSaveAs <- builderGetObject builder castToImageMenuItem "imagemenuitem4"
    botonQuit   <- builderGetObject builder castToImageMenuItem "imagemenuitem5"

    on botonOpen menuItemActivate $ do
        archivo <- openOpenFileDialog mainWindow
        if (archivo == "")
            then return ()
            else do putStrLn $ "El archivo de imagen es :" ++ archivo
                    let ext = takeExtension archivo
                    image <- case ext of 
                        ".jpeg"   -> loadJpegFile archivo
                        ".png"    -> loadPngFile archivo
                        ".gif"    -> loadGifFile archivo
                    putStrLn "Evaluando la imagen "
                    imageSetFromFile imagenW archivo
                    --  Configuracion de los botones
                    configurarBoton botonSaveAs image imagenW mainWindow ext boton1 negativeImg
                    configurarBoton botonSaveAs image imagenW mainWindow ext boton2 grayscaleImg
                    configurarBoton botonSaveAs image imagenW mainWindow ext boton3 gaussianBlurImg
                    configurarBoton botonSaveAs image imagenW mainWindow ext boton4 edgeDetectImg
                    configurarBoton botonSaveAs image imagenW mainWindow ext boton5 meanRemovalImg
                    configurarBoton botonSaveAs image imagenW mainWindow ext boton6 colorizeImg
                    configurarBoton botonSaveAs image imagenW mainWindow ext boton7 smoothingImg
                    configurarBoton botonSaveAs image imagenW mainWindow ext boton8 brightnessImg
                    configurarBoton botonSaveAs image imagenW mainWindow ext boton9 contrastImg
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
          return fileName
       ResponseCancel -> do
          putStrLn "Seleccion de archivo cancelada"
          return ""
       ResponseDeleteEvent -> do
          putStrLn "Cerrada ventana de seleccion de imagen"
          return ""

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
                        
negativeImg:: Graphics.GD.Image 
           -> Graphics.UI.Gtk.Image 
           -> String 
           -> IO Graphics.GD.Image
--   Funciones de manipulacion de imagenes provistas por la libreria imagefilters
negativeImg imagen imagenWidget ext = do
    putStrLn "Negativo"
    Filter.negative imagen
    let guardar = funcionGuardado ext
    guardar ("/tmp/imagen_temporal" ++ ext) imagen
    imageSetFromFile imagenWidget ("/tmp/imagen_temporal" ++ ext)
    return imagen
    
grayscaleImg :: Graphics.GD.Image
															-> Graphics.UI.Gtk.Image
															-> String
															-> IO Graphics.GD.Image
grayscaleImg imagen imagenWidget ext = do
    putStrLn "Escala de Grises"
    Filter.grayscale imagen
    let guardar = funcionGuardado ext
    guardar ("/tmp/imagen_temporal" ++ ext) imagen
    imageSetFromFile imagenWidget ("/tmp/imagen_temporal" ++ ext)
    return imagen
    
gaussianBlurImg :: Graphics.GD.Image
															-> Graphics.UI.Gtk.Image
															-> String
															-> IO Graphics.GD.Image
gaussianBlurImg imagen imagenWidget ext = do
    putStrLn "Gaussian Blur"
    Filter.gaussianBlur imagen
    let guardar = funcionGuardado ext
    guardar ("/tmp/imagen_temporal" ++ ext) imagen
    imageSetFromFile imagenWidget ("/tmp/imagen_temporal" ++ ext)
    return imagen

edgeDetectImg :: Graphics.GD.Image
															-> Graphics.UI.Gtk.Image
															-> String
															-> IO Graphics.GD.Image
edgeDetectImg imagen imagenWidget ext = do
    putStrLn "Detección de Bordes (Edge Detect)"
    Filter.edgeDetect imagen
    let guardar = funcionGuardado ext
    guardar ("/tmp/imagen_temporal" ++ ext) imagen
    imageSetFromFile imagenWidget ("/tmp/imagen_temporal" ++ ext)
    return imagen

meanRemovalImg :: Graphics.GD.Image
															-> Graphics.UI.Gtk.Image
															-> String
															-> IO Graphics.GD.Image
meanRemovalImg imagen imagenWidget ext = do
    putStrLn "Remoción de Media (Mean Removal)"
    Filter.meanRemoval imagen
    let guardar = funcionGuardado ext
    guardar ("/tmp/imagen_temporal" ++ ext) imagen
    imageSetFromFile imagenWidget ("/tmp/imagen_temporal" ++ ext)
    return imagen

{- colorizeImg utiliza una tupla (Int,Int,Int,Int) que corresponde
al desplazamiento RGBA que tendra la imagen 
    RGB valores entre -255 y +255,
    A valores entre -127 to +127. -}
colorizeImg :: Graphics.GD.Image
															-> Graphics.UI.Gtk.Image
															-> String
															-> IO Graphics.GD.Image
colorizeImg imagen imagenWidget ext = do
    putStrLn "Colorizar"
    Filter.colorize imagen (0,255,87,1)
    let guardar = funcionGuardado ext
    guardar ("/tmp/imagen_temporal" ++ ext) imagen
    imageSetFromFile imagenWidget ("/tmp/imagen_temporal" ++ ext)
    return imagen
    
smoothingImg :: Graphics.GD.Image
															-> Graphics.UI.Gtk.Image
															-> String
															-> IO Graphics.GD.Image
smoothingImg imagen imagenWidget ext = do
    putStrLn "Suavizar"
    Filter.smoothing imagen 10.0
    let guardar = funcionGuardado ext
    guardar ("/tmp/imagen_temporal" ++ ext) imagen
    imageSetFromFile imagenWidget ("/tmp/imagen_temporal" ++ ext)
    return imagen
    
brightnessImg :: Graphics.GD.Image
															-> Graphics.UI.Gtk.Image
															-> String
															-> IO Graphics.GD.Image
brightnessImg imagen imagenWidget ext = do
    putStrLn "Aumentar Brillo"
    Filter.brightness imagen 10
    let guardar = funcionGuardado ext
    guardar ("/tmp/imagen_temporal" ++ ext) imagen
    imageSetFromFile imagenWidget ("/tmp/imagen_temporal" ++ ext)
    return imagen
    
contrastImg :: Graphics.GD.Image
															-> Graphics.UI.Gtk.Image
															-> String
															-> IO Graphics.GD.Image
contrastImg imagen imagenWidget ext = do
    putStrLn "Aumentar Contraste"
    Filter.contrast imagen 10
    let guardar = funcionGuardado ext
    guardar ("/tmp/imagen_temporal" ++ ext) imagen
    imageSetFromFile imagenWidget ("/tmp/imagen_temporal" ++ ext)
    return imagen
