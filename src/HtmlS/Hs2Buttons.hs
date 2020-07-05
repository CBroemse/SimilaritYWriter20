module Hs2Buttons 
    () where
import Control.Monad
import Control.Concurrent (threadDelay)

import Paths

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Buttons
------------------------------------------------------------------------------}

runM :: IO ()
runM = do
    static <- getStaticDir
    startGUI defaultConfig { jsStatic = Just static } setup

setup :: Window -> UI ()
setup w = void $ do
    return w # set title "Buttons"
    UI.addStyleSheet w "buttons.css"

    buttons <- mkButtons
    getBody w #+
        [UI.div #. "wrap" #+ (greet ++ map element buttons ++ [viewSource])]

--greet :: [UI Element]
greet =
    [ UI.h1  #+ [string "\"AAABB\"  \"AAABAABAB\" \"AABAB\"  \"AAA\"  \"AAABBBAA\"   \"BBBAA\""]
    , UI.div #+ [string "Try the buttons below, they hover and click."]
    ]


mkButton :: String -> UI (Element, Element)
mkButton title = do
    button <- UI.button #. "button" #+ [string title]
    view   <- UI.p #+ [element button]
    return (button, view)

mkButtons :: UI [Element]
mkButtons = do
    list    <- UI.ul #. "buttons-list"
    
    (button1, view1) <- mkButton button1Title
    
    on UI.hover button1 $ \_ -> do
        element button1 # set text (button1Title ++ " [hover]")
    on UI.leave button1 $ \_ -> do
        element button1 # set text button1Title
    on UI.click button1 $ \_ -> do
        element button1 # set text (button1Title ++ " [pressed]")
        liftIO $ threadDelay $ 1000 * 1000 * 1
        element list    #+ [UI.li # set html "<b>Delayed</b> result!"]
    
    (button2, view2) <- mkButton button2Title

    on UI.hover button2 $ \_ -> do
        element button2 # set text (button2Title ++ " [hover]")
    on UI.leave button2 $ \_ -> do
        element button2 # set text button2Title
    on UI.click button2 $ \_ -> do
        element button2 # set text (button2Title ++ " [pressed]")
        element list    #+ [UI.li # set html ("<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/ilabel.png\" style=\"width:5%\">")]
    
    return [list, view1, view2]

  where button1Title = "Compute Complex number for this"
        button2Title = ("Click me, I work immediately\n")

viewSource :: UI Element
viewSource = UI.p #+
    [UI.anchor #. "view-source" # set UI.href url #+ [string "View source code"]]
    where
    url = samplesURL ++ "storage1.txt"

inserT = ((mysamples "storage1.txt"))

