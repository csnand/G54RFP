module Main where
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import qualified Parser                       as P
import Data.IORef


main :: IO ()
main = do
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "static"
        } setup

setup :: Window -> UI ()
setup window = do
    -- Sets the window title
    return window # set UI.title "Calculator -- Threepenny"
    UI.addStyleSheet window "bootstrap.min.css"
    UI.addStyleSheet window "creative.css"

    but123 <- mapM mkNumButton (convert [1..3])
    but456 <- mapM mkNumButton (convert [4..6])
    but789 <- mapM mkNumButton (convert [7..9])
    reset <- mkNumButton "C"
    clearEntry <- mkNumButton "CE"
    parens <- mapM mkNumButton ["(", ")"]
    butZeroDot <- mapM mkNumButton ["0", "."]
    butCSign <- mkNumButton "+/-"
    butOps <- mapM mkNumButton ["/", "*", "-", "+"]
    butEqual <- mkNumButton "="

    let event123 = zipWith digitAction but123 (convert [1..3])
    let event456 = zipWith digitAction but456 (convert [4..6])
    let event789 = zipWith digitAction but789 (convert [7..9])
    let eventParens = zipWith digitAction parens ["(", ")"]
    let eventZeroDot = zipWith digitAction butZeroDot ["0", "."]
    let eventOps = zipWith digitAction butOps ["/", "*", "-", "+"]
    let eventClear = clearLastEntry clearEntry
    let eventReset = resetAction reset
    let eventEqual = equalAction butEqual
    let eventCSign = changeSign butCSign

    let allEvents = event123 ++ event456 ++ event789 ++
                     eventParens ++ eventZeroDot ++ eventOps ++
                    [eventReset] ++ [eventClear] ++
                    [eventEqual] ++ [eventCSign]

    calc <- accumB "" $ foldl1 (unionWith const) allEvents

    row1 <- UI.div #. "row" #+ ([element reset] ++
                                [element clearEntry] ++
                                map element parens)
    row2 <- UI.div #. "row" #+ (map element but789)
    row3 <- UI.div #. "row" #+ (map element but456)
    row4 <- UI.div #. "row" #+ (map element but123)
    row5 <- UI.div #. "row" #+ (element butCSign : map element butZeroDot)
    opRow <- UI.div #. "col-md-3 operationSide" #+ (map element butOps ++ [element butEqual])

    numberPad <- UI.div #. "row numberPad" #+
                 ([UI.div #. "col-md-9" #+
                   map element [row1, row2, row3, row4, row5],
                   element opRow])


    displayBox <- UI.div #. "row displayBox" #+(
      [UI.label #. "prevAns" # set UI.id_ "prevAns"] ++ [UI.br] ++
      [UI.label #. "displayText" # set UI.id_ "display" #
       sink UI.text calc])


    calcDiv <- UI.div #. "col-md-4 col-md-offset-4 calculator" #
               set UI.align "center" #+
               (map element [displayBox, numberPad])

    getBody window #+ [UI.div #. "container" #+ [element calcDiv] ]
    return ()

digitAction :: Element -> String -> Event (String -> String)
digitAction button str = (\s -> addChar s str) <$ UI.click button

addChar :: String -> String -> String
addChar s1 s2 = s1 ++ s2

changeSign ::Element -> Event (String -> String)
changeSign button = (\s -> show (csign s)) <$ UI.click button
  where
    csign str = (-1) * read (P.eval str):: Double

clearLastEntry :: Element -> Event (String -> String)
clearLastEntry button = init <$ UI.click button

resetAction :: Element -> Event (String -> String)
resetAction button = (\_ -> "") <$ UI.click button

equalAction :: Element -> Event (String -> String)
equalAction button = P.eval <$ UI.click button

mkNumButton :: String -> UI Element
mkNumButton str = UI.button #. "btn btn-calc hvr-radial-out" #
  set UI.id_ str # set UI.text str

convert :: [Int] -> [String]
convert = (>>= return.show)

