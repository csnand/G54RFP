module Main where
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import qualified Parser                       as P
import Data.IORef

-- Simple type to allow the calculator to store the state
-- (Current calculation, previous answer)
newtype Calculator = C (Calc, Double)

-- Recursive Data type to store the calculation as a dynamic tree
data Calc = Num Double | Sign Calc |
            Add Calc Calc | Min Calc Calc |
            Div Calc Calc | Mul Calc Calc
-- Ability to show the calculation as a string in the UI
instance Show Calc where
    -- This will stop showing whole numbers as x.0
    show (Num i)   = if floor i == ceiling i then show (floor i) else show i
    -- Show using parentheses what will have its sign changed
    show (Sign c)  = "±(" ++ show c ++ ")"
    show (Add c d) = show c ++ " + " ++ show d
    show (Min c d) = show c ++ " - " ++ show d
    show (Div c d) = show c ++ " ÷ " ++ show d
    show (Mul c d) = show c ++ " x " ++ show d


-- Create a event for a given digit on the and button
createActionInt :: Element -> Double -> Event (Calculator -> Calculator)
createActionInt button i = (\x -> addDigitCalculator x i) <$ UI.click button

-- Helper function to add a Digit to the calculator
addDigitCalculator :: Calculator -> Double -> Calculator
addDigitCalculator (C (c, d)) i = (C ((addDigitCalc c i), d))

-- Adds the digit given to the current calculation
addDigitCalc :: Calc -> Double -> Calc
addDigitCalc (Num x)   i = Num (x*10 + i)
addDigitCalc (Sign c)  i = Sign (addDigitCalc c i)
addDigitCalc (Add d c) i = Add d (addDigitCalc c i)
addDigitCalc (Min d c) i = Min d (addDigitCalc c i)
addDigitCalc (Div d c) i = Div d (addDigitCalc c i)
addDigitCalc (Mul d c) i = Mul d (addDigitCalc c i)

-- Change the sign of the entire current calculation
changeSign :: Calculator -> Calculator
changeSign (C ((Sign c), d)) = (C (c, d))
changeSign (C (c, d)) = (C ((Sign c), d))

-- Creates a UI Button with the given the String
createButton :: String -> UI Element
createButton s = UI.button # set UI.text s

-- Create an action based upon a given calculation opperation
createAction :: Element -> Calc -> Event (Calculator -> Calculator)
createAction button (Add _ _) = ( \(C (c, d)) -> (C ((Add c (Num 0)), d)) ) <$ UI.click button
createAction button (Min _ _) = ( \(C (c, d)) -> (C ((Min c (Num 0)), d)) ) <$ UI.click button
createAction button (Mul _ _) = ( \(C (c, d)) -> (C ((Mul c (Num 0)), d)) ) <$ UI.click button
createAction button (Div _ _) = ( \(C (c, d)) -> (C ((Div c (Num 0)), d)) ) <$ UI.click button

-- Evaluate a given calculator
-- Operations are evaluated left to right NOT in order of precedence
evaluateCalc :: Calculator -> Calculator
evaluateCalc (C (c, _)) = (C ((Num 0), evaluate c))

-- Evaluate a given calculation
evaluate :: Calc -> Double
evaluate (Num i)   = i
evaluate (Add c d) = (evaluate c) + (evaluate d)
evaluate (Mul c d) = (evaluate c) * (evaluate d)
evaluate (Div c d) = (evaluate c) / (evaluate d)
evaluate (Min c d) = (evaluate c) - (evaluate d)
evaluate (Sign c)  = (-1) * (evaluate c)

-- Resets the previous answer on the calculator
clearEntry :: Calculator -> Calculator
clearEntry (C (c, _)) = (C (c, 0))

-- Resets the calculator
clearCalculator :: Calculator
clearCalculator = C ((Num 0), 0)

-- Replaces the last number in the calculation with a given value
addAnswerCalculator, addPiCalculator, addECalculator, addRt2Calculator :: Calculator -> Calculator
addAnswerCalculator (C (c, d)) = (C ((changeLastNumCalc c d),        d))
addPiCalculator     (C (c, d)) = (C ((changeLastNumCalc c pi),       d))
addECalculator      (C (c, d)) = (C ((changeLastNumCalc c (exp 1)),  d))
addRt2Calculator    (C (c, d)) = (C ((changeLastNumCalc c (sqrt 2)), d))

-- Takes in the current calculation and will change the last number to what is given
-- This is used to replace the last number with the ans or a constant.
changeLastNumCalc :: Calc -> Double -> Calc
changeLastNumCalc (Num x)   a = Num a
changeLastNumCalc (Sign c)  a = Sign  (changeLastNumCalc c a)
changeLastNumCalc (Add d c) a = Add d (changeLastNumCalc c a)
changeLastNumCalc (Min d c) a = Min d (changeLastNumCalc c a)
changeLastNumCalc (Div d c) a = Div d (changeLastNumCalc c a)
changeLastNumCalc (Mul d c) a = Mul d (changeLastNumCalc c a)

-- Shows the Answer part of the string
-- This will stop showing whole numbers as x.0
showAns :: Calculator -> String
showAns (C (_, d)) = "Ans: " ++ (if floor d == ceiling d then show (floor d) else show d)

-- Shows the calculation part of the calculator as a string
showCalc :: Calculator -> String
showCalc (C (c, _)) = show c

-- Main function simply starts the webserver and runs the setup
main :: IO ()
main = do
    startGUI defaultConfig
        { jsPort       = Just 8024
        , jsStatic     = Just "./static"
        } setup

setup :: Window -> UI ()
setup window = do
    -- Sets the window title
    return window # set UI.title "Calculator -- Threepenny"

    UI.addStyleSheet window "bootstrap.min.css"
    UI.addStyleSheet window "creative.min.css"
    UI.addStyleSheet window "creative.css"

    getBody window #+ [UI.div #. "container" #+ [calcDiv] ]
    return ()

calcDiv :: UI Element
calcDiv = UI.div #. "col-md-4 col-md-offset-4 calculator" # set UI.align "center" #+
  ([displayBox] ++ [numberPad])

displayBox :: UI Element
displayBox = UI.div #. "row displayBox" #+
  [UI.p #. "displayText" # set UI.id_ "display" # set UI.text "0"]

numberPad :: UI Element
numberPad = UI.div #. "row numberPad" #+
 ([UI.div #. "col-md-9" #+ ([row1] ++ [row2] ++ [row3] ++ [row4] ++ [row5])] ++
  [opRow])

row1 :: UI Element
row1 = UI.div #. "row" #+
  ([mkNumButton "clear" "C"] ++
   [mkNumButton "ce" "CE"] ++
   [mkNumButton "Lpar" "("] ++
   [mkNumButton "Rpar" ")"])


row2 :: UI Element
row2 = UI.div #. "row" #+
  (map mkNumButton' (convert [7..9]))

row3 :: UI Element
row3 = UI.div #. "row" #+
  (map mkNumButton' (convert [4..6]))


row4 :: UI Element
row4 = UI.div #. "row" #+
  (map mkNumButton' (convert [1..3]))



row5 :: UI Element
row5 = UI.div #. "row" #+
  ([mkNumButton "plus_minus" "+/-"] ++
   [mkNumButton "zero" "0"] ++
   [mkNumButton "decimal" "."])

opRow :: UI Element
opRow = UI.div #. "col-md-3 operationSide" #+ (
  [mkNumButton "divide" "/"] ++
  [mkNumButton "multiply" "*"] ++
  [mkNumButton "subtract" "-"] ++
  [mkNumButton "add" "+"] ++
  [mkNumButton "equals" "="])


mkNumButton :: String -> String -> UI Element
mkNumButton id str = UI.button #. "btn btn-calc hvr-radial-out" #
  set UI.id_ id # set UI.text str

mkNumButton' :: String -> UI Element
mkNumButton' str = UI.button #. "btn btn-calc hvr-radial-out" #
  set UI.id_ str # set UI.text str

convert :: [Int] -> [String]
convert = (>>= return.show)


    -- -- Create all the buttons from strings
    -- buttonsNum  <- mapM createButton (map show [0..9])
    -- buttonsOps  <- mapM createButton ["+", "-", "x", "÷"]
    -- buttonClr   <- createButton "C"
    -- buttonCrEnt <- createButton "CE"
    -- buttonSign  <- createButton "±"
    -- buttonAns   <- createButton "Ans"
    -- buttonEq    <- createButton "="

    -- -- Create all the events by adding functions to the associated buttons
    -- let eventsNum  = zipWith createActionInt buttonsNum [0..9]
    -- let eventsOps  = zipWith createAction buttonsOps [(Add (Num 1) (Num 1)), (Min (Num 1) (Num 1)), (Mul (Num 1) (Num 1)), (Div (Num 1) (Num 1))]
    -- let eventClear = (const clearCalculator) <$ UI.click buttonClr
    -- let eventCrEnt = (\x -> clearEntry x) <$ UI.click buttonCrEnt
    -- let eventSign  = (\x -> changeSign x) <$ UI.click buttonSign
    -- let eventAns   = (\x -> addAnswerCalculator x) <$ UI.click buttonAns
    -- let eventEqual = (\x -> evaluateCalc x) <$ UI.click buttonEq

    -- -- Make all of the events into a single list
    -- let events = eventsNum ++ eventsOps ++ [eventClear, eventSign, eventEqual, eventCrEnt, eventAns]

    -- -- Add the event handler to access the calculator with a starting value of reset
    -- calculator  <- accumB clearCalculator $ foldl1 (unionWith const) events

    -- -- Make labels with the current calculation and previous answer
    -- ansDisplay  <- UI.label # sink UI.text (fmap showAns calculator)
    -- calcDisplay <- UI.label # sink UI.text (fmap showCalc calculator)

    -- -- Add all of the buttons to window
    -- getBody window #+ [UI.center #+
    --                    ([element ansDisplay, UI.br, element calcDisplay, UI.br]++
    --                     [ element b | b <- buttonsNum ] ++ [UI.br] ++
    --                      [ element b | b <- buttonsOps ] ++ [UI.br] ++
    --                      [element buttonCrEnt, element buttonClr, element buttonSign, element buttonAns, UI.br] ++
    --                      [element buttonEq]
    --                                  )]


