module Main where

-- my modules
import qualified Colored_2_3_5_Counter20 as Co
import DataTypePunkt 
-- system modules
import Data.List
import Data.Char
import Control.Monad
import System.Environment
import System.IO
--import Data.Time as T
--import System.Locale
import System.Random

---------------------------------------------------------------
-- GLOBAL VARIABLES
overWrite = "1" --arun
--------------------------------------------------------------
-- GLOBAL FUNCTIONS ; exported over the whole program
avanti e =  mapM_ putStrLn $ e
--rnd gnerator and time
zufallsBasic1 t a x = (take t  (randomRs (1,a) (mkStdGen x)))::[Int]
-- filter a time string plug into random number generator
timeRNDInts foTime soMany digits = let prepStringtoInt = (map chr (filter (<58)(filter (>47)(map ord (show foTime)))))
                               in let plugRandom wieviele = zufallsBasic1 wieviele (read prepStringtoInt) digits
                               in (show (plugRandom soMany)) 

-- write six files then start back overwrite 1 ....
-- will turn on number digit of a Sring +1
--  e.g "aname1.txt" -> "aname2.txt"
--  used in time depending random file writing in 'dit' 
--aString:Sting ; the sourceFile
evalToWrite astrinG = if tzBool>0 then prsRoot++(head tz3)++(show tzInt)++"."++(last tz3)
                      else prsRoot++(head tz3)++("1.")++(last tz3)
     where
    poc1 fOsnd = reverse( fOsnd(break (=='/') (reverse(astrinG))));--prevent '/' cause trouble
    prsInput = poc1 fst;
    prsRoot = poc1 snd;  
    tz0 = (map ord prsInput);
    tz = (filter (>47) (filter (<57)  tz0));
    tzExp = (map chr tz);
    tzBool = length tzExp;
    tzRootSource  = filter (==47); 
    tz1 = tz0 \\ tz;
    tz2 = map (\c -> if c=='.' then ' '; else c);
    tz3 = words (tz2 (map chr tz1));
    tzInt = if tzBool==0 then 1
            else if (read tzExp)<7 then (read tzExp)+1
            else 1 ;


main:: IO()
main = do 
  putStrLn "get the fucntions back in"


rootPunkt = Punkt "fstInstance" Nothing Nothing Nothing Nothing Nothing  
fomain = do
  avanti ["decide: 1 mother\n"++
          "        2 father\n"++
          "        3. mother2\n"++
          "        4. loopnumber\n"++
          "        5 minMaxTruaOrFalse\n"] 
  cion <- getLine                   
  putStrLn cion  
   
