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
import System.Locale
import System.Random
--import qualified Data.Time as T
import qualified UsefulFunctions19 as U
import TheAtoBplotter
--import GHCguiNfunctions as H
--import Text.IPA
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
  putStrLn "Setup"
  fomain

rootPunkt = Punkt "fstInstance" Nothing Nothing Nothing Nothing Nothing  
fomain = do
  avanti ["START: 1 one analysis with plot via 'kArmTest5'\n"++
          "        2 a list of above\n"++
          "        3. write Html\n"++
          "        4. setup decide which functions to apply to input\n"++
          "        5 close\n"]
  let rnd ="1" 
  thats <- readFile ("src\\lala.wxm")   
  let rnd = head(words (U.replaceE (thats)))
  putStrLn ("the Rnd" ++ rnd) 
  stelsel <- getLine
  putStrLn "show output? 1==show it"
  hideOu <- getLine 

    -- TheATOBplotter>  runKBASE 1 [1,1] 1 1 li2 1 2 3 4 
  let selectOR = do 
         let contrl = "1" -- set 
         if stelsel=="1" then do
                 if hideOu == "1" && contrl == "1" then do  fomain2 "2" "1" rnd selectOR "1" -- keyboard, direktauto make patternfile , show output
                 else if hideOu /= "1" && contrl == "1" then do  fomain2 "2" "1" rnd selectOR "2" -- no keyboar, hide outputd
                 else if hideOu /= "1" && contrl /= "1" then do  fomain2 "2" "1" rnd selectOR "1" -- keyboard , hide output 
                 else fomain2 "2" "1" rnd selectOR "2" -- show output , globalvars
        -- else if stelsel=="1" && contrl /= "1" then do  fomain "2" "1" rnd selectOR "2" --globalvars, menu ,  
         else if stelsel=="2" && hideOu == "1" && contrl == "1" then do fomain2 "2" "2" rnd selectOR "1" --  "     , no-output , use keyboard
         else if stelsel=="2" && hideOu == "1" && contrl /= "1" then do fomain2 "2" "2" rnd selectOR  "2" --  "     , no-output ,use GLobal Vars
         else if stelsel=="2" && hideOu /= "1" && contrl == "1" then do fomain2 "2" "1" rnd selectOR  "1" -- yes output , keyboard
         else if stelsel=="2" && hideOu /= "1" && contrl /= "1" then do fomain2 "2" "1" rnd selectOR  "2" -- yes output , global var  
         else do
           construction --easyAccess
           fomain
  selectOR 

construction = do putStrLn "construction"
fomainAction = ["RUN: 1 one analysis with plot via 'kArmTest5'\n"++
                "        2 a list of above\n"++
                "        3. write Html\n"++
                "        4. setup decide which functions to apply to input\n"++
                "        5 close\n"] 
--lala
fomain2 autoInputI autoInputII foRnd goBack autoInputIII = do
  --dit  -- write random.txt for main
  foutN1 <- getLine 
  foautInp2 autoInputI "1" "senior.txt" "enter File to construct" --file22writ "Enter file to write e.g senio.txt"
  foOutputRAW autoInputI (avanti [""]) (avanti (fomainAction)) -- show GUI or not <= autoInputII
  putStrLn "fomain2:level"
  input <- getLine     
  let chart0 = if input == "1"
               then do
                               --   myReadLine thecsv 
                   let inserTs = "Enterli and pi in format" 
                    -- ["A",.." --(P.writePatternFile "senio1.txt")
                   putStrLn inserTs
                   li <- getLine
                   let pi = Punkt "M" Nothing Nothing Nothing Nothing Nothing
                   Co.kArmTest5 [(unwords (lines li))] 1 pi 1 1 [] "AAA"
                   goBack 
               else if input=="2"
               then do
                   putStrLn "construction 1!!!" --P.aMemoryRAW autoInputII file22writ file33writ
                   goBack 

              -- OSZILOSKOP1
              ---------------------------------------------------------
              -- KEYBOARD ON              show output &&  keyboard- on ; 
               else if input=="3" && autoInputII=="1" && autoInputIII=="1"
               then do
                -- set yes-output, read theWritetxt -> writes 
                   putStrLn "construction!!!" --P.aOsZilloskop1KEY "1" 
                   goBack 
             ---------------------------------------------------------
              -- KEYBOARD ON / SHOW NO OUTPUT    no show output && keyboard- on ; 
               else if input=="3" && autoInputII=="2" && autoInputIII=="1"
               then do
                -- set yes-output, read theWritetxt -> writes 
                   putStrLn "construction" --P.aOsZilloskop1KEY "2" 
                   goBack 
            ----------------------------------------------------------
            -- GLOBAL VARIABLES ON        show-output && global variables , yourFunction-off
               else if input=="3" && autoInputII=="1"  && autoInputIII=="2"
               then do
                -- control via Global variablea , set no-output, read theWritetxt -> writes 
                   putStrLn "construction" --P.aOsZilloskop1RAW "1" file22writ file33writ crit foRnd howMany fohans "1" 1 1
                   goBack 
              ----------------------------------------------------------
            -- GLOBAL VARIABLES ON / SHOW NO OUTPUT  no-output && global variables ,your functions off
               else if input=="3" && autoInputII=="2"  && autoInputIII=="2"
               then do
                -- control via Global variablea , set no-output, read theWritetxt -> writes 
                   putStrLn "construct3" --P.aOsZilloskop1RAW "2" file22writ file33writ crit foRnd howMany fohans "1" 1 1
                   goBack 
              ----------------------------------------------------------

              --CRUNCHLIST 
               else if input=="4" && autoInputII=="1"  && autoInputIII=="2" --show-output ; global var
               then do
                 --   read write howmany crit
                   construction --P.aCrunchList1RAW file22writ file33writ howMany crit
                   goBack 
               else if input=="4" && autoInputII=="2"  && autoInputIII=="2"  -- hide-output ; global var
               then do
                 --   read write howmany crit
                   construction --P.aCrunchList1RAW  file22writ file33writ howMany crit
                   goBack 
               else if input=="4" && autoInputII=="1"  && autoInputIII=="1"  -- show-output ; keyboard-io
               then do
                   construction --P.aCrunchList1KEY "1"
                   goBack
               else if input=="4" && autoInputII=="2"  && autoInputIII=="1"  -- no-output ; keyboard-io
               then do
                   construction --P.aCrunchList1KEY "2"
                   goBack
 
               else if input=="5"
               then do
                   putStrLn "End Compute"   
               else do 
                   goBack  
           -- uses input0 to stop computations  
  chart0 

foOutputBat output paks = foOutputRAW output paks (putStrLn (""))

foOutputRAW output paks pac = if output=="1" 
                       then do
                          paks
                       else do
                          pac

----------------------------------------------------
foautInp2 autoInput solong globalVar aTxT = do
           let customL = [1..(read solong)]
           doesTaht <- forM (customL) (\presetList -> do 
                   if autoInput=="1"
                   then do
                      morM <- forM [1] (\gu -> do
                            putStrLn aTxT -- e.g "Enter csv file to read"

                            exportThis <- getLine
                           -- let goAhead inserT1 = (foFunction inserT1) 
                           -- let inCompute = goAhead exportThis  
                           -- inCompute
                            return (exportThis))
                      putStrLn (unlines morM)
                   else do
                      putStrLn globalVar
                   return ())
           return()



-- 12-9-2020 Experiment3 
ert r = U.tk r (Co.ptc6 250)  
xS r = head (ert r)
yS r = last (drop 1 (take 2 (ert r)))
zS r = last (ert r)
maXxS = map xS [1..100]
maXyS r = map yS [1..100]
maXzS r = map zS [1..100]
whereTo e r = e `elemIndices` r
	   
-- engage Punkt data type 
-- with the intention to plug in any function f(x)
-- and select any x with m
basis22 foAL m = U.maybePu (head (Co.ausw m foAL ))
-- *e.g>basis22 (map show (ptc6 105)) 5
 -- => ...   --shows line 5 of ptc6
	   
-- store data in String because there are only 5 other spots left
-- when reading a longer list that wont help thus store in Punkt "String"
basisPunkt foAL r = U.maybePu (show(checkflow [] [((basis22 foAL r))]))

-- plug 'basisPunkt' into this test below best to be mapped via e
fmrTEST3 io e e2 forLine =  checkflow  io [(Punkt  (head(checkflow [] [(basis4 e2 forLine)]))(Just (basis2 e 1 )) (Just (basis2 e 3 )) (Just (basis2 e 4 ))(Just (basis2 e 5 )) (Just (basis2 e 6))) ]

--old:
-- make a function that is a [(Maybe Punkt)]-> that by itself is the definiton of
-- mother :: Punkt -> Maybe Punkt 
-- this function below shall lead to => a motherTYPE that is depending on the type of simiyritYvalue
-- new:
-- -- a group of functions that shall a [(Maybe Punkt)]-> that by itself is the definiton of
-- mother :: Punkt -> Maybe Punkt 
-- these functions below shall lead to => a motherTYPE that is depending on the type of simiyritYvalue
	   -- to plug any value into 'Maybe Punkt' we need a 'Maybe String'
--	   *>:t foAdecide2 (map show (Co.ptc6 100))
--	   *>foAdecide2 (map fshow (Co.ptc6 100)) :: Maybe String
foAdecide2 foA = let boa rt t = (Just (U.maybePu2 rt t)) --let whereBreak = chainDistribute crit bonelist crit (lines "1")
                 in let mapMaybePun k = let ste1 k rt = (boa (head(Co.ausw k rt))) ((Just (U.maybePu (head (Co.ausw k rt)))) ) 
                                        in ste1 k foA -- e.g foA = ["vb","vb2","vb3"]
                 in let preMoa = length foA
                 in let eindelijk = do (map mapMaybePun [1..preMoa]) 
                 in 
                 if foA==[] then Nothing
                 else let chssd i = U.maybePu2 (head(Co.ausw i foA))  (((boa (head(Co.ausw i foA)) (head(Co.ausw i eindelijk)))))  
                      in Just (show[(chssd 1)])

-- the 'Maybe String' is turned to a 'Maybe Punkt' that has an error handler
-- mayerPunkt :: IO(int) String -> Maybe String -> Maybe Punkt -> 
mayerPunkt r foa = if (foa) == [] then U.maybePu "empty"
                   else U.maybePu2 (r) (Just(U.maybePu ((show [(foAdecide2 (foa))]))))


iDF z = U.tk z ["AaEe0","AaEeI","i0000","OoUu0","OoU0Y","y0000"]
lP z = U.tk z ["0*x + y + 0*z = 3","0*x + y + 0*z = 33*x + 0 + 0*z = 6","3*x + 0 + 0*z = 6","0*x + 0*y + z = 2","0*x + 0*y + z = 2x + y + z = 11","x + y + z = 11"]
choosDomain ff z = do 
             if ff == 1 then iDF z
             else lP z



