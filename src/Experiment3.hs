-- this module provides:
-- --------------------------------------                                                    concept                                               beard  vs   not beared
-- EXPERIMENT 3                                                                            -----------                                                  :     :                                  
-- the output of functions                                                                 --                beard        not beared                    :     :
-- themselves has a           syntax            AND/OR           a concept                 --                collum I       collumII                 wo :_____:ist   
-- functions used with type pure outpout e.g                                               --            --------------------------                    /      \    
--                            kArmWORK                            kArmTest5                --    row I A |      B      |    not B  |    C .. --\  wo' /        \ 
--                 is a work version for other      is to illustrate this function in ghc  --  astigmatic| '  cell I   |   cell II |            \  |  \        / ist' 
--                 computations                     on screen                              -- -- ----------------------------------------       / \|/  \______/    
--                                                                                         --   row II A'|     B       |    not B  |         --/  C'of wo     C' of ist
--                                                                                         -- not astigm |   cell III  |   cell IV |                     \  /
--                                                                                         -- - matic    --------------------------                        C
--                                                      ########################################################  initiate  data stream li -> Baysian -> CELLS -> BIAS 
--                                                                         
--external cell run   ################################################## 11-1-21 cell reason
---------------------
--pi: Punkt "extern" ...; guess String e.g "ddd"
--    ||                ||
--    \/                \/
--  kArmWORK       triggerWord   -> runEXP3  -> cellStream3EXT   -> expressAinA
--                                                               -> expressAsinA
--                                           -> simVALS
--                                           -> runEXP3Char
--                                           -> experiment3RAW11
--    kArmTest5    .??                        -> experiment3RAW22  -> buildPrior                      no use yet
--                                                                -> theB' -> -- li :: [[String]]
--                                                                               li3 :: [String]     no use yet
--                                                                -> theBQuirky                      no use yet
--                                           -> experiment3RAW23 -> commmB  -> foStrCd ->  qw wo ist r = (foStrCd wo ist r)

--                                                               -> commmB2
--
-- 'zufallsBasic1'-> 'longRun' => random number
--                                   ||
--                                   \/
--                               |  rekenen  OR rekenen2 |
--                                   ||              
--                                   \/
--   takeMY =>      countABCs -> runCellRAW -> runCellRnd -> ogR -> cellStream3   -> poolBorNotB 
--                                                                                -> inActie 
--                                                                 ################################################### 11-1-21 reason cell run export via Punt do cellStream3EXT
--
--variables: 
-------------                                                                               
-- li      ::[String]                                any input as [String] should have minimum list length of 4
--              :: [CELLI,CELLII,CELLIII,CELLIV]     BAYSIAN TYPE                                                                ########################################## initiate  data stream li 
--
--'liSolution'::= a real Prior/Bias?!?              this String contains the 'solution' of li and liSolution:"xyz=11"                ---------------------##################### RELATED BIAS with liSolution
--                          or " x + y + z = 11 "    a stream of data is introduced to another similar one with certain
--                                                   properties so far in MOST functions liSolution is to compare via simival       
-- e.g *>let liSol = ["0*x + y + 0*z = 3","0*x + y + 0*z = 33*x + 0 + 0*z = 6","3*x + 0 + 0*z = 6","0*x + 0*y + z = 2","0*x + 0*y + z = 2x + y + z = 11","x + y + z = 11"]
--                                                  all functions that use 'runEXP3' manouver between CELLS ?
--                                                  can be switched to 2 different states COMPARE with "intern" vs "cell". a ghAdd::String is added to  
--compare:  
--   runEXP3 li4 "xyz=11" pi                       set a [String] with Simval li guess to 0
--
--glossary: 
--                            syntax                             concept
--   kArmTest5                                      sample IO, a do monad to sort a given li list
--                                                  ment to be a display in 'where' passage of function, an example how to access
--                                                  all functions displayed there, that are accesable via allFunctions'. 
--
--   kArmWORK   kArmWORK 2 ["A","","","","",""] li4 2 pi 2 2 [] "xyz=11"  
--                                                  same as above but only one output(no GUI) to be used in other computations 
--                                                  simiVal compare every String of li [String] with addGH
--                                                            [String] -> String => [(Fractional)] 
--                                                  simiVal 4x4 matrix, set to zero with addGH:: String e.g li4 && "xyz=11"  
--                                                  only depends on li4 and addGH  the list ["A","","","","",""] is kept
--                                                  so it can be compared to possible outcomes  
--
--   experiment3RAW11                               Busschop and Bradly ??? also see 'Colored_2_3_5_Counter20.hs, build a hexagon
--                                                  turn 3d ptc functions in 2d/3d svg  
--
--   baysianType                                    determine from String to String 
--
--   poolB           e.g*>map chr $ poolB li4 5     just parse the input li -> always show one 'Char' as  'x' :[] -> [Char]
                                       --             [String] -> Int -> p (B|A) -> if lenght li > Int => always B or CELLI
--                   with conditionI always B          li      -> solution 
--                                                   => Baysian Type , CELLI  ... needs type check qw??? 
-- FASTER ---------------------------------------------------------------------------------------------------------------######################################### BAYSIANS/ SOLUTIONS
--   poolBandNotB                                   based on liT, give a list of strings  
--                                                  import li -> randomize input -> give noisy output,   Baysian approach*1
--                                                  enter in defSearch how many Int to take 
--
--   poolBandNotBdif                                 takes an [Int] that has now further side effects yet, could be used
--                                                  with simiVal (exter?? intern? cell?) to find a meassure of order for the given sequence of Chars

--   cellStream1 (cellStream1 33 li4 2 )               [String] -> Int    => BaysianType ->  CELL IV rename to cellstream4???
--                                                      li     -> charNo => show order in Type -> [Int] <=> String -> human interpretations  
--                                                    even if result does not carry the right letters it might
--                                                    carry the right minimal order, it can compare the length of the result to one string of li
--                                                    and possibly compare the length of a result with a guess 
--                                                    and see if the invoced cellStream has similar groups in occuring letters. 
--
--   cellStream2  (cellStream2 3 li4 1 )=romi          [String] -> Int => [Int] <=> [Int] -> computational interpretatiosn 
--                                                    show zero space -> find positions zeros -> 
--                  *> [0] `elemIndices` romi         => [Int] order of occourance of zeros
--                  [106,146,186,432,472]                                                
--                                                     
--   cellStream3  map chr (cellStream3 li4 22222222)   [String] -> Int => [Int] <=> String -> human interpretatiosn 
--   astigmatic::= arithmetic reihe order n
--                 with A beard= order 2 ?!?
--
--   runEXP3Char 
--                                                  only depends on li
--   inActie                                        1. output fix number of suggested astigmatic A:   [String] -> [String]
--
--                                                            the difference between an P(A|B) and P(A|notB) is
--                                                            the order in which letters occure
--                                                            order 
--                                                  2.        a letter recommondation plus possible solutions to 
--                                                            that letter 
--                                                  3.        fix number of type BaysianType one of many
--                                                            possible lines concsists of: [[Int]]

--
-- SLOW: ------------------------------------------------------------------------------------------------------------------------- ##################### like above but slow 
--   poolBorNotB     *> poolBandNotB li4 3          how many possibities are there?   128-48 * (length li)*(Int of population)  
--   poolBorNotBdif  *> poolBandNotBdif li4 3 [4,3,2,6,7,1]   80*(length$concat li)*a
--                                                  [String] -> a Int -> n Int -> if length i > a == True then  always B and/or not B
--                                                  => BaysianTyper cell II 
--                                                  

--   buildPrior        given a  let gh r = chainDistribute r ["2","2","1"] r (lines "1")
--                                                   
--                                                  not used yet : expressAsinA, expressAinA   
--                                                    
--
-- concept                                              beard  vs   not beared
-- -------                                                   :     :
--                beard        not beared                    :     :
--                collum I       collumII                 wo :_____:ist   
--            --------------------------                    /      \    
--    row I A |      B      |    not B  |    C .. --\  wo' /        \ 
--  astigmatic| '  cell I   |   cell II |            \  |  \        / ist' 
-- -- ----------------------------------------       / \|/  \______/    
--   row II A'|     B       |    not B  |         --/  C'of wo     C' of ist
-- not astigm |   cell III  |   cell IV |                     \  /
-- - matic    --------------------------                                                        ########################################################  initiate  data stream li -> Baysian -> CELLS -> BIAS 

--
--
-- compare "intern" "extern" "cell"
-- --------------------------------
--
--
-- *1) Baysian approach
-- A= the occoring and non ocurring Chars of an ideal==prior==bias ???
--  a solution depends on various As.
--  A : One based on the ordered Char list 
--  A' :Another based on the 'quirky example'
--      functions :
--
--  *Experiment3> let li4 =  ["0xy0z=3","0xy0z=3x0y0z=6","x0y0z=6","0x0yz=2","0x0yz=2xyz=11","xyz=11"]
--     -- with the solution (y=3,x=2,z=2)
--
-- A 3rd double collum C added in 
-- via a C' in 2d or in 3d with C to visualize
-- a general C where BgivenA 'CgeneralBgivenA'            
--
--               C  

  --                                       C
--
-- syntax
-- -------                                                             
-- build upon case 3 of experiment2
-- add all number digits to the solution space
-- *TheAtoBplotter> map chr (([48..128] \\ [58..96] ) \\ [123..128])
-- *> "0123456789abcdefghijklmnopqrstuvwxyz"
--  = P(B|A)*P(A)  = 36
--  add '=' to above
--  TheAtoBplotter> map chr (sort (61 : (([48..128] \\ [58..96] ) \\ [123..128])))
--  *> "0123456789=abcdefghijklmnopqrstuvwxyz" 
--  --  = P(B|A)*P(A)  = 37

-- exam3.1
-- head li = "0xy0z=3"
--  relate to A: 
-- "0xy0z=3" of li3 = 6/37
-- relate to A'
--  what is the most effective way to describe '3rd double collum' ?  { C ;  C' ; (not C)}
--
--  Test   crit1: Make any messurable example
--              premise pos
--                There is a way to generalize from Bayes to a C
--                which can be coherently described through a series of transformations
--                of a group of ptc functions that is effectivly different than 
--                one described by a series of transformations  that use
--                random number generator (luck).
--              premise neg 
--                messure more data does not always lead to find a solution.

--         crit2: Use Haskell to messure runtime to compute a solution ?
--             premise pos: shorter compute time -> Maybe No luck -> better Algorythm 
--             premise neg: by coincidence the random number generator had a lucky
--                          streak and hit a sequnece that generally matches
--                          everything within normal statistical distribution.
--                          (and no way to plot it ;)    
--
--         crit3: ???
--
--         crit4: How does the difference in li3 (with error) and li4 = (li3 without error)
--                look in ptc6 ptc7 ptc8 and ptc9 like? 
  
--
-- *write your root function 
module Experiment3 (
    
      defSearch  -- enter pg functions and progVars
    , defSearchRAW  
    --, runKRAW  -- like below but set to pick 4 variables of a bonelist
    --, runKBASE -- enter a [li,li..], decide to plot, choose search for a bonelist
     , basis2 -- added 12-9-2020 within experiment3 use for Punkt type 
     , basis4 -- as above with different output
     , checkflow -- as above could be imported to main from Colored..Writer20.hs as well
  -- export to HoofdDev
     , li4 -- input to experiment3 'quirky example' 
     , runEXP3   -- cell stream AND  data type ROLE MODEL for 'HoofdDev.hs'
     , triggerWord   -- all needed for role model
     , checkflow
     , theAChars -- string domain to compare cell streams with 
   -- own algorithms for HoofdDev
     , kArmWORK
     , inActie -- guess a solution
   -- baysian types
     , baysianTypeInt
     , baysianTypeString 
     ) where

import Data.List
import Data.Char
import Control.Monad
import System.Environment
import System.IO
--import Control.Applicative
--import Data.ByteString.Lazy
--import qualified Data.ByteString.Read as W hiding (writeFile,readFile, putstrLn,read,map,unlines,lines)

import UsefulFunctions19
import DataTypePunkt
import qualified Path2Val2 as HT
import qualified FourierFieldCLASSIFIER as F
import qualified WriteWXmaximaJuicy as M
import qualified GHCguiNfunctions as G
import qualified Colored_2_3_5_Counter20 as C

--I.
 -- Experiment3 randomly guessing solutions
-- B in A via
-- e.g
-- *Experiment3> (inActie ["0xy0z=3"] ["x0y0z=6"] ["0x0yz=2"] ["xyz=11"] li5 "xyz=11")
-- *Experiment3> let li6 = ["1b*00=3","AAA=3BAABAB","A4A=5BAB","A5=7AA","AA5=A7BBBAA","B2BBAA"] 
li6 = ["1b*00=3","AAA=3BAABAB","A4A=5BAB","A5=7AA","AA5=A7BBBAA","B2BBAA"] 
-- => will work in the 'inActie' main function of 'Experiment3.hs'

-- whereas li7 below wont becsaue the last atom 
-- is missing a number digit
-- *> let li7 = ["1b*00=3","AAA=3BAABAB","A4A=5BAB","A5=7AA","AA5=A7BBBAA","BBBAA"]
-- LI list must have 
-- 
-- the result is ONLY dealing with strings that must 
-- contain:
--      - numbers otherwise leads to an error
--      - sufficient numbers in overall li input
--      - an equal sign '='
--      - letters
-- =>  e.g "A3=sd4A"  -- will work
--        "A=sdA"    -- wont
--	"AsdA"     -- wont
--	"AAA=sd1A"   
-- WITH ABOVE AND one side of the '=' length>3 will
-- ALWAYS WORK
 
li8 = ["1A00=1","ADs=D1","AB=A1BBBBB","A1AA=AAA1","A1=1AAA","A=1sss"]
--II.
-- also generate "infinetly"  many solutions
-- *Experiment3> poolBandNotB li4 1111111
--  [61,121,120,61,121,120,61,121,120,61,121,120,61,121,120,61,121,120]
--
--  *Experiment3> map chr (poolBandNotB li4 1111111)
-- "=yx=yx=yx=yx=yx=yx

--III./IV.
-- li4                                    with zeros                without zeros
-- compare to bonelist to solution 1   expressAinA liSolution li5 pi 1    expressAsinA liSolution li5 pi 1
--                     to solution 2   expressAinA liSolution li5 pi 2    expressAsinA liSolution li5 pi 2
--  *Experiment3> expressAsinA "xyz=11" li4 pi 2
-- [48.51138353765324,48.78048780487805,48.421052631578945,43.67816091954023]     

--  *Experiment3> expressAinA "xyz=11" li4 pi 1
-- [90.54290718038528,90.59233449477351,90.52631578947368,89.65517241379311]
--
-- => change from comparing a row of a li list with its own atoms 
--   let pi = Punkt "intern" Nothing Nothing Nothing Nothing Nothing
--   OR 
--   compare with guess 
--   let pi = Punkt "extern" Nothing Nothing Nothing Nothing Nothing

                
inActie pv1 pv2 pv3 pv4 solution liSolution = do
          timE 
          let cookieForOn foAL m =  maybePu (head (ausw m foAL ))
   -- kArmWork need list length 4 , a Punkt-type that can hold data to represent
   -- the inherent structure of 
       --   let onDefault foAL m = Punkt cookieForOn foAL m pv1 pv2 pv3 pv
          let setAAxiom = theAChars
         
     --     endlessSubly <- forM [1..37]
          let seeChr =  qw liSolution "werdd" "rdddw" 3  -- just there not used (jtnu)
       
          let seperateIntandChar foli4 = aleph [head foli4] --not used so far, Just Num Chars- > Int only works on Int in String  ##################### 11-1-21 reason abt cells see conceptProject21.svg
          let mainRandomLine foli4 n = cellStream3 foli4 n
           -- generate 10 different 'random lines' length 100 
           -- f set to 49 to 128
          let runRandomLin foli4 = nub(map (cellStream3 foli4)[1,2,536,537,538,1073,1074,1075,1076,1610,1611,1613])
          
          let showZeroSpace expanS solution t = cellStream2 expanS solution t
          let solutionInRandom expanS solution t= cellStream1 expanS solution t
          let solSpace foli4 expanS s = goldTray foli4 expanS s -- = map poolB (concat$concat$concat$ausw s (allLi4 expanS))
          -- e.g*> likelyhood li4 2
          let sol100result foli4 expanS = poolBandNotB foli4 expanS -- search solutions set to max 2 
          let map100 foli4 t = tbeMapped foli4 t -- = map chr (poolBandNotB t)
          let suggest foli4 pi n =  expressAinA liSolution foli4 pi n -- compare result to li4
          let withOutZero foli4 pi n =  expressAsinA liSolution foli4 pi n -- compare to same result without zeros
         -- get two solutions B in A n>2 will lead to an error
          let drawXYZ foli4 pi n = likelyhood foli4 n  -- get an Int out of the result n	
          putStrLn "like does"
         -- let wantedList = li4 --1
          let newCellStream1 expanS sol fol100 nIdeal = let findSol sol want = map length fol100 --(organelleFind expanS sol want)
                  in let findposis want sol foN = foN `elemIndices` (findSol sol want)
                  in let mapUp want = map (findposis want sol) [1..(length sol)]
                  in map mapUp [1..6]
         -- the tbMapped
       --   let ofsix exp fol100 expanS = map (newCellStream1 expanS (map ord (head(ausw exp li4))) fol100) [1..6]
        --  let 
         -- let n= 1 
          newRekenen <- forM [1..10] (\rk -> do 
                   let forekenen2 foli4 = head$ ausw rk (runRandomLin foli4)
                   let fok2 = (forekenen2 solution)  -- length 100
                   let bnNotB2 = tbeMapped solution (head(ausw rk [1,2,537,538,1073,1074,1075,1610,1611,1613]))
--------------------------------------------------------------------------------------------------------
-- various suggested solutions
                   let oprep n =  ((ausw n (nub bnNotB2)))
                   let feed6 = (ausw rk bnNotB2)
       -- give 2 good guesses n>2 will lead to error
                   let likelyhood2 n = let step1 =  ((oprep n))
                               in let step2 = nub$reverse$sort step1
                               in if (length step2) == 1 then take 1 step1 
                                  else show ( cellInt (nub$reverse$(sort (oprep n)))-1) 
--------------------------------------------------------------------------------------------------------
--mark positions of guesses
                   withsix <- forM [1..6] (\ws -> do
                          let foliSearch = head (ausw ws solution)
                          let fromBNNotB = ausw rk bnNotB2 -- ["zz6zz6zz6zz6zz6zz6","001001001001001001",... -> [zz6zz6...] 
                          links <- forM [1..(length fromBNNotB)] (\fi -> do
                                 let fromB2 = head (ausw fi fromBNNotB)
                                 let inIt = fromB2 `elemIndices` foliSearch
                                -- agetter <- forM [1..(init)] (\ok -> do
                      --                let lwasd k = head(ausw k (ausw ws li4))
                        --              let kd = head(ausw 1 (lwasd ok))
                          --            let jk = map lwasd (map ord kd)
                            --          return (jk))
                              --   let getAction = map agetter inIt
                                 return (inIt))
                          let gather1 =  ausw 1 (concat links)

                          let gather2 =  ausw 2 (concat links)
                          let gather3 =  ausw 3 (concat links)
                          let gather = [gather1,gather2,gather3]
                        --  let getfromLi = (ausw  foliSearch-- (ausw (head gather1) foliSearch)
                          return(links))
                   
                   
                   return (bnNotB2)) --(withsix)) --(bnNotB2)) --(withsix)) --(feed6)) --bnNotB2)) --(withsix)) --(oprep rk)) --(feed6)) --(likelyhood2 3)) --(withsix))

                   
          return (newRekenen) --(mao)
    
          let mao m= head $ ausw m (((newRekenen)))
          let innerList m t = ord(head$ausw t (mao m))
          let getSolus m = do 
              chgAble <- forM [1..6] (\cg -> do
                 let varingLength = (length (mao m))
                 vL <- forM [1..varingLength] (\fvL -> do
  --[  [[[4]],[[4,11]],[[4]],[[4]],[[4,9]],[[2]]]...]  -> 	
                     let ofthat f = (ausw f (mao m))
                     let getAboveAtom =  length (ofthat fvL)
                     let fromSolution = head $ausw cg solution
                     parsE <- forM [1..getAboveAtom] (\pr -> do 
                            let oft1 = head$ ausw 1 (ofthat fvL)
                            let ghr r = r + 1
                            let finde = map ghr (oft1 `elemIndices` fromSolution)
                            let oft2 =  ausw ((head finde)+1) fromSolution
                            let togth ttg = let step1 = if ttg == [] then "+"
                                                        else  ttg --head oft2  
                                            in  ausw (head finde) (togth fromSolution)
                            return( finde))
                     return (nub$concat$parsE)) --(ofthat fvL)) --(togth))
                 return( concat vL))
              return(nub chgAble)

          let mao2 t = show (map nub (concat(ausw 1 ( getSolus t))) ) 
          putStrLn "suggested 'A'" 
          putStrLn (show newRekenen) 
       --   putStrLn (show (newCellStream1 1 (map ord (head(ausw 1 li4))) 1))
          putStrLn "continue Here !!!!" 
          putStrLn (show (head(ausw 2 (mao 1))))
          putStrLn (show ( nub (getSolus 1)))
          putStrLn ""
          HT.avanti (map mao2 [1..10])
           
          let calcTime = timE 
          putStrLn "Need calc difference for computing time"
          calcTime
       
  where
    pop e y = head $ (ausw e y); 
    quadCd w = pop w (map readAnyTh [pv1,pv2,pv3,pv4]);
    makePvs pv1 pv2 pv3 pv4 = [(quadCd 1),(quadCd 1)++(quadCd 2),(quadCd 2),(quadCd 3),(quadCd 3)++(quadCd 4),(quadCd 4)];
    iterateLi l = pop l (makePvs pv1 pv2 pv3 pv4);
-- e.g *> (head((experiment3RAW22 "DFDGGGF" li2 subroutinE C.ptc7 [li])))
-- ["\154\163hg","\155\164ih","\153\162gf"]
-- run 'calcB' until a certain boundry with simVal 
    calcB's foLiter =  (experiment3RAW22 liSolution "DFDGGGF" [solution] subroutinE C.ptc7 [foLiter]) ; -- subitile differences nand iterate
    simVal foPunkt ghAdd foli = simVALS foPunkt ghAdd foli liSolution;
  -- qw3 
    chngDomain domain guess t = map (qw liSolution guess domain) [1..(length t)] ;
    readPunktChar g = startDchange g "intern" (1,2) ; -- if fst trigger word == 1 then internal
                 -- else external 
------------------------------------------------------------------
    quadToHexPointID aToC bToC = triggerWord aToC bToC;
  -- throw in random number generator to generate chars
    randomChars r = ( ( longRun 80 r)+48);
    -- parse action , add a function via 'foCalc'
    oftheDomains n wantedList solution foCalc= countABCs n wantedList solution foCalc; 
 -- Punkt data architecture --------------------------------------
 -- r=li4 ; li4 ::  [[Char]] - > the given solution of domain A and not stigmatic
 -- B (A) of w atom 2 of r  
    bn foAL m =  maybePu (head (ausw m foAL ))
 -- store data in String because there are only 5 other spots left
 -- when reading a longer list that wont help thus store in Punkt "String"
    punktNewB foAL r =  maybePu (show(checkflow [] [((bn foAL r))]));  -- like basis22
    -- plug 'punktNewBPunkt' into this test below best to be mapped via e
    fmrTEST3 io e e2 forLine =  checkflow  io [(Punkt  (head(checkflow [] [(punktNewB e2 forLine)]))(Just (bn e 1 )) (Just (bn e 3 )) (Just (bn e 4 ))(Just (bn e 5 )) (Just (bn e 6))) ]

    -- these functions below shall lead to => a mother:TYPE that is depending on the type of simiyritYvalue
	   -- to plug any value into 'Maybe Punkt' we need a 'Maybe String'
--	   *>:t foAdecide2 (map show (Co.ptc6 100))
--	   *>foAdecide2 (map fshow (Co.ptc6 100)) :: Maybe String
    foAdecide2 foA = let boa rt t = (Just (maybePu2 rt t)) --let whereBreak = chainDistribute crit bonelist crit (lines "1")
                 in let mapMaybePun k = let ste1 k rt = (boa (head(ausw k rt))) ((Just (maybePu (head (ausw k rt)))) ) 
                                        in ste1 k foA -- e.g foA = ["vb","vb2","vb3"]
                 in let preMoa = length foA
                 in let eindelijk = do (map mapMaybePun [1..preMoa]) 
                 in 
                 if foA==[] then Nothing
                 else let chssd i = maybePu2 (head(ausw i foA))  (((boa (head(ausw i foA)) (head(ausw i eindelijk)))))  
                      in Just (show[(chssd 1)]);

------------------------------------------------------------------------------------- 15-01-21
--Same as anove but with Punkt type as selector
-- father,mother,father,mother2,loopNumber,minMaxTrueOrFalse
--  ||     ||     ||     ||       ||           ||
--  \/     \/     \/     \/       \/           \/
--
inActieRAW pv1 pv2 pv3 pv4 solution liSolution = do
          timE 
          let cookieForOn foAL m =  maybePu (head (ausw m foAL ))
   -- kArmWork need list length 4 , a Punkt-type that can hold data to represent
   -- the inherent structure of 
       --   let onDefault foAL m = Punkt cookieForOn foAL m pv1 pv2 pv3 pv
          let setAAxiom = theAChars
         
     --     endlessSubly <- forM [1..37]
          let seeChr =  qw liSolution "werdd" "rdddw" 3  -- just there not used (jtnu)
       
          let seperateIntandChar foli4 = aleph [head foli4] --not used so far, Just Num Chars- > Int only works on Int in String  ##################### 11-1-21 reason abt cells see conceptProject21.svg
          let mainRandomLine foli4 n = cellStream3 foli4 n
           -- generate 10 different 'random lines' length 100 
           -- f set to 49 to 128
          let runRandomLin foli4 = nub(map (cellStream3 foli4)[1,2,536,537,538,1073,1074,1075,1076,1610,1611,1613])
          
          let showZeroSpace expanS solution t = cellStream2 expanS solution t
          let solutionInRandom expanS solution t= cellStream1 expanS solution t
          let solSpace foli4 expanS s = goldTray foli4 expanS s -- = map poolB (concat$concat$concat$ausw s (allLi4 expanS))
          -- e.g*> likelyhood li4 2
          let sol100result foli4 expanS = poolBandNotB foli4 expanS -- search solutions set to max 2 
          let map100 foli4 t = tbeMapped foli4 t -- = map chr (poolBandNotB t)
          let suggest foli4 pi n =  expressAinA liSolution foli4 pi n -- compare result to li4
          let withOutZero foli4 pi n =  expressAsinA liSolution foli4 pi n -- compare to same result without zeros
         -- get two solutions B in A n>2 will lead to an error
          let drawXYZ foli4 pi n = likelyhood foli4 n  -- get an Int out of the result n	
          putStrLn "like does"
         -- let wantedList = li4 --1
          let newCellStream1 expanS sol fol100 nIdeal = let findSol sol want = map length fol100 --(organelleFind expanS sol want)
                  in let findposis want sol foN = foN `elemIndices` (findSol sol want)
                  in let mapUp want = map (findposis want sol) [1..(length sol)]
                  in map mapUp [1..6]
         -- the tbMapped
       --   let ofsix exp fol100 expanS = map (newCellStream1 expanS (map ord (head(ausw exp li4))) fol100) [1..6]
        --  let 
         -- let n= 1 
          newRekenen <- forM [1..10] (\rk -> do 
                   let forekenen2 foli4 = head$ ausw rk (runRandomLin foli4)
                   let fok2 = (forekenen2 solution)  -- length 100
                   let bnNotB2 = tbeMapped solution (head(ausw rk [1,2,537,538,1073,1074,1075,1610,1611,1613]))
--------------------------------------------------------------------------------------------------------
-- various suggested solutions
                   let oprep n =  ((ausw n (nub bnNotB2)))
                   let feed6 = (ausw rk bnNotB2)
       -- give 2 good guesses n>2 will lead to error
                   let likelyhood2 n = let step1 =  ((oprep n))
                               in let step2 = nub$reverse$sort step1
                               in if (length step2) == 1 then take 1 step1 
                                  else show ( cellInt (nub$reverse$(sort (oprep n)))-1) 
--------------------------------------------------------------------------------------------------------
--mark positions of guesses
                   withsix <- forM [1..6] (\ws -> do
                          let foliSearch = head (ausw ws solution)
                          let fromBNNotB = ausw rk bnNotB2 -- ["zz6zz6zz6zz6zz6zz6","001001001001001001",... -> [zz6zz6...] 
                          links <- forM [1..(length fromBNNotB)] (\fi -> do
                                 let fromB2 = head (ausw fi fromBNNotB)
                                 let inIt = fromB2 `elemIndices` foliSearch
                                -- agetter <- forM [1..(init)] (\ok -> do
                      --                let lwasd k = head(ausw k (ausw ws li4))
                        --              let kd = head(ausw 1 (lwasd ok))
                          --            let jk = map lwasd (map ord kd)
                            --          return (jk))
                              --   let getAction = map agetter inIt
                                 return (inIt))
                          let gather1 =  ausw 1 (concat links)

                          let gather2 =  ausw 2 (concat links)
                          let gather3 =  ausw 3 (concat links)
                          let gather = [gather1,gather2,gather3]
                        --  let getfromLi = (ausw  foliSearch-- (ausw (head gather1) foliSearch)
                          return(links))
                   
                   
                   return (bnNotB2)) --(withsix)) --(bnNotB2)) --(withsix)) --(feed6)) --bnNotB2)) --(withsix)) --(oprep rk)) --(feed6)) --(likelyhood2 3)) --(withsix))

                   
          return (newRekenen) --(mao)
    
          let mao m= head $ ausw m (((newRekenen)))
          let innerList m t = ord(head$ausw t (mao m))
          let getSolus m = do 
              chgAble <- forM [1..6] (\cg -> do
                 let varingLength = (length (mao m))
                 vL <- forM [1..varingLength] (\fvL -> do
  --[  [[[4]],[[4,11]],[[4]],[[4]],[[4,9]],[[2]]]...]  -> 	
                     let ofthat f = (ausw f (mao m))
                     let getAboveAtom =  length (ofthat fvL)
                     let fromSolution = head $ausw cg solution
                     parsE <- forM [1..getAboveAtom] (\pr -> do 
                            let oft1 = head$ ausw 1 (ofthat fvL)
                            let ghr r = r + 1
                            let finde = map ghr (oft1 `elemIndices` fromSolution)
                            let oft2 =  ausw ((head finde)+1) fromSolution
                            let togth ttg = let step1 = if ttg == [] then "+"
                                                        else  ttg --head oft2  
                                            in  ausw (head finde) (togth fromSolution)
                            return( finde))
                     return (nub$concat$parsE)) --(ofthat fvL)) --(togth))
                 return( concat vL))
              return(nub chgAble)

          let mao2 t = show (map nub (concat(ausw 1 ( getSolus t))) ) 
         -- let punktMaschine = Punkt name 
          putStrLn "suggested 'A'" 
          let outBandNotB = (show newRekenen) 
       --   putStrLn (show (newCellStream1 1 (map ord (head(ausw 1 li4))) 1))
          putStrLn "continue Here !!!!" 
          let outLetterB = (show (head(ausw 2 (mao 1))))
          let outMatrixX =  (show ( nub (getSolus 1)))
          putStrLn ""
          let moreMystery =  (map mao2 [1..10])
          --let chooseOut = do 
            --      if gD == 1 then  
          let calcTime = timE 
          putStrLn "Need calc difference for computing time"
          calcTime
       
  where
    pop e y = head $ (ausw e y); 
    quadCd w = pop w (map readAnyTh [pv1,pv2,pv3,pv4]);
    makePvs pv1 pv2 pv3 pv4 = [(quadCd 1),(quadCd 1)++(quadCd 2),(quadCd 2),(quadCd 3),(quadCd 3)++(quadCd 4),(quadCd 4)];
    iterateLi l = pop l (makePvs pv1 pv2 pv3 pv4);
-- e.g *> (head((experiment3RAW22 "DFDGGGF" li2 subroutinE C.ptc7 [li])))
-- ["\154\163hg","\155\164ih","\153\162gf"]
-- run 'calcB' until a certain boundry with simVal 
    calcB's foLiter =  (experiment3RAW22 liSolution "DFDGGGF" [solution] subroutinE C.ptc7 [foLiter]) ; -- subitile differences nand iterate
    simVal foPunkt ghAdd foli = simVALS foPunkt ghAdd foli liSolution;
  -- qw3 
    chngDomain domain guess t = map (qw liSolution guess domain) [1..(length t)] ;
    readPunktChar g = startDchange g "intern" (1,2) ; -- if fst trigger word == 1 then internal
                 -- else external 
------------------------------------------------------------------
    quadToHexPointID aToC bToC = triggerWord aToC bToC;
  -- throw in random number generator to generate chars
    randomChars r = ( ( longRun 80 r)+48);
    -- parse action , add a function via 'foCalc'
    oftheDomains n wantedList solution foCalc= countABCs n wantedList solution foCalc; 
 -- Punkt data architecture --------------------------------------
 -- r=li4 ; li4 ::  [[Char]] - > the given solution of domain A and not stigmatic
 -- B (A) of w atom 2 of r  
    bn foAL m =  maybePu (head (ausw m foAL ))
 -- store data in String because there are only 5 other spots left
 -- when reading a longer list that wont help thus store in Punkt "String"
    punktNewB foAL r =  maybePu (show(checkflow [] [((bn foAL r))]));  -- like basis22
    -- plug 'punktNewBPunkt' into this test below best to be mapped via e
    fmrTEST3 io e e2 forLine =  checkflow  io [(Punkt  (head(checkflow [] [(punktNewB e2 forLine)]))(Just (bn e 1 )) (Just (bn e 3 )) (Just (bn e 4 ))(Just (bn e 5 )) (Just (bn e 6))) ]

    -- these functions below shall lead to => a mother:TYPE that is depending on the type of simiyritYvalue
	   -- to plug any value into 'Maybe Punkt' we need a 'Maybe String'
--	   *>:t foAdecide2 (map show (Co.ptc6 100))
--	   *>foAdecide2 (map fshow (Co.ptc6 100)) :: Maybe String
    foAdecide2 foA = let boa rt t = (Just (maybePu2 rt t)) --let whereBreak = chainDistribute crit bonelist crit (lines "1")
                 in let mapMaybePun k = let ste1 k rt = (boa (head(ausw k rt))) ((Just (maybePu (head (ausw k rt)))) ) 
                                        in ste1 k foA -- e.g foA = ["vb","vb2","vb3"]
                 in let preMoa = length foA
                 in let eindelijk = do (map mapMaybePun [1..preMoa]) 
                 in 
                 if foA==[] then Nothing
                 else let chssd i = maybePu2 (head(ausw i foA))  (((boa (head(ausw i foA)) (head(ausw i eindelijk)))))  
                      in Just (show[(chssd 1)]);
-------------------------------------------------------------------------------------------------------------------------------------------------------
 
-- PART II using the ptc functions as described in Experiment3 
----------------------------------------------------------------------------
 ----- #####################################################################
-- write your subroutine
--  wxmWritetoken :: Int, pass a token to write various wxms in runKBASE
--                       without being stuck at the same file
--                       overwriting what was gained in the iteration
--  lalas = wxmWritetoken 
--  subroutine :: ptc ptc ptc ptc ptc ptc lalas -> list -> [Double] 
subroutinE = [[(C.ptc6 5),(C.ptc6 25),(C.ptc6 50),(C.ptc6 100),(C.ptc6 125),(C.ptc6 150),[[1.0]]],{-
             -}[(C.ptc6 5),(C.ptc6 25),(C.ptc6 50),(C.ptc6 100),(C.ptc6 125),(C.ptc6 150),[[2.0]]] ]   {--,{-  a plaine 
                -}[(C.ptc6 5),(C.ptc6 25),(C.ptc6 50),(C.ptc6 100),(C.ptc6 125),(C.ptc6 150),[[2.0]]],{- interesting
                -}[(C.ptc7 5),(C.ptc7 25),(C.ptc7 50),(C.ptc7 100),(C.ptc7 125),(C.ptc7 150),[[3.0]]],{- half 'crown'
                -}[(C.ptc8 5),(C.ptc8 25),(C.ptc8 50),(C.ptc8 100),(C.ptc8 125),(C.ptc8 150),[[4.0]]],{- similar to above
                -}[(C.ptc9 5),(C.ptc9 25),(C.ptc9 50),(C.ptc9 100),(C.ptc9 125),(C.ptc9 150),[[5.0]]]] -}
----------------------------------------------------------------------------


-------------------------------------------------------------------------
theDs f g = concat$ausw f ((concat g))
-- df2 ttt offOn target plot addGh ghAdd n (theDs ttt d) (get1) (get2) (get3)  (get4) 1 (theDs ttt d)
-- =  runKAXIOM offOn target plot addGh ghAdd n (theDs ttt d) (get1) (get2) (get3)  (get4) 1 (theDs ttt d)

-- e.g>runKBASE 1 [1,2] 1 2 "AAA" 1 (li3) 1 2 3 4 subroutinE "xyz=11"

runKBASE offOn target plot addGh ghAdd n d get1 get2 get3 get4 subroutineList liSolution = do
       allforIV <- forM [1..(length target)] (\four4 -> do
            let fg = runKAXIOM offOn target plot addGh ghAdd n (theDs four4 d) (get1) (get2) (get3) (get4) 1 (theDs four4 d) (four4) [(read(show four4))] ((tk four4 subroutineList)) liSolution
            fg
            return (fg))

       header <- readFile "HtmlS/allRowsHeader2.txt"
       taiL <- readFile "HtmlS/allRowsTail2.txt"

   --    let whichopen =  if clicks==0 then 7
     --                   else if clicks<7 then (tzExp)-1
       --                 else 1
--   wirs <- readFile ("HtmlS/foyourRun"++show whichopen++".txt")
  -- wirs2 <- readFile ("HtmlS/foyourRun"++show (whichopen)++".txt")
       writeAll <- forM [1..(length target)] (\dt -> do
            wirs <- readFile ("HtmlS/foyourRun"++(show (dt+1))++".txt")
            return (wirs))
     --  nugget = map ord nuDoe
     --  let gbb = map chr [nugget]  tsRAW
       let theListIV = (header++unwords writeAll++(G.screenInfo (map lines["which info"]) 1) ++taiL)
       writeFile "HtmlS/yourRun.html" (theListIV) 
       writeFile (root++"/src/index2.html") (theListIV)

-----------------------------------------------------------
experiment3RAW offOn target plot addGh ghAdd n d get1 get2 get3 get4 subroutineList liSolution= do
       allforIV <- forM [1..(length subroutineList)] (\four4 -> do
            let fg = runKAXIOM offOn target plot addGh ghAdd n (theDs four4 d) (get1) (get2) (get3) (get4) 1 (theDs four4 d) (four4) [(read(show four4))] ((tk four4 subroutineList)) liSolution
            fg
            return (fg))
       return allforIV

-- d: [[String]] a list of bonelist or li 
experiment3RAW2 addGh ghAdd n d subroutineList = experiment3RAW 2 [1..(length subroutineList)] 2 addGh ghAdd n d 1 2 3 4 subroutineList

-- filter everthing smaller char 43 = '+' , '*',' ' ... -------------------------########################################## prep any bonelist for reading
-- thus can be fully processed as li list chars < 43 lead to error
-- *e.g>  readAnyTh (C.progLiT)
readAnyRAW foLiT t = let stapa t = map chr (filter (>43) (map ord (head$ausw t foLiT)))
                     in stapa t

readAnyTh foLiT = map (readAnyRAW foLiT) [1..6]

-- set with subroutin list length 2
-- *e.g> experiment3 "ttt" [li2]
experiment3 ghAdd d = experiment3RAW2 1 ghAdd 1 d subroutinE
-- see if section in runKAxiom :777 'experiment3 'can be deleted !?
-- function below has better properties ?
-- -- *e.g> experiment3RAW22 "DF" li2 subroutinE C.ptc7 li2
-----------------------------------------------------------------------------------------------
-- start getting 2x2 3x3 5x5 coordinate pairs in
-- experiment3RAW11 "DF" li2 subroutinE C.ptc7 li2
-- *> 
-- nforCalc: Int to be fed into 5x5 to clacuate coordinate pairs
-- fopi: Punkt see differences internal pi2 is to generate Chars via 'runEXP3Char'
experiment3RAW11 liSolution ghAdd d subroutineList foPtc foLi nforCalc fopi= do
   -- givens:
       let moreReads = do   
            exp3Fractional <- forM [1..(length subroutineList)] (\four4 -> do
                  let exP3 =  runEXP3 (head(ausw four4 d)) liSolution fopi ghAdd --kWORK --(G.fobase progVar1 progVar2 progVar3 progVar4 progVar5 progVar6 daZip1 daZip2 daZip3 textAA (ptcButoons) (foalt))
                  return (exP3))
            return exp3Fractional
       moreReads
       let exp3Frac = moreReads
       let minS = [(minimum maXxS),(minimum maXyS),(minimum maXzS)]
     -- 'quirky example'  --
       let calcAlgoExperiment3 =  let triangle_' =  [(b',c'),(b',0),(0,0)]
                                       in let triangle_'' = [(b',c'),(0,0),(0,a')]
                                       in let triangle_k m = [(b',c'),(b',a'),(b,a')]
                                     --  in let triangle_p = 
                                       in let wriSVG ank = C.fofina2 foPtc ank 1 1 
                                       in let testField = C.foBrad 
                                       in let testHex = C.aHexa
                                    {-   in let punktFit = do 
                                                     let prepA = [[maxX,maxY,maxY], minS] -- wriSVG testHex --triangle_k 1 --map getOutX twoXtwo --[twoXtwo,threeXthree,(fiveXfive nforCalc)] --fiveXfive 1
                                                     polyLine <- forM [1,2] (\ly -> do  -- any geometric shape higher than 1 
                                                               let plugCol = colorList ly 
                                                               aMonada <- forM [1..(length anchor)] (\os -> do 
                                                                   let conLongituda =  (tk os anchor)
                                                                   let readMore = if length anchor == 1 then 1 
                                                                                  else (length conLongituda)
                                                                   innRead <- forM [1..(length conLongituda)] (\cs -> do 
                                                                          let gtFst = show$fst$head$ausw cs conLongituda
                                                                          let gtSnd = show$snd$head$ausw cs conLongituda
                                                                          let inPlug = graphs ((unwords( map fst$dotSize ly))++" "++(unwords(map snd$dotSize ly)))                    
                                                                          return (inPlug))
                                                               return (innRead))
                                                     return (polyLine)
                                       return punktFit -}
                                       --in punktFit 
                                       in wriSVG testField
       calcAlgoExperiment3
  where
                   maxX = maximum maXxS;
                   maxY = maximum maXyS;
                   maxZ = maximum maXzS;
                   pi2 = Punkt "extern" Nothing Nothing Nothing Nothing Nothing;
                   ert r = tk r (foPtc 250);
                   xS r = head (ert r)  ;
                   yS r = last (drop 1 (take 2 (ert r))) ;
                   zS r = last (ert r) ;
                   maXxS = map xS [1..100];   
                   maXyS = map yS [1..100];
                   maXzS = map zS [1..100] ;  
                   --bOn = exp3Frac;
                   twoXtwo = [(10,maxY),(10,15),(0,15),(0,maxY)];
                   fo3y = (maxY - (maxY- (maxX-11)));
                   threeXthree = [(maxX,maxY), (maxX, 15),(11,fo3y),(11,maxY)];
                   varX x = (maxX- ( (maxX-10)- (x))); 
                   fiveXfive x = [((varX x),25), ((varX x), 0),(0,0),((varX x),25)];
                   getOutX t = fst t;
                   getOutY t = snd t; 
                   hex2x2 = [(10, maxY),(0,15),(0,0)];
                   -- searched a' alpha c = (sin alpha) * c;
                   mAscent ins which = let step1 t r = head$ausw r t
                             in let fomapS = (length ins) 
                             in let mapS = map (step1 ins) [1..(fomapS)]
                             in let x1S = getOutX$head (ausw which mapS) --getOutX mapS
                             in let y1S = getOutY$head (ausw which mapS) --getOutX mapS
                             in let x2S = getOutX$head (ausw (which +1) mapS) 
                             in let y2S = getOutY$head (ausw (which +1) mapS) --getOutY mapS 
                             in let calc_m = (y2S - y1S) / (x2S -x1S)  -- (y2-y1)/(x2-x1)
                             in sqrt (x1S^2+y1S^2)
                   foa'' r = (head(ausw r hex2x2)) ;
                   fob'' r = (head(ausw r threeXthree)) ;
                   foc'' r m = (head(ausw r (fiveXfive m))) ;
                   a''= (getOutY (foa'' 3));  -- y pf Cq is the y coordinate of Point C(3) of towXtwo
                   pointCk m = (( a''),(m*a''+ 0));
                   xAqof2x2 = getOutX (foa'' 1); -- x of A quadrant of 2x2 
                   yAqof2x2 = getOutY (foa'' 1); -- a of 2x2 = (y of Aq) 
                   yBqof2x2 = getOutY (foa'' 2) ;
                   xBqof3x3 = getOutX (fob'' 3) ;
                   a' = yBqof2x2;
                   b' = xAqof2x2;
                   c' = c;
                   c =  yAqof2x2;
                   b'' = sqrt (( (xAqof2x2 - yBqof2x2)^2) + yBqof2x2^2);
                   c''= xAqof2x2; -- (yAof2x2 - yBof2x2);
                   aAk m = xBqof3x3  -- + (b' - (getOutY (foa'' 1 )));
                   bBp m = xAqof2x2 - (aAk m); --  ;-- side b o f triangle p
                   cCk = yAqof2x2 - yBqof2x2; 
                   alpha = a'/b';
                   a = (sin alpha)* c;
                   cosAlpha = ((a'^2)/(-2*b'*c)) - ((b'^2)/(-2*b'*c)) -((c^2)/(-2*b'*c));
                  -- get b with b' of above
                   cosBeta = (cos 90);
                 -- + 1 due to offset of whole wxm data
                   b = ((((((-1)* a)) + (((-1)*c)) - (sqrt(2*a*c**cosBeta)))/10)-1)*(-1);
                   
--------------------------------------------

------------------------------------------------------------------------------
-- guess something to add to First + -
-- experiment3RAW22 "xyz=11" "DF" li2 subroutinE C.ptc7 li2
-- *> map ord (head(head(experiment3RAW22 "xyz=11" "DFDF" li2 subroutinE C.ptc7 [li3])))
-- *> [20,27,20,20]
experiment3RAW22 liSolution ghAdd d subroutineList foPtc foLi = do
   -- givens:
       let pi2 = Punkt "m" Nothing Nothing Nothing Nothing Nothing
       let ert r = tk r (foPtc 250)
       let xS r = head (ert r)  
       let yS r = last (drop 1 (take 2 (ert r))) 
       let zS r = last (ert r) 
       let maXxS = map xS [1..100]   
       let maXyS = map yS [1..100]
       let maXzS = map zS [1..100]   
       let moreReads = do   
            exp3Fractional <- forM [1..(length subroutineList)] (\four4 -> do
                  let exP3 =  runEXP3 (head(ausw four4 d)) liSolution pi2 "AAA" --kWORK --(G.fobase progVar1 progVar2 progVar3 progVar4 progVar5 progVar6 daZip1 daZip2 daZip3 textAA (ptcButoons) (foalt))
                  return (exP3))
            let exp3Frac = concat$concat$exp3Fractional
            exp3IntfoChar <- forM [1] (\four4 -> do

                     let comp1 = runEXP3Char pi2 liSolution ghAdd (head (ausw four4 foLi))
                     let comp2 = (zipWith (+) [1,1,1,1] (runEXP3Char pi2 liSolution ghAdd (head (ausw four4 foLi))))
                     let comp3 = let mis t z = z - t
                                 in (map (mis 1) (runEXP3Char pi2 liSolution ghAdd (head (ausw four4 foLi))))
                     let thecombs = [comp1,comp2,comp3]
                     return ([comp1,comp2,comp3]))-- (thecombs))
            let exp3Intfo t = head $ ausw t $ concat$exp3IntfoChar 
            let maxX = maximum maXxS
            let maxY = maximum maXyS
            let maxZ = maximum maXzS
     -- 'quirky example'
            let calcAlgoExperiment3 =  twoXtwo --fiveXfive 1
                  where
                   bOn = exp3Frac;
                   twoXtwo = [(10,maxY),(10,15),(0,15),(0,maxY)];
                   fo3y = (maxY - (maxY- (maxX-11)));
                   threeXthree = [(maxX,maxY), (maxX, fo3y),(11,fo3y),(11,maxY)];
                   varX x = (maxX- ( (maxX-10)- (x))); 
                   fiveXfive x = [((varX x),25), ((varX x), 0),(0,0),((varX x),25)];

            let cE3 = calcAlgoExperiment3
            let foRunner gb1 gb2 = let foGb1 gb =  map realToFrac (map ord gb )
                                   in let foGb2 gb =  map realToFrac (map ord gb )
                                   in  similaritYvalue (foGb1 gb1) (foGb2 gb2)
            let ptcToInt m = let loadInBlank = map (\c -> if c=='.' then ' '; else c)  
                             in let myTruncate = read$head$words$loadInBlank m
                             in myTruncate
            aRunner <- forM [1..3] (\aRon -> do
                     let bsp = map chr (exp3Intfo aRon)
                     return (bsp))
            -- map chr ((exp3Intfo))
    --   show (aRunner ) -- show cE3 --exp3Frac
      -- return (aRunner )
      -- show (take 1 exp3Intfo)
       --exp3IntfoChar
            return cE3 --aRunner
       moreReads 
     --  show (aRunner 1)
    --   show (aRunner 2)
    --   show (aRunner 3)
     --  show (aRunner 4)
     --  show (aRunner 5)
------------------------------------------------------------
--similar to above shall be iterated
experiment3RAW23 liSolution ghAdd d subroutineList foPtc foLi gb1 gb2 = do
   -- givens:
       let pi2 = Punkt "m" Nothing Nothing Nothing Nothing Nothing
       let ert r = tk r (foPtc 250)
       let xS r = head (ert r)  
       let yS r = last (drop 1 (take 2 (ert r))) 
       let zS r = last (ert r) 
       let maXxS = map xS [1..100]   
       let maXyS = map yS [1..100]
       let maXzS = map zS [1..100]   
       let moreReads = do   
            exp3Fractional <- forM [1..(length subroutineList)] (\four4 -> do
                  let exP3 = [ runEXP3 (head(ausw four4 d)) liSolution pi2 "AAA" ]--kWORK --(G.fobase progVar1 progVar2 progVar3 progVar4 progVar5 progVar6 daZip1 daZip2 daZip3 textAA (ptcButoons) (foalt))
                  return (exP3))
            let exp3Frac = concat$concat$exp3Fractional
            exp3IntfoChar <- forM [1] (\four4 -> do

                     let comp1 = runEXP3Char pi2 liSolution ghAdd (head (ausw four4 foLi))
                     let comp2 = (zipWith (+) [1,1,1,1] (runEXP3Char pi2 liSolution ghAdd (head (ausw four4 foLi))))
                     let comp3 = let mis t z = z - t
                                 in (map (mis 1) (runEXP3Char pi2 liSolution ghAdd (head (ausw four4 foLi))))
                     let thecombs = [comp1,comp2,comp3]
                     return ([comp1,comp2,comp3]))-- (thecombs))
            let exp3Intfo t = head $ ausw t $ concat$exp3IntfoChar 
            let maxX = maximum maXxS
            let maxY = maximum maXyS
            let maxZ = maximum maXzS
     -- 'quirky example'
            let calcAlgoExperiment3 =  twoXtwo --fiveXfive 1
                  where
                   bOn = exp3Frac;
                   twoXtwo = [(10,maxY),(10,15),(0,15),(0,maxY)];
                   fo3y = (maxY - (maxY- (maxX-11)));
                   threeXthree = [(maxX,maxY), (maxX, fo3y),(11,fo3y),(11,maxY)];
                   varX x = (maxX- ( (maxX-10)- (x))); 
                   fiveXfive x = [((varX x),25), ((varX x), 0),(0,0),((varX x),25)];

            let cE3 = calcAlgoExperiment3
            let foRunner fogb1 fogb2 = let foGb1 gb =  map realToFrac (map ord gb )
                                       in let foGb2 gb =  map realToFrac (map ord gb )
                                       in  similaritYvalue (foGb1 fogb1) (foGb2 fogb2)
            let ptcToInt m = let loadInBlank = map (\c -> if c=='.' then ' '; else c)  
                             in let myTruncate = read$head$words$loadInBlank m
                             in myTruncate
            aRunner <- forM [1..3] (\aRon -> do
                     let bsp = map chr (exp3Intfo aRon)
                     return (bsp))
            let se =  if  (foRunner gb1 gb2) < 10.0 then foRunner gb1 gb2 --aRunner --map chr ((exp3Intfo)) --show$ "2" --foRunner (ptcToInt maxX) (ptcToInt maxY)
                      else foRunner gb1 gb2 --aRunner --show $ "3" -- zufallsBasic1 10 (ptcToInt maxX) 1
            -- map chr ((exp3Intfo))
    --   show (aRunner ) -- show cE3 --exp3Frac
      -- return (aRunner )
      -- show (take 1 exp3Intfo)
       --exp3IntfoChar
            return aRunner --se
       moreReads

-- search in string ap3 
-- the letter that is at 
-- position t of ap
-- *TheAtoBplotter> focommmB li "AABB" "AVFGB" 3
--               *> [4]
focommmB ap ap3 t = let atSpot ap ap3 t = (head (ausw t ap)) `elemIndices` (ap3)
                    in atSpot ap ap3 t
-- guess: String e.g "DrF3" 
buildPrior liSolution r guess  = let buildA r = readAnyTh r
                      in head$ last (experiment3RAW22 liSolution guess [buildA r] subroutinE C.ptc7 [buildA r]) 

commmB liSolution ap ap3 t = let buildA r = readAnyTh r
         --  in let aprior = head$ last (experiment3RAW22 "DF" [buildA r] subroutinE C.ptc7 [buildA r]) 
           in let atSpot ap ap3 t = (head (ausw t (map ord ap))) `elemIndices` (map ord ap3)
           in let aprior4 gb1 gb2 r = head$ last (experiment3RAW23 liSolution "DF" [(buildA r)] subroutinE C.ptc7 [buildA r] gb1 gb2)
          -- in let testIterate ap = atSpot ap (aprior4 aprior "fp")
           in atSpot ap ap3 t--aprior

commmB2 liSolution ap ap3 t = let buildA r = readAnyTh r
         --  in let aprior = head$ last (experiment3RAW22 "DF" [buildA r] subroutinE C.ptc7 [buildA r]) 
           in let atSpot ap ap3 t = (head (ausw t (map ord ap))) `elemIndices` (map ord ap3)
           in let aprior4 gb1 gb2 r = head$ last (experiment3RAW23 liSolution "DF" [(buildA r)] subroutinE C.ptc7 [buildA r] gb1 gb2)
          -- in let testIterate ap = atSpot ap (aprior4 aprior "fp")
           in aprior4 ap ap3 t--aprior

-----------------------------------------------------------------
-- P(B) given AStigmatic 
-- A= the occoring and non ocurring Chars of an ideal
--  However the solution depends on various As.
--  A : One based on an seemingly ordered Char list 
--  A' :Another based on the 'quirky example'
--      functions :
--  *TheAtoBplotter> let li3 =  ["0xy0z=3","0xy0z=33x00z=6","3x00z=6","0x0yz=2","0x0yz=2xyz=11","xyz=11"]
--     -- with the solution (y=3,x=2,z=2)
-- (find the error in li3)
-- A 3rd double collulm C added in 
-- via a C' in 2d or in 3d with C to visualize
-- a general C where BgivenA 'CgeneralBgivenA'            
--
-- concept                                                   A _____B
-- -------                                                    /     \
--             |      B      |    not B  |    C .. ----\  A' /       \  not
-- Astigmatic  | 'aBgivenA'  |           |              \    \       /  B
-- -- -----------  ----------------------------         /     \____ /
-- not Astigm- |                                   ----/      C'    C'
-- -- matic    |                                                \  /
--                                                               C
--
-- syntax
-- -------                                                             
-- build upon case 3 of experiment2
-- add all number digits to the solution space
-- *TheAtoBplotter> map chr (([48..128] \\ [58..96] ) \\ [123..128])
-- *> "0123456789abcdefghijklmnopqrstuvwxyz"
--  = P(B|A)*P(A)  = 36
--  add '=' to above
--  TheAtoBplotter> theA = map chr (sort (61 : (([48..128] \\ [58..96] ) \\ [123..128])))
--  *> "0123456789=abcdefghijklmnopqrstuvwxyz" 
--  --  = P(B|A)*P(A)  = 37
theAChars = map chr (sort (61 : (([48..128] \\ [58..96] ) \\ [123..128])))

-- exam3.1
-- head li = "0xy0z=3"
--  relate to A: 
-- "0xy0z=3" of li3 = 6/37
-- relate to A'
--  what is the most effective way to describe '3rd double collum' ?  { C ;  C' ; (not C)}
--
--  Test   crit1: Make any messurable example
--              premise pos
--                There is a way to generalize from Bayes to a C
--                which can be coherently described through a series of transformations
--                of a group of ptc functions that is effectivly different than 
--                one described by a series of transformations  that use
--                random number generator (luck).
--              premise neg 
--                messure more data does not always lead to find a solution.

--         crit2: Use Haskell to messure runtime to combute a solution ?
--             premise pos: shorter compute time -> Maybe No luck -> better Algorythm 
--             premise neg: by coincidence the random number generator had a lucky
--                          streak and hit a sequnece that generally matches
--                          everything within normal statistical distribution.
--                          (and no way to plot it ;)             
--       --
--         crit3: When introducing C do a special 'chain destribution'
--                that is A chain destributed in A'. 
--
--         crit4: How does the difference in li3 (with error) and li4 = (li3 without error)
--                look in ptc6 ptc7 ptc8 and ptc9 like? 
{-
 usedVowel -> P(B, given A) = "eo" = 2/2+3 = 2/5= 0.4

 usedNOTVowel -> (PA, given B) = "prsn" = 4/6

 NotusedVowel&NotusednotVowel -> case3 \\ "person" 
"abcdfghijklmqtuvwxyz"  = 20/26

 NotusedVowel = "aiu" = 3/26

=>  B, given A :

(usedVowel / (usedVowel+ usedNOTVowel))
*
(usedVowel+notusedVowel)/(usedVowel+notusedVowel+usedNotVowel+notusedNotVowel)
-}

-- special chain destribution (Cd)
easyCd liSolution wo k = sort$group k >>= (findinCd liSolution wo)
--------------------------------------
-- bound to A of 'ist' via the Syntax via commmB
-- wo  :: [Char], can be B of astigmatic
--                    or A + - change (sum, change a little)
--
--   =>propose, 'wo' can be everything EXCEPT not A of Haskell Char encoding !?  
--
-- ist :: [Char] , is a guess  
-- Experiment3> foStrCd "xyz=11" "wer" "r" 3
-- [1]  -- where is 'r' in 'wer' look ONLY at occurance position 3 (spot 3) 
foStrCd liSolution wo ist das =  commmB liSolution wo ist das

-- still collapses if mWidth does not occure -> if length k < length mWidth == true
--                                           then Error0
dasError0 k mWidth = if (length k<= mWidth) then (length k)
                             else mWidth

-- still collapses if mWidth does not occure -> if length k < length mWidth == true
--                                           then Error0
dasErrHnl k nHeight mWidth = if (length k<=length mWidth) then (length k)
                             else if (length k<length nHeight) then (length nHeight)
                             else if (length mWidth<length k)&&(length mWidth<length k) then read nHeight
                             else length mWidth


--cell1to4: 12-1-21 ----------------------------------------------------######################################################reason about cell I ..IV 
--doesnt need Int input as syntax but as concept -> condition length k >= length ist
--                                               => needs right length ist -> Int
--                                                  needs right order to be cellI...
--besides error proneness there must be an right order to 
-- be cellI <- cellII <- cellIII <- always starts cellIV
--                                  with Punkt Error handler always could?
--                               <- right letters 
--                               <- right order of letters e.g 'chainDistribute'
--  both types are still unsafe                            
baysianTypeString liSolution wo ist = map (foStrCd liSolution wo ist) [1..(dasError0 ist (length ist))]
-- still collapses if mWidth does not occure -> if length k < length mWidth == true
--                                           then Error0
-- stores a String -> Int in nHeight
-- wo: String , a population
--
baysianTypeInt liSolution wo nHeight ist = map (foStrCd liSolution wo ist) [1..(dasErrHnl ist nHeight ist)]
--------------------------------------------------------------------------------------------------------------

-- before 1-1-21
findinCdRAW liSolution wo ist = (map (foStrCd liSolution wo ist) [1..(length ist)])
findinCd liSolution wo ist = (findinCdRAW liSolution wo ist)
-- reflexive function show me where B in A =>
qw liSolution wo ist r = (foStrCd liSolution wo ist r)


--find the gap in the data 11-1-21
-- show the occurance of Char number n in a [Char]
-- *Experiment3> foStrCd "xyz=11" "werfgrrrrf" "wrerfrfrrf" 4
-- [4,6,9]
-- *Experiment3> foStrCd "werfgrrrrf" "wrerfrfrrf" 5
-- []
-- *Experiment3> foStrCd "werfgrrrrf" "wrerfrfrrf" 6
-- [1,3,5,7,8]
----------------------------------------
-- pre 2021
-- qw3 ready to map for different ist's(guesses)
-- domain: higher domain of this program
--         if 'domain' = "" then no restrictions
--         else see below
-- guess(wo): must be (length wo) >= (length t)  --------otherwise error 
-- t    : must be (length t) <=  (length wo) 
--        determines length of ANY domain EXCEPTION theAChar
--
--        no matter if qw3 is used wo is always 
--          B of astigmatic  
--   OR     B of (    "      + not aticmatic ) 
--   ... always the higher domain 
--
--       with regard to 't'
--
--choose a cell or row or collum always wo > ist 
-- wo-domain   higher domain of ist-domain
-- concept Cd               
--                                                          
--                collum I       collumII                 wo ______ist   
--            --------------------------                    /      \    
--    row I   |      B      |    not B  |    C .. --\  wo  /        \ 
--        A   | '  cell I   |   cell II |            \  |  \        / ist 
-- -- ----------------------------------------       / \|/  \______/    
--   row II   |     B       |    not B  |         --/  C'of wo     C' of ist
-- not stigm- |   cell III  |   cell IV |                     \  /
-- - matic    --------------------------                        C  

-- --------
qw3 liSolution domain guess t = map (qw liSolution guess domain) [1..(length t)]
--   solve where is B in A
--               is B in A'
--               is B in C'



-------------------------------------------------------------------------------------------------------------------A's ?
theASyntax = theAChars -- solve relation to Char encoding in Haskell
---- A' source for an A' 'quirky example' without errors
-- solution (y=3,x=6,z=2) == A'' ?!
-------------------------------------------------------------------------------------------------------------------A' nand/nor A'' ?
li4 =  ["0xy0z=3","0xy0z=3x0y0z=6","x0y0z=6","0x0yz=2","0x0yz=2xyz=11","xyz=11"]
theA' = head li4


--theB's -- of commmB
------------------------------------ ##################################################################### B's
-- we can run a list of the length n and see which letters of a solution occure.
--   1. assumption 1 all Chars in the solution will occure given all Chars from 48 .. until 128 
--                    WITHOUT...

--guess based on 'quirky example'
-- based on variable li2
-- li :: [[String]]
-- li3 :: [String]
theB' liSolution li2 li3= (head(head(experiment3RAW22 liSolution "DFDGGGF" li2 subroutinE C.ptc7 [li3]))) 

-- choose 4 variables as to build pv functions
-- progVar1 = "0*x + y + 0*z = 3"
--progVar2 = "0*x + y + 0*z = 33*x + 0 + 0*z = 6"
--progVar3 = "3*x + 0 + 0*z = 6"
--progVar4 = "0*x + 0*y + z = 2
--progVar5 = "0*x + 0*y + z = 2x + y + z = 11"
--progVar6 = "x + y + z = 11"
--
--progVar1 = "0*x + y + 0*z = 3"
--progVar2 = "0*x + y + 0*z = 33*x + 0 + 0*z = 6"
--progVar3 = "3*x + 0 + 0*z = 6"
--progVar4 = "0*x + 0*y + z = 2
--progVar5 = "0*x + 0*y + z = 2x + y + z = 11"
--progVar6 = "x + y + z = 11"
-- *Experiment3> (head((experiment3RAW22 "DFDGGGF" [li4] subroutinE C.ptc7 [li])))
--["\154\163hg","\155\164ih","\153\162gf"]
theBQuirky liSolution li4 li = (head((experiment3RAW22 liSolution "DFDGGGF" [li4] subroutinE C.ptc7 [li])))

-- put CONSTRAINT On NOTES -> do come back to experiment3RAW22 
--  inspect 2x2 3x3 5x5 to follow instructions
--
--  every solution is a String every Char of this [Char]
--  fills a space of soltions e.g of

-- *Experiment3> triggerWord "cell" "cell11"
-- "2"
-- *> triggerWord "cell" "cell"
-- "0"
-- *> triggerWord "cell123" "cell"
-- "cell"
--for matches in 'elemIndices' via
-- focommB, commB, runCell,oglleLocateSolu
--
--      derive the position of matches e.g 
--      in oglleLocateSolu wanted solution nIdeal =>       
--  3     -----> randomGuesses ---> Maybe not B ---> continue search
--                             ---> Maybe B ------> solution after search
--                             ---> Maybe ( not b) && Not A ---> continue search or change the solution 
-- e.g*> (inActie ["0xy0z=3"] ["x0y0z=6"] ["0x0yz=2"] ["xyz=11"])                  
-- -- a group of functions that shall a [(Maybe Punkt)]-> that by itself is the definiton of
-- mother :: Punkt -> Maybe Punkt
 

 
-- a general C where BgivenA 'CgeneralBgivenA'            
--
-- concept                                                  
-- -- -------    
--  the rod to hold the carrot
--  to drag us to where we should go ! 
--      A _____B
--       /     \
--  BA' /       \  not
-- A |  \       /  B
-- -- ---\____ /
-- not stiC'    C'
-- -- mati\  /
--         C
---------------------------------------------------------------

---------------------------------------------------------------------------
--  CELL FUNCTIONS
--  functions used in all cell streams 1..3
--  readCell c >>=   

replaceDot = map (\c -> if c=='.' then ' '; else c)
replaceEmpty = map (\c -> if c==' ' then ','; else c)

readCell c = [replaceDot c] >>= replaceEmpty
-- zip by deleting all double occurcances A sigmatic or B
-- or even all others A not and not B
-- *Experiment3> nub((readCell (unwords (take 3 (iterate evalToWrite "cell")))))
--"cel,12" 
zipCellRAW ct = nub((readCell (unwords (take 3 (iterate evalToCounter ct)))))
zipCell = zipCellRAW "cell"

buildCell r = "cell" ++ (tk r (map show [1..5]))
-- ################################################################################################ to be plugged into trigger Point !!!!!!!!!!!!!! 16.9.2020
--    when going back
--      B    <---    C
cellType ct =  fst(charFilterInt(evalToWrite ct )) 
cellInt ct =   snd(charFilterInt(evalToWrite ct )) 

-- take length found in string build String 'ct' with variable length    
-- turn ct's into [String] 
--
-- if last digtit Int then take n many iterations without Int
-- e.g *Experiment3> iterCT "cell5" 
--     ["cell5","cell","cell","cell","cell"]
--
-- will take max of 8 iterations
--     *Experiment3> iterCT "cell55"
--     ["cell55"]
--     *Experiment3> iterCT "cel3l55"
--     ["cel3l55"]
--     *Experiment3> iterCT "cel2"
--     ["cel2","cel"]
--     *Experiment3> iterCT "cel8"
--     ["cel8","cel","cel","cel","cel","cel","cel","cel"]
iterCT ct = take (snd(charFilterInt ct)) (iterate cellType ct )-- e.g  take 4 (iterate cellType "cell44")
aleph ed = map iterCT ed
--pipeStringToHigherD = if qw3 w e r == aleph ed 
--rekenen
-- e.g*> takeMY 7 3 [1,2,34]
--
-- parse action 
countABCs n wantedList solution foCalc = let stap1RAW countN wantedList solution = takeMY countN wantedList solution
            in let lengther daLength = length (head $ ausw 1 daLength)  
            in let stap1 countN = lengther (stap1RAW countN wantedList solution)
            in let foLen2 = (lengther (stap1RAW (n+1) wantedList solution))
            in let peelCondi = if ((stap1)n+1) == foLen2  then (stap1RAW (n+1) wantedList solution ) 
                               else []
            in if (peelCondi /=[]) == True then (foCalc (n+1))
               else foCalc n
-- generate one random Int based on 'rekenen'  
runCellRAW n wantedList solution = countABCs n wantedList solution (rekenen)
runCellRAWdif n wantedList solution = countABCs n wantedList solution (rekenen2)

--   
--  *Experiment3> takeMY 4 3 [1,2,5,5] --11-1-21
--  [[1,2,5,5],[1,2,5,5],[1,2,5,5]]
--
--
-- 'zufallsBasic1'-> 'longRun' => random number
--                                   ||
--                                   \/
--                               |  rekenen   |
--                                   ||              
--                                   \/
--   takeMY =>      countABCs -> runCellRAW -> runCellRnd -> ogR -> cellStream3   -> poolBorNotB 
--                                                                                -> inActie 
--
--  e.g*> (map (runCell n [1,66,34,99,0]) [1..100])
-------------------------------------------------------------------------------------------------------------                                                                                
runCellRnd want sol n = runCellRAW n want sol
runCellRnddif wantedList solution n =   runCellRAWdif n wantedList solution

runLists wantedist= let a= 100*wantedist
                    in [a..(a+100)] 
-- expandS +1 give 100 new random numbers
organelleFind expanS solution n = (map (runCell n solution) (runLists expanS))
organelleFinddif expanS solution n = (map (runCelldif n solution) (runLists expanS))


-- solutions of B in A and A' in [0]  ------------------------######################################### locate SOLUTIONS A HERE
organelleSearch expanS solution = map (organelleFind expanS solution) [1..(length solution)] -- solution depending of length guess
organelleSearchdif expanS solution = map (organelleFinddif expanS solution) [1..(length solution)] 
oglleLocateSolu expanS solution nIdeal = let findSol sol want = map length (organelleFind expanS sol want)
                  in let findposis want sol foN = foN `elemIndices` (findSol sol want)
                  in let mapUp want = map (findposis want solution) [1..(length solution)]
                  in map mapUp [1..6] -- getposis -- mapUp wanted --getposis --solution wanted nIdeal  

-- orderK : [Int] ; new order has no side effect yet
-- shall be used together with SimiVal , compare orderK with order in solution
--       and with order in guess  simiVal orderK li 
--       and 
--         simiVal orderK guess
--    => find underlaying order ??? 
-- stillyields different output  to function above due to different random number
--       simiVal something to oglleLocateSolu
oglleLocateSoludif expanS solution nIdeal orderK = let findSol sol want = map length (organelleFinddif expanS sol want)
                  in let findposis want sol foN = foN `elemIndices` (findSol sol want)
                  in let mapUp want = map (findposis want solution) [1..(length solution)]
                  in map mapUp orderK -- getposis -- mapUp wanted --getposis --solution wanted nIdeal  
 

---------------------------------------------------------------------------------------------------
-- Three streams in and out of cells -----------------------------------------------------------
-- --------------------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------------------
-- SOLUTIONS B, not b, A stigmatinc ,Not Astigmatic -----------------------------------------------------
-- ---------------------------------------------------------------------------------------
--  1   in ----> the solution -----> B -----> out
--  cellStream1:
--  important depending of how the concept is understood how to appy the baysian logic
--   *Experiment3> map chr $concat$concat$(cellStream1 533333333 li4 6)
--  "\DLE8`'O"   -- one could understand this being                     cell I and cell II
--  also  "\DLE8`'" ++ "0" -> not arithmetic A ++ arithmetic A ??       cell I and cell III
--
-- can give cellI + cell II or cell I + cell III
cellStream1 expanS solu t =  (oglleLocateSolu expanS (map ord (head(ausw t solu))) 421)
--orderK:[Int] ; which order to test Chars in li
cellStream1dif expanS solu orderK t =  (oglleLocateSoludif expanS (map ord (head(ausw t solu))) 421 orderK)

poolB foli4 a = map ord (ausw a (concat foli4 ))  -- add solution here!!!

--  both create random number between 48 -128 
--e.g*> theAChars
-- both always are notB, if [[],[Int]] == true => must contain an empty list to be notB
--    [String] -> Int -> 
--    foli:[String] , a population
--    a: Int        , char number which Int of population 
--    n: Int        , random-number run number Int
poolBorNotB foli4 a n =  (ausw a (cellStream3 foli4 n))  -- adds guess  -> maybe solution -- poolnotB
poolBorNotBdif foli4 a n = (ausw a (cellStream3dif foli4 n))  --                                                            

allLi4 foli4 expanS = map (cellStream1 expanS foli4) [1..6] -- length of a [pv] == 6
allLi4dif foli4 orderK expanS = map (cellStream1dif expanS foli4 orderK ) [1..6] -- length of a [pv] == 6

goldTray foli4 expanS s = map (poolB foli4) (concat$concat$concat$ausw s (allLi4 foli4 expanS))
goldTraydif foli4 orderK expanS s = map (poolB foli4) (concat$concat$concat$ausw s (allLi4dif foli4 orderK expanS))
poolBandNotB foli4 expanS=  concat$ concat(map (goldTray foli4 expanS) [1..(length foli4)])
poolBandNotBdif foli4 orderK expanS=  concat$ concat(map (goldTraydif foli4 orderK expanS) [1..(length foli4)])

------------------------------------------------------------------------- ####################################### computationally expensive
-- all find solutions but are computationally expensive--------------------------------------------------
tbeMapped foli4 t = map chr (poolBandNotB foli4 t)
tbeMappeddif foli4 orderK t = map chr (poolBandNotBdif foli4 orderK t)

--  2      ----> the Zero space ---> [[0]] -----> out
--               indicate the position of all solutions 
--               OF THIS B in A
--               
-- BewAre not functiona !!!not used 
bAndNotB foli4 n = filter(/='0') (head (ausw n (nub$ map (tbeMapped foli4) [1..100])))
-- carefully explore via orderK :: [Int]
-- the order matters, the digits do and the length -> keep it short max length  n < ca. 10
-- all Int in orderK <=6
-- e.g*> (bAndNotBdif li4 [1,2,6] 8)  -- really export elements of B|A and notB|A
-- "6=6=6=6=6="                          test if works: there is a population 
--  there is a handfull of other      in which all four CELLs together are the most ordered state
--  functions as descriped that       breaking into new 4 cells where P (A|B) or cell I is the right order 
--  may work as well, which?              and cellII is good in length missing order and/or letters
--                                           cell III is right order missing max 1/6 of letters 
-- baas                                           cell IV  the ordered state of ptc9     
bAndNotBdif foli4 orderK n = filter(/='0') (head (ausw n (nub$ map (tbeMappeddif foli4 orderK) [1..100])))


likelyhood foli4 n = let step1 =  ((bAndNotB foli4 n))
                     in let step2 = nub$reverse$sort step1
                     in if (length step2) == 1 then take 1 step1 
                        else show ( cellInt (nub$reverse$(sort (bAndNotB foli4 n)))-1)

 
-- which String n of a [String] (As) does resemble 'bAndNotB' most ?
--
expressAsinA liSolution foli4 pi n = nub$concat$(cellStream3EXT liSolution foli4 pi ((bAndNotB foli4 n)))
-- which String n of a [String] (As) does resemble the solution most ?
-- *Experiment3> nub$concat$(cellStream3EXT pi (rek2 1))
-- *> [68.0648769574944,67.89709172259508,68.12080536912752,70.80536912751678]
-- => 6 is part of the solution of (head li4)
-- needs v variables in solution to work. eg "e2" works
--  "2" doesnt
expressAinA liSolution foli4 pi n = nub$concat$(cellStream3EXT liSolution foli4 pi (likelyhood foli4 n) )
----------------------------------------------------------------------------------------------------------------
--NULL SPACE : cellStream2
-- via organelleFind , organelleSearch , oglleLocate
-- runCell -> one of above -> solutions B in A ->  cellStream1
-- to be maped with n
--  runCellRAW  -> runCell
--  runCellRAW2 -> runCelldif 
runCell wantedList solution n = (head (ausw wantedList solution)) `elemIndices` [runCellRAW n wantedList solution]
runCelldif wantedList solution n = (head (ausw wantedList solution)) `elemIndices` [runCellRAWdif n wantedList solution]


-- expanS: Int which pv function to see 1..max (sqrt [(Int range)^2] ) -- must be N > -1 	
-- (Int range -9223372036854775808..9223372036854775807)
-- the null-space is used in 'runCelllRAW','runCell' at the moment.
--  runCell -> organelleFind 
cellStream2 expanS solution t = organelleSearch expanS (map ord (head(ausw t solution)))
---------------------------------------------------------------------------------
--RANDOM LINE : cellStream3 
--  unrestricted search with  
ogR solu n = (runCellRnd 1 solu n)
ogR2 solu n = (runCellRnddif 1 solu n) -- same as above with different random number run
-- every n generates a new list +100
cellStream3 solu n= map (ogR solu) (runLists n)
cellStream3dif solu n= map (ogR2 solu) (runLists n)

{- connect to EXTERNAL via triggerWord' last state domain must be changed .........................  -}   
-- pi: Punkt , if intern switches intern 
--             else extern
--e.g> 
--external cell run   ################################################## 11-1-21 cell reason
---------------------
--pi: Punkt "extern" ...; guess String e.g "ddd"
--    ||                ||
--    \/                \/
--  kArmWORK       triggerWord   -> runEXP3  -> cellStream3EXT   -> expressAinA
--                                                               -> expressAsinA
--                                           -> simVALS
--                                           -> runEXP3Char
--                                           -> experiment3RAW11
--                                           -> experiment3RAW22  -> buildPrior                      no use yet
--                                                                -> theB' -> -- li :: [[String]]
--                                                                               li3 :: [String]     no use yet
--                                                                -> theBQuirky                      no use yet
--                                           -> experiment3RAW23 -> commmB  -> foStrCd ->  qw wo ist r = (foStrCd wo ist r)

--                                                               -> commmB2
--                                                                 ################################################### 11-1-21 reason cell run export via Punt do cellStream3EXT 
cellStream3EXT liSolution foli4 pi guess = nub $ concat$ (runEXP3 foli4 liSolution pi guess)  -- => simvals

--  vs       
--  
-- internal cell run
-- --e.g> defSearchRAW "AAABB" "AAABBAAABAB" "AAABAB" "AAA" "AAABBBAA" "BBBAA" 1 1 1 1 1 li liSolution
--
--pipebone: variable for runKBASE, n-(length target)-many, of a [bonelist]
-- defSearchRAW -> defSearch -> runKAXIOM -> runKBASE -> experiment3RAW -> experiment3RAW2 

-- runCellRAW
--simVALS
--------
-- to be mapped with Iterations and or li          --change between diferent projections
-- depend on fixed variable -- stir which domain to compare to what ....
-- wtih length 1 maps agains
-- does kArmWORK run that simiVals 
-- note ! change order if in need to map li
-- set to map with liSolution
-- e.g *> simVALS pi "WE" li "xyz=11"
--     *>[[[20.490367775831874,26.761023580752717,20.175131348511385,20.450764570705708]],
simVALS foPunkt ghAdd foli liSolution = runEXP3 foPunkt liSolution foli ghAdd

 
--theASolveBonelist -- solve relation to whole of li4 ( a bonelist) 
-- C' = defined by at least one SimiVal boundry e.g below 0.4         
-- C' = overlaying hex A and C

-- types of A's
-- -------------
--   to solve from different A's 
--   one way to solve from is  reflexive qw3
--       qw3 liSolution l1 l2 =>  where is l1 in l2
--                 or  where is l2 in l1 
--    where is the prior in the latter ?
-- e.g *Experiment3> qw3 "liSolution" "wer" "rw"
-- [[1],[],[0]] ??
-- *Experiment3> qw3 "xyz=1" "wer" "wer"
-- [[0],[1],[2]]
--
--  secondly just see occurance 
--
-----------------------------------------------------

--------------------------------------------------------------------------
-- RANDOM number generators

longRun variance t = head $ zufallsBasic1 1 variance t
longRun2 variance to n = last $ zufallsBasic1 n variance to
-- variance 'from' Int 'to' Int n-many steps
rekenenRAW to from n =  ( ( longRun to n)+from)
rekenenRAW2 to from n =  longRun2 from to n 
 
rekenen r = (rekenenRAW 80 48 r) --comparEB (head(ausw r ([li]))) (map show [1..( ( longRun 80 r)+48)])
rekenen2 r = (rekenenRAW2 80 48 r) 
----------------------------------------------------

-------------------------------------------------------------------------
-- calculate Ints for String -> [Char]-> [Ord]-> add or substract
-- aBgivenA li =  ((buildPrior li))
allB = readAnyTh C.progLiT
-----------------------------------------------------------


-----------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------
-- 12-9-2020
-- function below is based an 'runKBASE' above
-- calling fst (kArmWORK...)
-- => compare internal SimiVals of aLi
-- or calling snd (kArmWORK ...) 
-- => compare every String of aLi 
--    to an external String ghAdd 
-- experiment 3 compare two bonlists with each other
-- let one list be the solution domain 
-- let another be the snytactic domain
--
-- e.g> runEXP3 li liSolution pi ghAdd
-- connected to ht if ht == 3 then run Experiment3  
-- domain a) determine how to apply LP to IDF
--           y' = xz(IDF) + - xz (LP)
--           I) let xz' = (maximum maXxS(IDF )) - (maximum maXyS (LP))
--rabbithole: pi
-- runEXP with fst -> [Fractional] a pointer , with pre-selection criteria
--        with snd -> [[Fractional]] needs pointer with selection
--  try: pi can handle the selection
checkFoGo g = if (length g) == 0 then ""
            else "MOTHER MODE: on"  
-- foAli: String the name of this Punkt
baas foA foAli  =  (unwords(checkflow [] [(Punkt foAli foA foA foA foA foA)] ))
-- start domain change
-- g: a Punkt name ...
-- t: String , to make a command
-- iOE: to be plugged into runEXP3 
--      do the action decide to 
--   compare li to itself, internal
--   or li to an String ,external
-----------------------------------------------

---------------------------------------------------------------
--access intern or extern via Punkt ..."intern" ...
--
startDchange g t iOE = let piToBinary = baas Nothing t  
                       in if g == t then fst iOE --fst internal or external
                          else snd iOE

triggerWord g domain = triggerWordRAW g domain "cell"
triggerWordRAW g domain focell = if domain == "intern" then show(stC1 g )
                       else if domain == domain then (stC2 g domain) -- "cell" 
                       else show(stC1 g)
     where
        stC1 g = (startDchange g "intern" (1,2));
   -- get an in from idea name ("cell" ++ (show token))
   -- this prep step depends on another A'' set 
   -- pv1: a b c d e f
   -- pv2: g h i j k l
   -- pv3: m n o p q r
   -- pv4: s t u v w x________________  
   --                 |missing 'y z' |
   --                 | and   '='    |
   --                 |              |
   --                 |______________|
   -- y :: a..z \\ (z-1)
        fromCell = map chr [48..57];
        toHaal toInt =  charFilterInt toInt;
        condI numP   = if (fst (toHaal numP ))== focell && (snd (toHaal numP)) >= 0 then (toHaal numP)
                       else if (numP) == (focell++"3") then (numP,(snd(charFilterInt numP)))
                       else ((unlines(checkflow [mother] [(maybePu "CELL")])) , 0 ) ;
        stC2 g numP = startDchange g  (fst(condI numP)) (show(snd(condI numP)),(fst(condI numP)));

--conVerge
------------------------------------------------------

------------------------------------------------------
-- SIMIVAL RUNS for  'Quirky example'
-- manifest the given li function with and without 'appropiate mathematical syntax'
-- give a simiVal list to compare both states
-- liSolution: a real Prior/Bias?!? 
-- e.g -["0*x + y + 0*z = 3","0*x + y + 0*z = 33*x + 0 + 0*z = 6","3*x + 0 + 0*z = 6","0*x + 0*y + z = 2","0*x + 0*y + z = 2x + y + z = 11","x + y + z = 11"]
-- so far liSolution is merely a symbolic type it has no use other to be simivalued to other liSolutions 
-- 
-- example for Punkt as selector will try filter : PROTRACTOR at how many degrees is the blind shut
--           test define a function that computes  
--           with Punkt "intern" there is NO ghAdd influence ONLY li 
--           with "cell"  YES ghAdd influence AND li plays an influence   ############### whole function is ################################## SAME AS   cellStream3EXT- 13-1-21        
runEXP3 li liSolution pi ghAdd = do                                                          --  ########################## BOTH are an INDEX matrices (or simply values)
       -- domain2 syntax                                                                                   ##############   I: EXPORT TO MAIN
       --                                                                                                                  II:  PUNKT JUNCTION POINT 
       let iDF = li --["AaEe0","AaEeI","i0000","OoUu0","OoU0Y","y0000"]                                                    III: JOIN with a. 2dplot, 3dplot, 2/3dplot, ptcChart.html
       -- domain1 solution                                                                                                      in Main
       --                                                                                                                  IV: EXPORT BAYSIAN type to MAIN 
       --                                                                                                                      make nice exlpainatory interactive 
       --                                                                                                                   V: 1-1-21 issue 'story line' join the lose ends
       --                                                                                                                      with 'Colored_2_3_5_Counter20.hs'=C20 see overview
       --                                                                                                                         found most versataile ptc6 ptc7? , ptc9
       --                                                                                                                         see solutions of 'Quirky-Example' this lP function
       --                                                                                                                         in C20 ; mix wit first 'C.vb' example
       --                                                                                                                         => writes 'src/2dpgfunctions.wxm" 
       --                                                                                                                            watch an ordered state and the ptc data 
       --                                                                                                                            and the purple solution function which main aim
       --                                                                                                                            is to turn a very ordered state into -> 
       --                                                                                                                            -> [[Char]] -> [[Int]] -> ruEXP3 =
       --                                                                                                                            => relation -> fourierFS to mq-functions
       --                                                                                                                  VI: THE NEW CELL TYPE as basis ptc9 is involved 
       --                                                                                                                      with a rating of guesses AFTER PUNKT JUNCTION POINT
       --                                                                                                                      in Main.
       --
       let lP = liSolution --["0*x + y + 0*z = 3","0*x + y + 0*z = 33*x + 0 + 0*z = 6","3*x + 0 + 0*z = 6","0*x + 0*y + z = 2","0*x + 0*y + z = 2x + y + z = 11","x + y + z = 11"]
       let choosDomain ff =  if ff == 1 then iDF 
                             else lP 
       let chD ff  = choosDomain ff 
       allforIV <- forM [1] (\four4 -> do
            -- first loop is 'IDF' second one is 'LP'
            let de e r = head $ ausw e r 
            let foaLi e =  de e (chD four4)
            let aLi = [(foaLi 1),(foaLi 3),(foaLi 4),(foaLi 6)] 
            --let prepComp g = startDchange g (1,2)
                -- compare internaly
            --let plugChoose = triggerWord pi  
            let fg = let allFor = (kArmWORK 2 (chD 2) aLi 1 pi 1 1 [] ghAdd) --(runKAXIOM offOn target plot addGh ghAdd n (theDs four4 d) (get1) (get2) (get3) (get4) 3 (theDs four4 d) (four4) [(read(show four4))] ((tk four4 subroutineList)))
                     in let toHaal =  charFilterInt ghAdd;
                     in  if (triggerWord (filter (/=' ') (head(checkflow [] [pi]))) "intern" ) == "1" then (fst allFor)
                         else if (triggerWord (filter (/=' ') (head(checkflow [] [pi]))) "cell" )  == "1" then (map read [(triggerWord "cell" "cell3")]) -- snd (ghAdd,(snd(charFilterInt ghAdd)))

                         else (head (snd allFor))

            fg     
            return (fg))

       -- apply an algorythm
       let beforeAlgo  = allforIV -- if (triggerWord (unlines(checkflow [] [pi]))) == 1 then (map fst allforIV)
                       --  else ((map snd allforIV))
       --let beforewithGH = head $last $ head $ allforIV
       (return (beforeAlgo))
------------------------------------------------------------
-- needs not "intern" to genearate new Char guesses by
-- adding chars to ghAdd
-- pi: Punkt , ghAdd: String ; li: the source [String ]
--
runEXP3Char pi liSolution ghAdd li = 
       -- domain2 syntax
       let asun = (runEXP3 li liSolution pi ghAdd)
       in let toChar = map round $ head$ (head asun)
       in toChar

-- ["AaEe0","AaEeI","i0000","OoUu0","OoU0Y","y0000"]
       -- domain1 solution
       --let choosDomain ff =  if ff == 1 then iDF 
      --                       else lP 
      -- let chD ff  = choosDomain ff 
     --  allforIV <- forM [1,2] (\four4 -> do
            -- first loop is 'IDF' second one is 'LP'
       --     let fg = chD four4 --runEXP3 (chD four4) pi ghAdd --kArmWORK 2 (pv1to6) (chD four4) 1 pi 1 1 [] ghAdd --(runKAXIOM offOn target plot addGh ghAdd n (theDs four4 d) (get1) (get2) (get3) (get4) 3 (theDs four4 d) (four4) [(read(show four4))] ((tk four4 subroutineList)))
--obtc
         --   return (fg))
       --return (iDF)
       --return (lP)
       -- apply an agorythm
       --let beforeAlgo  = show allforIV
       --let beforewithGH = head $last $ head $ allforIV
       --(return (beforeAlgo))

------------------------------------------------------------

-- 4-8-2020  ***************************************************************************updated write ptc buttons
-- e.g let myTest at = runKAXIOM 1 [1,2] 2 2 at 2 (li3) 1 2 3 4 3 (head(ausw 1 li3)) "xyz=11"
-- toca : Int ; give token to change length of pg  functions points n 
--               write toHtml set to write index2.html via 'defSearch', under development
runKAXIOM offOn target plot addGh ghAdd n d get1 get2 get3 get4 ht ulu bog toca subroutine liSolution=  do
   let ghd t de = ( (ausw t de))
   let leng= if length d >1 then ((length d) - 1)
             else 1
   let robe = ulu
   let hold  = do
          dooer <- forM [1] (\fd -> do
              let beta =  ulu --(foact ulu ) --(map --robe --let foact = if fd == 1 then (drop 0 ) (take stepper (concat d))
                               --     else  (drop (fostepper (fd-1))) (take stepper (concat d))

              let topass =  ulu 
              let pi = Punkt "notM" Nothing Nothing Nothing Nothing Nothing
        --aaS <- forM [1..(length bonelis)] (\bl -> do  --   kArmTest5 act 1 pi 1 1 [] "AAA"
              let whichLine1 = (ausw get1 topass)
              let wL2 = (ausw get2 topass)
              let wL1n2 = (whichLine1++wL2)
              let wL3 = (ausw get3 topass)
              let wL4 = (ausw get4 topass)
              let wL3n4 = (wL3++wL4)
              let vanAllen = defSearch offOn d plot addGh ghAdd whichLine1 wL1n2 wL2 wL3 wL3n4 wL4 n ulu (ulu) ht (evalToWrite("foyourRun"++show bog++".txt")) ht toca subroutine liSolution
              vanAllen
              return([vanAllen]))
          return(dooer)
   hold 
-- tsRAW  

        
-- program variables:
defSearch offOn target plot addGh ghAdd pV1 pV2 pV3 pV4 pV5 pV6 n bonelist pipebone ht forRunHtml htm toca subroutine liSolution = defSearchRAW offOn target plot addGh ghAdd pV1 pV2 pV3 pV4 pV5 pV6 50 50 50 50 n bonelist pipebone ht forRunHtml htm toca subroutine liSolution
--e.g> defSearchRAW "AAABB" "AAABBAAABAB" "AAABAB" "AAA" "AAABBBAA" "BBBAA" 1 1 1 1 1 li
--
--pipebone: variable for runKBASE, n-(length target)-many, of a [bonelist]
defSearchRAW offOn target plot addGh ghAdd pV1 pV2 pV3 pV4 pV5 pV6 ptc0Len ptc3Len ptc3aLen ptc3bLen n bonelist pipebone ht forRunHtml htm toca subroutine liSolution= do

     --  let pi = fopi --Punkt "M" Nothing Nothing Nothing Nothing Nothing -- switch for io at bottom
     --  ausw :: Int -> [a] -> [a]
     --  ausw n k = drop (n-1) (taken n k)                                                      
     --                                                     PV1s  list a  | PV1s list b
       let progVar1 = head(ausw htm pV1) --"ccccc" --"AAABB"  --  "AAABB"       |   "TTTT" 
       let progVar2 = head(ausw htm pV2) --"AAABBAABAB"
       let progVar3 = head (ausw htm pV3) --"A"--"AABAB"
       let progVar4 = head(ausw htm pV4) --"A"--"AAA"
       let progVar5 = head (ausw htm pV5) --"AAABBBAA"
       let progVar6 = head (ausw htm pV6) --"BBBAA"

       let liT = [progVar1,progVar2,progVar3,progVar4,progVar5,progVar6] --["AAABB","AABAB","AAA","BBBAA"]
       let pg1 x = F.fourierMQ6NOPAN123 x--(sin (read((head((formTest [father] (preX x) [show(sin (x))] (words(show(sin 2))))  ))))) -- (((([(formTest [mother] 1 [show(F.fourierMQ6NOPAN123 (realToFrac (show x)))] (words(show(sin (x)))))]  )))) -- head(ausw 1 [(F.fourierMQ6NOPAN123 x)]) 
       let pg2 x = (F.fourierMQ5NOPAN123 x)
       let pg3 x = (F.fourierMQ4NOPAN123 x)
       let pg4 x = (F.fourierMQ4TRACE (x)) -- cos x -- 14-8-2020
       let pg11 x = show x --show(F.fourierMQ6NOPAN123 x)
       let pg22 x = show x --show(F.fourierMQ5NOPAN123 x)
       let pg33 x = show x --show(F.fourierMQ4NOPAN123 x)
       let pg44 x = show x --show(F.fourierMQ4TRACE x)
       let pgFun x = x
   
       let wohlGeorNet02 compar fopu punktList = --let inpu = uniquEClass01 compar 1
                   let b=  (pg1 punktList) 
                   in let c = (pg2 punktList)
                   in let d = (pg3 punktList)
                   in let e = (pg4 punktList)
                   in let loop = head(F.chooseMQ 5 [punktList])
 
                 --  in let g fopu= (getInts compar fopu)
                 --  r: e.g 100
                 --  in let forythm = concat (zufallsBasic1 1 10) fopu
                   in let rythm  = head(zufallsBasic1 1 5 fopu)
                   in let h  = (F.chooseMQ (rythm) [b,c,d,e,loop])
                   in h  -- h punk


       let uniquEClassReallyRAW compar punktList befehle = 
                   let sta1 d = map ord d  
                  -- in let sta1New = map realToFrac (sta1 (show(F.fourierMQ6NOPAN2 fstOsnd )))
                   in let sta2New = map realToFrac (sta1 (show(head(ausw 1 befehle ) punktList )))
                   in let sta3New = map realToFrac (sta1 (show(head(ausw 2 befehle ) punktList )))
                   in let sta4New = map realToFrac (sta1 (show(head(ausw 3 befehle ) punktList )))
                   in let sta5New = map realToFrac (sta1 (show(head(ausw 4 befehle )punktList )))
                   in let staLoop = map realToFrac (sta1 (show(wohlGeorNet02 compar 1 punktList )))

                   in let staCompare = map realToFrac (sta1 ((compar))) -- *****************************SELECT EACH LINE OF GUESS
                   in [(similaritYvalue staCompare sta2New),(similaritYvalue staCompare sta3New),(similaritYvalue staCompare sta4New),(similaritYvalue staCompare sta5New),(similaritYvalue staCompare staLoop)]


       let uniquEClassRAW compar pk = uniquEClassReallyRAW compar pk [(F.fourierMQ6NOPAN123),(F.fourierMQ5NOPAN123),(F.fourierMQ4NOPAN123),(F.fourierMQ4TRACE)] 
--similaritYValue "wrist" (
       let uniquEClass01 compar punktList = uniquEClassRAW compar punktList 
--
       let toleranceNet compar punktList = let inpu = uniquEClass01 compar punktList 
                   in let b=  head(F.chooseMQ 1 inpu) 
                   in let c = head(F.chooseMQ 2 inpu)
                   in let d = head(F.chooseMQ 3 inpu)
                   in let e = head(F.chooseMQ 4 inpu)
                   in let loop = head(F.chooseMQ 5 inpu)
                   in let foCombos func  = map realToFrac (map ord (show(func)))
                   in let way1 =[(similaritYvalue (foCombos b) (foCombos c)),(similaritYvalue (foCombos b) (foCombos d)),(similaritYvalue (foCombos b) (foCombos e)),(similaritYvalue (foCombos c) (foCombos d )),(similaritYvalue (foCombos c) (foCombos e))]
                   in let way2 =[(similaritYvalue (foCombos e) (foCombos d)),(similaritYvalue (foCombos e) (foCombos c)),(similaritYvalue (foCombos e) (foCombos b)),(similaritYvalue (foCombos e) (foCombos e )),(similaritYvalue (foCombos e) (foCombos loop))]
                   in way2

  --if b>c && c>d && d>e then [punktList]
                      --else []
       let wohlGeorNeRAW compar punktList = let inpu = uniquEClass01 compar punktList
                   in let f = ((minimum inpu) `elemIndices` (inpu)) 
                   in f --F.chooseMQ f [b,c,d,e]--if b>c && c>d && d>e then [punktList]
                     -- else []
                     --
       let toleranceNew compar punktList=
                let inpu = toleranceNet compar punktList
                in (minimum inpu) `elemIndices` (inpu) 

       let gettolerance compar punktList = map (wohlGeorNeRAW compar) [1..punktList]
       let wohlGeorNet0 compar punktList = let inpu = uniquEClass01 compar punktList
                   in let b=  head(F.chooseMQ 1 inpu) 
                   in let c = head(F.chooseMQ 2 inpu)
                   in let d = head(F.chooseMQ 3 inpu)
                   in let e = head(F.chooseMQ 4 inpu)
                   in let loop = head(F.chooseMQ 5 inpu)
               --    in let d = head(F.chooseMQ 5 inpu)
                   in let f = ((minimum inpu) `elemIndices` (inpu)) 
                   in let g fopu= concat(gettolerance compar fopu)
                   in let h fopu = (F.chooseMQ ((head (g fopu))+1) [b,c,d,e,loop])
                   in h punktList--if b>c && c>d && d>e then [punktList]
                     -- else []
       let wohlGeorNet01 compar punktList = let inpu = toleranceNet compar punktList
                   in let b=  head(F.chooseMQ 1 inpu) 
                   in let c = head(F.chooseMQ 2 inpu)
                   in let d = head(F.chooseMQ 3 inpu)
                   in let e = head(F.chooseMQ 4 inpu)
                   in let loop = head(F.chooseMQ 5 inpu)
               --    in let d = head(F.chooseMQ 5 inpu)
                   in let f = ((minimum inpu) `elemIndices` (inpu)) 
                   in let g fopu= concat(gettolerance compar fopu)
                   in let h fopu = (F.chooseMQ ((head (g fopu))+1) [b,c,d,e,loop])
                   in h punktList--if b>c && c>d && d>e then [punktList]
                     -- else []

       let wohlGeorNet03 compar fopu punktList = --let inpu = uniquEClass01 compar 1
                   let b=  (pg1 punktList) 
                   in let c = (pg2 punktList)
                   in let d = (pg3 punktList)
                   in let e = (pg4 punktList)
                   in let loop = c
 
                 --  in let g fopu= (getInts compar fopu)
                 --  r: e.g 100
                 --  in let forythm = concat (zufallsBasic1 1 10) fopu
                   in let rythm  = head(zufallsBasic1 1 5 fopu)
                   in let h  = (F.chooseMQ (rythm) [b,c,d,e,loop])
                   in h --punk



--type3 compar punktList = let fg = (punktList*1.0) 
  --                  in let inpu = (wohlGeor3 compar fg) 
    --                in let tak = punktList
      --              in drop (tak-1) (take tak inpu)

       let wohlGeorNet compar punktList = let inpu = uniquEClass01 compar punktList
                   in let b=  head(F.chooseMQ 1 inpu) 
                   in let c = head(F.chooseMQ 2 inpu)
                   in let d = head(F.chooseMQ 3 inpu)
                   in let e = head(F.chooseMQ 4 inpu)
               --    in let d = head(F.chooseMQ 5 inpu)
                   in if b>c && c>d && d>e then [punktList]
                      else []
       let findOrdung compar until= map (wohlGeorNet compar) [1..until] 
-- I) rate bone with 1,2,3,4,5
       let df1 compar pub = map (uniquEClassRAW compar) [1..pub] -- [pg1,pg2,pg2,pg4,pg5] 
------------------------------------------------------
-- II) rate within df1: 4. with 1,2,3,5  
       let df2 compar pub = map (toleranceNet compar) [1..pub]
------------------------------------------------------
       let fotoGeor3 compar punktList pg = (wohlGeorNet03 compar pg) punktList
  
-- serve 5 possible random values that are OUTPUT of each Line called
-- the output is used to find solutions.
       let toGeor3 compar punktList = (concat (map (fotoGeor3 compar punktList) [1..(length liT)]))

-- III) get a random value of 1,2,3,4
       let df3 compar pub = map (toGeor3 compar) [1..pub]
   ------------------------------------------------------


------------------------------------------------------
       let wohlGeor1 compar punktList = map (wohlGeorNet0 compar) [1..punktList]

       let wohlGeor2 compar punktList = map (wohlGeorNet01 compar) [1..punktList]

       let wohlGeor3 compar punktList = let inpu = (toGeor3 compar punktList) -- wohlGeor3
                       in let a = head$F.chooseMQ 1 inpu
                       in let b = head$F.chooseMQ 2 inpu
                       in let c = head$F.chooseMQ 3 inpu
                       in let d = head$F.chooseMQ 4 inpu
                       in let e = head$F.chooseMQ 5 inpu
                       in let foCombos func  = map realToFrac (map ord (show(func)))
                       in let way1 =[(similaritYvalue (foCombos a) (foCombos b)),(similaritYvalue (foCombos a) (foCombos c)),(similaritYvalue (foCombos a) (foCombos d)),(similaritYvalue (foCombos a) (foCombos e )),(similaritYvalue (foCombos e) (foCombos d)),(similaritYvalue (foCombos e) (foCombos c))]
                       in let way2 =[(similaritYvalue (foCombos e) (foCombos d)),(similaritYvalue (foCombos e) (foCombos c)),(similaritYvalue (foCombos e) (foCombos b)),(similaritYvalue (foCombos e) (foCombos a))]
                       in way1


       let theGeors o = let auswa = head (F.chooseMQ o [(wohlGeor1),(wohlGeor2)])
                    in auswa

       let theVarias p = let auswa = head (F.chooseMQ p [(progVar1 ),(progVar2 ),(progVar3 ),(progVar4 ),(progVar5 ),(progVar6 )])
                     in auswa

--o:Int choose wohlGeors 
--t:Int choose atom in line
--t:Int choose line
       let atrix0R foGeo o t foprog p n  = (F.chooseMQ t (concat ((foGeo o) (foprog p) n)))
       let atrix0R2 t foprog p n  = (F.chooseMQ t ((wohlGeor3 (foprog p) n)))

--------------------------------------------------
-- wohlGeor1 <--> wohlGeor2 <-> wohGeor1
--   I              I+II         III
       let atrix0 t n = atrix0R theGeors 1 t theVarias 1 n
       let atrix1 t n = atrix0R theGeors 2 t theVarias 2 n
       let atrix2 t n = atrix0R theGeors 1 t theVarias 3 n

       let atrix3 t m = (F.chooseMQ t (wohlGeor3 (progVar1 ) m))
--    IV   <->      IV++VI  <->  VI
       let atrix4 t n = atrix0R theGeors 1 t theVarias 4 n
       let atrix5 t n = atrix0R theGeors 2 t theVarias 5 n
       let atrix6 t n = atrix0R theGeors 1 t theVarias 6 n
---------------------------------------------------
--------------------------------------------------
-- wohlGeor1 <--> wohlGeor1 <-> wohGeor1
--   I              I+II         III
--   question for project: isomorphsims ?!:) over various pg functions eqivalent to 
       let atrix0a t n = atrix0R theGeors 1 t theVarias 1 n
       let atrix1a t n = atrix0R theGeors 1 t theVarias 2 n
       let atrix2a t n = atrix0R theGeors 1 t theVarias 3 n

       let atrix3a t m = (F.chooseMQ t (wohlGeor3 (progVar1 ) m))
--    IV   <->      IV++VI  <->  VI
       let atrix4a t n = atrix0R theGeors 1 t theVarias 4 n
       let atrix5a t n = atrix0R theGeors 1 t theVarias 5 n
       let atrix6a t n = atrix0R theGeors 1 t theVarias 6 n
---------------------------------------------------
--------------------------------------------------
-- wohlGeor2 <--> wohlGeor2 <-> wohGeor2
--   I              I+II         III
       let atrix0b t n = atrix0R theGeors 2 t theVarias 1 n
       let atrix1b t n = atrix0R theGeors 2 t theVarias 2 n
       let atrix2b t n = atrix0R theGeors 2 t theVarias 3 n

       let atrix3b t m = (F.chooseMQ t (wohlGeor3 (progVar1 ) m))
--    IV   <->      IV++VI   <->  VI
       let atrix4b t n = atrix0R theGeors 2 t theVarias 4 n
       let atrix5b t n = atrix0R theGeors 2 t theVarias 5 n
       let atrix6b t n = atrix0R theGeors 2 t theVarias 6 n
---------------------------------------------------

       let amatrix n m = concat [(atrix0 n m),(atrix1 n m),(atrix2 n m)]
       let amatrixDif n m = concat [(atrix4 n m),(atrix5 n m),(atrix6 n m)]
       let amatrixa n m = concat [(atrix0a n m),(atrix1a n m),(atrix2a n m)]
       let amatrixDifa n m = concat [(atrix4a n m),(atrix5a n m),(atrix6a n m)]
       let amatrixb n m = concat [(atrix0b n m),(atrix1b n m),(atrix2b n m)]
       let amatrixDifb n m = concat [(atrix4b n m),(atrix5b n m),(atrix6b n m)]

-------------------------------------------------------------
--COMPUTE UNIQUE POINTCLOUD WITH progVar3 ; added 25-4-20 (first approach)

-- atrix constant based on MQ function does not change with string
       let atrixCo t m = (F.chooseMQ t (wohlGeor3 (progVar3 ) m))
-- Makes own metric
-- one value selected each line is sorted
-- with -order1 which is ascending
-- the othe two rows wil be sorted like order1
   
       let amatrix2 n m = concat [(atrixCo 1 m),(atrixCo 2 m),(atrixCo 3 m)]
       let foorder k t  = F.chooseMQ k (sort (amatrix2 t t))

-- where is value k in each line t
       let findes k t = let inpu = foorder k t 
                in let inpu2 = (head inpu) `elemIndices` (amatrix2 t t)
                in inpu2
-- sort them
       let changs t k = F.chooseMQ ((head(findes k t))+1) (amatrix2 k t)
   
       let theTrix w  = let auswa = head (F.chooseMQ w [(amatrix ),(amatrixDif ),(amatrixa ),(amatrixDifa ),(amatrixb ),(amatrixDifb )])
                    in auswa
       let changs2 w t k = F.chooseMQ ((head(findes k t))+1) ((theTrix w) k t)

-- WHAT HAPPENED here:
-- Experiment every atom of bonelist is compared to a 'formation'
--  pg1 -> MQ6;  pg2 -> MQ5;  pg3 -> MQ4 ; pg4 -> TRACE
--
--   input x -> MQ6 x              -> MQ6  |
--           -> MQ5 x              -> MQ5  | --\ val 1=(x1)
--           -> MQ4 x              -> MQ4  | --/  [ MQ6(x1),MQ5(x1),MQ4(x1),TRACE(x1)]
--           -> MQ4 (MQ5(MQ6 x))   -> TRACE|
       let chgLine t = concat(map (changs t) [1..3]) 
       let chgLine2 w t = let a w t k=  (head(changs2 w t k ))
                      in map (a  w t) [1..3]

       let pointCloud04 n = ( map chgLine [1..n])
       let pointCloud05 n = (map (chgLine2  1) [3..n])

       let pointCloud06 n =   (map (chgLine2  2) [3..(realToFrac n)])
       let pointCloud07 n =  (map (chgLine2  3) [3..(realToFrac n)])
       let pointCloud08 n = (map (chgLine2  4) [3..n])
       let pointCloud09 n = (map (chgLine2  6) [3..n])

       let workptc9 ptc n = let be f = nub (ptc n) --([(ste1 f),(ste2 f),(ste3 f)])
                        in be n
              where
                ste1 f= head (ptc f);
                ste2 f = head(ausw 2 (ptc f));
                ste3 f = last (ptc f);  
  --order1 =  if 
       let pointCloud01 n  = let toMap e = last((map (amatrix e) [1..ptc0Len]))
                  in map toMap [1..n]
-- based on wohlGeor3 -> constant-> any string -> aways same points depending on pg functions
       let pointCloud02 n = drop (n-1) (map (amatrix2 n) [1..(realToFrac n)])

       let pointCloud03 n  = let toMap e = last((map ((theTrix 2) e) [1..ptc3Len]))
                  in map toMap [1..n]
       let pointCloud03a n  = let toMap e = last((map ((theTrix 4) e) [1..ptc3aLen]))
                  in map toMap [1..n]
       let pointCloud03b n = let toMap e = last((map ((theTrix 6) e) [1..ptc3bLen]))
                  in map toMap [1..n]

       let foptc e = take e [1,4..]  --ptc1
       let ptc0  e = pointCloud01  e
-- constant based on Mqs
       let ptc2 e = ( pointCloud02 e)

       let ptc3RAW e = pointCloud03 e
-- ptc3 : variable function 
-- decide how many points to plot via function variable 'e'
       let ptc3 e = (ptc3RAW e)


       let ptc3a e = pointCloud03a e
       let ptc3b e = pointCloud03b e

       let ptc4 e = pointCloud04 e
       let ptc5  n = pointCloud05 n 
       let ptc6 n = pointCloud06 n 
       let ptc7  n = pointCloud07 n
       let ptc8  n = pointCloud08 n 
       let ptc9  n = pointCloud09 n  
------------------------------------------------------
-- MINIMA I,II,III
       let fodf1 compar pub = let inpu2 =   map (wohlGeorNeRAW compar) [1..pub] -- same as get tolerance
                   in let toAds t = take t [1,1..]
                   in let toPubs = toAds (length [1..pub])
                   in zipWith (+) (toPubs) (concat inpu2)

       let fodf2 compar pub = let inpu2 = map (toleranceNew compar) [1..pub]
                   in let toAds t = take t [1,1..]
                   in let toPubs = toAds (length [1..pub])
                   in zipWith (+) (toPubs) (concat inpu2)

       let fodf3 compar pub = let inpu fopu = wohlGeor3 compar fopu
                   in let inpu2 fopu = (minimum(inpu fopu)) `elemIndices` (inpu fopu)
                   in let toAds t = take t [1,1..]
                   in let toPubs = [1..pub]
                   in zipWith (+) (toAds (length toPubs)) (concat(map inpu2 toPubs))

   ---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
--tsRAW is an important junction point. It establishes coloring rule to the ptc buttons 
-- turn a 3d Pointcloud into 3 destinct 2d representations
-- functions exported to main module:
-- tsRAW -- take 100 and nub out (delete all occurances>1) 
-- pp t -- get nubbed lines 
-- daZip -- reduce matix
       let tsRAW pt =  if (toca == [1]) then do [nub( pt 5)] --if (head target) == " 1" then [nub(pt 5)] 
                       else if toca==[2] then do [nub(pt 20)]
                       else do [nub( pt 30)] 
                    --   else if (head(ausw 2 target)) == [' ','2'] then  [nub( pt 15)] 
                      -- else if htm == (3)  then  [nub( pt 25)]  -- set to write in writeHtml via fobase 
                       --else  [nub( pt 137)]

       let forTs r = tsRAW r
       let ts r = (forTs r) 
       let maxa t r =head$last$group$sort$concat$concat$ausw t (forTs r)
 
       let picks t r = concat(concat(ausw t (transpose (ts r))))
       let runM t r = take (length (picks t r)) (repeat (maxa t r))
       let pp t r = concat$concat$ausw t (ts r)
       let daZip t r = zipWith(/) (runM t r) (pp t r)
       let qs t r= ( "1.0") `elemIndices` (map show (daZip t r))
       let rep r = map (\c -> if c==r then "0"; else c)
       let rep2 r = map (\c -> if c==r then "0"; else c)

       let getrep t r = rep "1.0" (map show(daZip t r))
       let getrep2 s t r = rep2 s (map show(daZip t r))
       let source t line = (group((getrep t line)))

       
       let myMonad ptC= do
            let metric r line = ausw r (group(sort(concat(concat(ausw line (ts ptC))))))
            let dats r line = ausw r (group(getrep line ptC))
            let sol r line = length (concat(metric r line))
            pin <- forM [1,2,3] (\pu -> do
                let fosol = length (group(getrep pu ptC)) 
                checklin <- forM [1..(fosol)] (\chk -> do
                       let maak = metric chk pu
                       let retrun = if (map length (maak)) == [2] && (concat maak)/=[1.0,1.0] then [1.0,1.0]
                                    else (concat maak) --[(head (metric 1 line)),head(ausw 2 (metric 2 line)),["0","0"]]
                       return(retrun))
                let binder = ( (concat checklin))
          --putStrLn checklin
                return(checklin))
          
            (pin)      

       let choosM t ptC = (concat(concat(ausw t (myMonad ptC)) ))
       let build2d ptC = nub(concat [fw1,fw2,fw3])
            where 
             frameW t g = transpose [(choosM t ptC),(choosM g ptC)];
             fw1 = frameW 1 2;
             fw2 = frameW 1 3;
             fw3 = frameW 2 3;
       
             -- let pop = (ptc0  5)
              --M.writeWXCloudNODE (pop) (ptc2 5) (ptc3 5) (ptc4 5) (ptc5 5) (ptc6 5)
           --   M.writeWXCloudNODE (ptc3b 1) (ptc3b 2) (ptc3b 3) (ptc3b 4) (ptc3b 5) (ptc3b 6)
           --   M.writeWXCloudNODE (ptc3 2) (ptc3 4) (ptc3 6) (ptc3 8) (ptc3 10) (ptc3 20)  -- nested graph ? 
           --   M.writeWXCloudNODE (ptc2 5) (ptc2 25) (ptc2 50) (ptc4 100) (ptc4 125) (ptc4 150) -- ???
           --   M.writeWXCloudNODE (ptc5 5) (ptc5 25) (ptc5 50) (ptc5 100) (ptc5 125) (ptc5 150) --  a plaine 
           --   M.writeWXCloudNODE (ptc6 5) (ptc6 25) (ptc6 50) (ptc6 100) (ptc6 125) (ptc6 150) -- interesting
            --  M.writeWXCloudNODE (ptc7 5) (ptc7 25) (ptc7 50) (ptc7 100) (ptc7 125) (ptc7 150) -- half 'crown'
             -- M.writeWXCloudNODE (ptc8 5) (ptc8 25) (ptc8 50) (ptc8 100) (ptc8 125) (ptc8 150) -- similar to above
       let plotOn = if plot == 1 then do
                   putStrLn "readY to plot"

      -- plot only one function to follow its course and change how many points to plot (track path !!!)***********************************WRITE WXMS
      --    *> let theOlmegs = ptcs = liT
      --  e.g> olmeg m = (head(ausw m liT))  
                   --M.writeWXCloudNODE (nub(ptc3 2)) (nub(ptc3 3)) (nub(ptc3 5)) (nub(ptc3 19)) (nub(ptc3 25)) (nub(ptc3 50)) -- similar to above      
                   --an idea space ptc5 ..ptc9 depend in bonlist 
                   let concon t = concat$concat t
                   M.writeWXCloudNODE (nub(tk 1 (concat subroutine))) (nub(tk 2 (concat subroutine))) (nub(tk 3 (concat subroutine))) (nub (tk 4 (concat subroutine))) (nub(tk 5 (concat subroutine))) (nub (tk 6 (concat subroutine))) -- (words(show((tk 7 (subroutine)))))  -- (nub(ptc9 5)) (nub(ptc9 25)) (nub(ptc9 50)) (nub(ptc9 100)) (nub(ptc9 125)) (nub(ptc0 5))  
                   M.writeWXCloud4 (ptc2 5) (ptc2 25) (ptc2 50) (ptc4 5) (ptc4 25) (ptc4 50)
                   putStrLn "END plotter";

                else putStr (unwords [""]) -- nothing :)
       plotOn
           --   M.writeWXCloudNODE (ptc0 5) (ptc0 25) (ptc0 50) (ptc0 100) (ptc0 125) (ptc0 150) -- ???

--              M.writeWXCloudNODE (ptc2 5) (ptc2 15) (ptc2 25) (ptc4 2) (ptc4 3) (ptc4 5)
--
       let pi = Punkt "M" Nothing Nothing Nothing Nothing Nothing
    -- on is the default :)   
       let offON ghAdd pipeBone = if offOn == 1 then do
                               let forBase = (length target)
                               foBase <- forM [1..forBase](\l -> do
        --e.g *> let li = ["AAABB","AABAB","AAA","BBBAA"]
 --       let pi = Punkt "M" Nothing Nothing Nothing Nothing Nothing
 --           *>kArmTest5 li 1 pi 1 1 [] "AAA"
                                     let pray = kArmTest5 addGh liT pipeBone 1 pi 1 1 [] ghAdd --"AAA"
                                     pray
                                     putStrLn "END 'display 'exportMother'"
                                     return (pray))
                               return foBase
                            else do 
                               (return [(return())]   )
                               --putStrLn "no display" 
       offON ghAdd pipebone

       let wHtml = 1
       let nn n =  (realToFrac n) 
       let nn1 n =  (realToFrac n) 
       let spawn b p = (tsRAW(p (nn b)))
       let swn b r = map (spawn b) r
       
      -- let mapIns n = map (inser n ) [1..9]
       --let cd n = do 
         --   cD <- forM [1..9] (\nu -> do
           --      let trunInt n = realToFrac n
             --    let inser n b = ausw b ([map show (ptc0 (n)),map show (ptc2 (n)),map show (ptc3 (n)),map show (ptc4 (n)),map show (ptc5 (n)),map show (ptc6 (n)),map show(ptc7 (n)),map show(ptc8 (n)),map show(ptc9 (n))]) 
               --  return(inser n nu))  
          ---  return(cD) i

 -- a length variable related to complexity of the resulting graph
       let tsR dr = length$nub$concat$tsRAW dr 
       let ts0 = tsR ptc0 --length (nub( ptc0 10)) 
       let ts2 = tsR ptc2 --length (nub( ptc2 25)) 
       let ts3 = tsR ptc3 --length (nub( ptc3 30)) 
       let ts4 = tsR (ptc4)
       let ts5 = tsR (ptc5)
       let ts6 = tsR ptc6 --length (nub( ptc6 10)) 
       let ts7 = tsR ptc7 --length (nub( ptc7 10)) 
       let ts8 = tsR (ptc8)
       let ts9 = tsR (ptc9)
       let stril = [ts0,ts2,ts3,ts4,ts5,ts6,ts7,ts8,ts9]

       let tsal g = tsRAW g
 ----------------------------------------------------
 -- connect to raPlot in git-pages
      -- let raplot = ht==1 then do
  
       -- ht:Int ;if==1 then plot else not
       -- target:[Int] ;length how many lines to plot
       -- pv1: progVar1 ;String  fst pick runKBASE
       -- daZip: a 2d representation of a 3d cloud 
--picks pt0 pt2 pt3 pt4 pt5 pt6 pt7 pt8 pt9 = [pt0,pt2,pt3,pt4,pt5,pt6,pt7,pt8,pt9] --daZip
       let writeHtmlIn target ht daZip1 daZip2 daZip3 textAA = do
                 if ht==1 && plot ==1 then do
                     let foBase = (length target)
                  --   let prem2 n = length$nub$group$ausw n fotsRAW
                    -- let prem3 d n = map length$group$nub$ausw n d
                     foPeace <- forM [1](\j -> do
                          let textAA = "=>(1.167*10^5*%i+2.772*10^5)^(1/3)+(4.489*10^3)/(1.167*10^5*%i+2.772*10^5)^(1/3)+69.18\"\n"
                          
                          let infoSt pt pt2 = ("points: "++ pt ++ "\n 'daZip'-type: " ++(show (daZip (head toca) pt2))++"\n reduced-2d: "++ (show (build2d pt2)) ++" length: "++ show (length$concat$tsRAW pt2) ++ "\n" ++ show(tsRAW pt2)) 
                          let foalt = map words [("info ptc0\n"++ ((infoSt (show ts0) (ptc0)) )),("info ptc2\n"++ infoSt (show ts2) (ptc2) ),("info ptc3"++ infoSt (show ts3) (ptc3) ),("info ptc4"++ infoSt (show ts4) (ptc4) ),("info ptc5\n"++ infoSt (show ts5) (ptc5) ),("info ptc6\n"++ infoSt (show ts6) (ptc6) ),("info ptc7\n"++ infoSt (show ts7) (ptc7) ),("info ptc8\n"++ infoSt (show ts8) (ptc8) ),("info ptc9\n"++ infoSt (show ts9) (ptc9) )]  
                          ptcButoons <- forM [1..9] (\btn -> do
                                
                                let seleD = head(ausw btn stril)
                                --let seleD = show((realToFrac foseleD)/3)
                                let rightPtc = if btn == 1  then btn - 1
                                               else btn

                                --chooslabel <- forM [1] (\wbt -> do  
                                let gh = if seleD ==3 then ["ptc"++show (rightPtc)++"reduced.png"]
                                         else if seleD ==6 then ["ptc"++show (rightPtc)++"green.png"]
                                         else if seleD == 30 then ["ptc"++show (rightPtc)++"red.png"]
                                         else ["ptc"++show (rightPtc)++"blue.png"]
                                
                                --         return (df))
                                --let altText = do stril 
                                return(gh))  
                                --return(seleD))   
              ----------------------------------------------------------------------------------------------------------------------------------------------------------     
                          let raplot = do
                                      goddTime <- forM [1..(length subA)] (\fa -> do
                                           let drinks = (concat (ausw fa subA)) -- e.g [[93.72586872586874,94.02573529411764,3.0303030303030303]]
                                           let xs = head drinks;
                                           let ys = head$ausw 2 drinks;
                                           let zs = last drinks;
                                           let foframe = "push()\n"++
				                         "translate(plotX +"++show xs ++", plotY +" ++show ys++", plotZ +"++ show zs++")\n"++
				                         "sphere(1)\n"++
				                          "pop()\n"
                                           return (foframe)) 
                                      return (goddTime)
                                      buildOP <- readFile "p5SimY/dataJstoOP.txt"
                                      
                                      let makRaport = return (thebase goddTime)
                                      let zipdataJs = (lines buildOP) ++ makRaport

                                      --makRaport
                                      writeFile "p5SimY/raPlotter.js" (unlines zipdataJs)
                                      putStrLn "wrote p5SimYs/raPlotter.js"
                                      --return(makRaport)
                                where
                                    substituteAlt = [(nub(ptc4 10)),(nub(ptc4 25)),(nub(ptc4 50)),(nub(ptc4 75)),(nub(ptc4 84)),(nub(ptc4 100))]; -- similar to above
                                    subA = head (ausw 1 substituteAlt);
                                    thebase beat = G.foraPlot beat;
                          raplot 
			   -- does not work on git-hub?
                          let sEEnplot = do
                                      godPlot <- forM [1..(length subA)] (\fa -> do
                                           let drinks = (concat (ausw fa subA)) -- e.g [[93.72586872586874,94.02573529411764,3.0303030303030303]]
                                           let xs = head drinks;
                                           let ys = head$ausw 2 drinks;
                                           let zs = last drinks;
                                           let foSeen = ("        seen.P("++show xs++","++show ys++","++show zs++")")
                                           return (foSeen)) 
                                      return (godPlot)
                                      buildSeen <- readFile "p5SimY/seenJs/simiYValsBuildRAW.txt"
                                      let seenHeader = (take 118 (lines buildSeen))
                                      let seenTail = (drop 122 (take 188 (lines buildSeen)))
                                      let target l = show (head (ausw l(head (ausw 1 (subA)))))
                                      let focusScreen = (lines ("      ], seen.P("++ target 1 ++","++ target 2++","++target 3++"))\n")) 
                                      let zipdataJs = ((seenHeader) ++ godPlot ++ focusScreen ++ (seenTail))
                                     
                                      --make pointcloud in seen.js and plot it
                                      writeFile "p5SimY/seenJs/simiYVals.html" (unlines zipdataJs)
                                      putStrLn "wrote p5SimYs/seenJs/simiYVals.html"
                                      --return(makRaport)
                                where
                                    substituteAlt = [(nub(ptc0 10)),(nub(ptc2 25)),(nub(ptc3 50)),(nub(ptc5 75)),(nub(ptc6 84)),(nub(ptc8 100))]; -- similar to above
                                    subA = head (ausw 1 substituteAlt);
                                    thebase beat = G.foraPlot beat;
                          sEEnplot         
                          let df =  (G.fobase progVar1 progVar2 progVar3 progVar4 progVar5 progVar6 daZip1 daZip2 daZip3 textAA (ptcButoons) (foalt))
                          
                          return (df))
                     return foPeace
                     --return(fobase "A" "A" "A" "A" "A" "A" daZip1 daZip2 daZip3 textAA (unlines (map fst (concat$foBae))) foalt))
                     putStrLn "1" 
                   --  writeFile "HtmlS/yourRun.html" ((unlines$concat$ foBae))
                     header <- readFile "HtmlS/allRowsHeader.txt"

                     let theListIV = header++(unlines foPeace)++ "</body>\n"++"</html>\n"
                     --writeFile "HtmlS/yourRun.html" (theListIV)
                     putStrLn (unlines foPeace)
                     writeFile  ("HtmlS/"++(forRunHtml)) (unlines$foPeace)
--------------------------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------------------------
                 ---------------------------------------------------------------------------------------------------------------------------------------------------------
 -------------- ###########################################################################################             EXPERIMENT 3
    -- added 12-9-2020 a solution domain LP and a syntacial domain IDF are related to each other
    --          with y''  
                 else if ht == 1 && plot==2 then do
                     putStrLn "Experiment 3"
                     let ert r = tk r (ptc6 250)
                     let xS r = head (ert r)  
                     let yS r = last (drop 1 (take 2 (ert r))) 
                     let zS r = last (ert r) 
                     let maXxS = map xS [1..100]   
                     let maXyS = map yS [1..100]
                     let maXzS = map zS [1..100]        
        -- engage Punkt data type 
        -- with the intention to plug in any function f(x)
        -- and select any x with m
                     let basis22 foAL m = maybePu (head (ausw m foAL ))
                     let fmrTEST3 io e e2 forLine =  checkflow  io [(Punkt  (head(checkflow [] [(basis4 e2 forLine)]))(Just (basis2 e 1 )) (Just (basis2 e 3 )) (Just (basis2 e 4 ))(Just (basis2 e 5 )) (Just (basis2 e 6))) ]
        -- based on role model 
   -- e.g
    --
                     let foAdecide2 foA = let boa rt t = (Just (maybePu2 rt t)) --let whereBreak = chainDistribute crit bonelist crit (lines "1")
                          in let mapMaybePun k = let ste1 k rt = (boa (head(ausw k rt))) ((Just (maybePu (head (ausw k rt)))) ) 
                                                 in ste1 k foA -- e.g foA = ["vb","vb2","vb3"]
                          in let preMoa = length foA
                          in let eindelijk = do (map mapMaybePun [1..preMoa]) 
                          in 
                          if foA==[] then Nothing
                          else let chssd i = maybePu2 (head(ausw i foA))  (((boa (head(ausw i foA)) (head(ausw i eindelijk)))))  
                               in Just (show[(chssd 1)])
                     {-let infoSt pt pt2 = ("points: "++ pt ++ "\n 'daZip'-type: " ++(show (daZip (head toca) pt2))++"\n reduced-2d: "++ (show (build2d pt2)) ++" length: "++ show (length$concat$tsRAW pt2) ++ "\n" ++ show(tsRAW pt2)) 
                     
                     let foalt = map words [("info ptc0\n"++ ((infoSt (show ts0) (ptc0)) )),("info ptc2\n"++ infoSt (show ts2) (ptc2) ),("info ptc3"++ infoSt (show ts3) (ptc3) ),("info ptc4"++ infoSt (show ts4) (ptc4) ),("info ptc5\n"++ infoSt (show ts5) (ptc5) ),("info ptc6\n"++ infoSt (show ts6) (ptc6) ),("info ptc7\n"++ infoSt (show ts7) (ptc7) ),("info ptc8\n"++ infoSt (show ts8) (ptc8) ),("info ptc9\n"++ infoSt (show ts9) (ptc9) )]  
                     ptcButoons <- forM [1..9] (\btn -> do
                                
                                let seleD = head(ausw btn stril)
                                --let seleD = show((realToFrac foseleD)/3)
                                let rightPtc = if btn == 1  then btn - 1
                                               else btn

                                --chooslabel <- forM [1] (\wbt -> do  
                                let gh = if seleD ==3 then ["ptc"++show (rightPtc)++"reduced.png"]
                                         else if seleD ==6 then ["ptc"++show (rightPtc)++"green.png"]
                                         else if seleD == 30 then ["ptc"++show (rightPtc)++"red.png"]
                                         else ["ptc"++show (rightPtc)++"blue.png"]
                                
                                --         return (df))
                                --let altText = do stril 
                                return(gh))  
                    -- let li = (unlines [pV1,pV3,pV4,pV6])
                     let pi = Punkt "MM" Nothing Nothing Nothing Nothing Nothing
         -- build to be called as iteration of simiVal functions and phiMax rating 
                     kWORK <- forM [1] (\btn -> do
                                let makeWORK = kArmWORK addGh liT bonelist 1 pi 1 1 [] ghAdd    
                          --      let seleD = head(ausw btn stril)
                                --let seleD = show((realToFrac foseleD)/3)
                            --    let rightPtc = if btn == 1  then btn - 1
                                     --          else btn

                                --chooslabel <- forM [1] (\wbt -> do  
                              --  let gh = if seleD ==3 then ["ptc"++show (rightPtc)++"reduced.png"]
                                --         else if seleD ==6 then ["ptc"++show (rightPtc)++"green.png"]
                                  --       else if seleD == 30 then ["ptc"++show (rightPtc)++"red.png"]
                                    --     else ["ptc"++show (rightPtc)++"blue.png"]
                                
                                --         return (df))
                                --let altText = do stril
                                putStrLn (show makeWORK )
                                return())  
                              -}
                     let exP3 =  (experiment3RAW22 liSolution "ttt" [liT] subroutinE ptc6 [liT]) --runEXP3 [progVar1,progVar2,progVar3,progVar4,progVar5,progVar6] pi "AAA" --kWORK --(G.fobase progVar1 progVar2 progVar3 progVar4 progVar5 progVar6 daZip1 daZip2 daZip3 textAA (ptcButoons) (foalt))
            --         let comp1 = (runEXP3Char pi ghAdd)
              --       let comp2 = (zipWith (+) [1,1,1,1] (runEXP3Char pi ghAdd))
                --     let comp3 = let mis t z = z - t
                  --               in (map (mis 1) (runEXP3Char pi ghAdd))
                    -- let thecombs = [comp1,comp2,comp3]
                     return (exP3)
                    
                     do (putStrLn$show$head$exP3)
       --return (cd n)          
                                  --putStrLn "1"
     --  let chuckList x = [(ptc0 x),(ptc2 x),(ptc3 x)]
      -- print (concat(chuckList n))

                 else do 
                     putStrLn "alternate output: set NO HTML and NO Experiment via variable ht"
                     putStrLn (unwords(return("")))

       (writeHtmlIn target wHtml (show(daZip 1 ptc0)) (show(daZip 2 ptc0)) (show(daZip 3 ptc0))  "Time?")
        
----------------------------------------------------------------------------------------------




--addGh:Int ; 1 == add new line to a bonelist: ghCheck and write111111111
-- e.g> let li = ["AAABB","AABAB","AAA","BBBAA"]
--       let pi = Punkt "M" Nothing Nothing Nothing Nothing Nothing
-- e.g>  kArmTest5 2 ["AAABB","AAABBAAABAB","AAABAB","AAA","AAABBBAA","BBBAA"] li 1 pi 1 1 [] "DD"
kArmTest5 addGh liT bonelist mofaList connectWrist dit dit2 mCommand crit= do
     let allAcc foPun =  (checkflow [] [(foPun)])
 
     let foAdecide foA = if foA==[] then Nothing
                         else (Just (maybePu foA)) 

-- make a function that is a [(Maybe Punkt)]-> that by itself is the definiton of
-- mother :: Punkt -> Maybe Punkt 
-- this function below shall lead to => a motherTYPE that is depending on the type of simiyritYvalue
     let foAdecide2 foA = let boa rt t = (Just (maybePu2 rt t)) --let whereBreak = chainDistribute crit bonelist crit (lines "1")
                          in let mapMaybePun k = let ste1 k rt = (boa (head(ausw k rt))) ((Just (maybePu (head (ausw k rt)))) ) 
                                                 in ste1 k foA -- e.g foA = ["vb","vb2","vb3"]
                          in let preMoa = length foA
                          in let eindelijk = do (map mapMaybePun [1..preMoa]) 
                          in 
                          if foA==[] then Nothing
                          else let chssd i = maybePu2 (head(ausw i foA))  (((boa (head(ausw i foA)) (head(ausw i eindelijk)))))  
                               in Just (show[(chssd 1)])
     let mayer foa = if foa == [] then [""]
                              else lines(show [(foAdecide2 foa)])
     let mayer2 r foa = if (foa) == [] then maybePu "empty"
                                       else maybePu2 (r) (Just(maybePu ((show [(foAdecide2 (foa))]))))
     let justNames = ["name1","name2","name3"]
     let motherType foas r = map (mayer2 (head(ausw r (justNames)))) ([foas])

     putStrLn "TEST mayer2"
     let formTestRAW io r e e2 normalFunction =  let punktOrPg = checkflow  io [(Punkt  (head(checkflow [] [(basis4 e2 r)])) (Just (basis2 e 1 )) (Just (basis2 e 3 )) (Just (basis2 e 4 )) (Just (basis2 e 5 )) (Just (basis2 e 6))) ]
                 in let choosBy = length(head (group(punktOrPg)))
                 in if choosBy ==2 then normalFunction
                                   else punktOrPg 
 

     let formTest io r e normalFunction = formTestRAW io r e liT normalFunction

     let fnACCRAW cou = if unPoint == ("\"notM\"") then fst cou
                        else snd cou
            where 
             unPoint = (show(head(words(unwords(checkflow [] [connectWrist]))))) ;
 
 -- import 'roaming' data into kArmTrack5     |  
 --  all data in one clunky data-type :
     let checkFo g = if (length g) == 0 then ""
                     else "MOTHER MODE: on"  

     let basis mm foA = Punkt (fnACCRAW(nACCRAW (unwords(allAcc (connectWrist))) ["When set M will work:"++" now sleeping", checkFo mm ] ) ) foA foA foA foA foA
    -- EXAMPLE Punkt new format connect two [Maybe Punkt] list1 and list2
     let formationRAW io1 io2 rd = (head(head(map words(checkflow io1 [(basis (checkflow io1 [(basis4 liT (preX rd))]) (Just (maybePu(unwords(formTest io2 (preX rd) [show(F.fourierMQ6NOPAN123 rd )] (words(show(sin rd))))))  ))]))))
     let formation io2 rd = [(formationRAW [mother] io2 (realToFrac rd))] --(preX rd) list1 (checkflow [] [(basis4 list2 rd)])) 
--formTest io  rd bonelist io2 rd
   --  let formation2 io1 io2 rd  = checkflow [] [(basis (checkflow io1 [(basis4 liT (preX rd))]) (Just (maybePu(unwords(formTest io2 (preX rd) [show(F.fourierMQ6NOPAN123 rd )] (words(show(sin rd))))))  ))]
     let asDot2 inp = ((map mayer (map words inp)))

 -- RETRIEVE the DATA turn any 
 -- type:  [Maybe Punkt] -> String
 -- with brute force.
     let justGoneRAW2 io t  = let prep1 = ((map ord (((head(ausw t((formation io t))))))))
         -- take always second
                           in let prep2 = head ((ord '[') `elemIndices` (prep1))
         -- take always second , fixed
                           in let prep3 = head  ((ord ']') `elemIndices` (prep1))

                           in let prep4 = drop prep2 (take prep3 ((head((ausw t ((formation io t))))))) 
                           in let filAll = filter (/=92) (filter (/=34) (filter (/=93) (filter (/=91) (map ord (prep4))))) 
                           in map chr filAll
     let justGone2 io t = justGoneRAW2 io t  --(asDot2 ([(show(formation [] 10))])) 
     putStrLn (show(map scanChar(formationRAW [mother] [mother] (realToFrac 1))))  --------------------------------------Punkt data: show pg values
     let formT io r z = (formTest io r liT z)
     putStrLn (show (formT [father] 2 bonelist )) ---------------------------------------------------------------data for Punkt  mother father ...progVars
   --  let basis mm foA = basisRAW fnACCRAW nACCRAW connectWrist checkFo foA 
     -- take from deze [String] e.g -> 
     let foChain = length bonelist
     let makePalette pick1 pick2 punktList togoToList togtoList2  = noSense togoToList togtoList2
                       where
                          dada fopick fodeze = head (ausw (read fopick) fodeze);
                          noSense deze deze2 = Punkt (dada pick1 deze) (Just (maybePu (dada pick2 deze2))) Nothing Nothing Nothing Nothing;
     let gaussRate eins zuI = similaritYvalue eins zuI
     -- ---------------------------------------------------------------------------------
     let beRepKEYRAW pick1 pick2 onelist twoList punktList = (crunchtoSimi pick2) 
                      where
                  --      commands =   [bone1,bone2,bone3,bone4,bone5]; 
                        compar = head (ausw (read pick2) twoList);
                        paletteKEY fpic2 twoList astrList = makePalette pick1 fpic2 astrList onelist twoList;
                        cleanPaletteRaw fpic2 twoList astrList = checkflow [] [(paletteKEY fpic2 twoList astrList )];  -- go to pick2 
                        crunchtoSimi fpic2  = let sta1 d = map ord d  
                                              in let sta2New = let ste1 = (map realToFrac (sta1 (unwords(cleanPaletteRaw fpic2 twoList punktList))))
                                                                in drop 1 (take (length ste1) ste1)
                                              in let sta3New = map realToFrac (sta1 ((compar))) -- *****************************SELECT EACH LINE OF GUESS
                                              in gaussRate sta3New sta2New

     let beRepKEY pick1 pick2 punktList = (beRepKEYRAW pick1 pick2 commands commands punktList  )-- makePalette pick1 pick2 punktList commands
                      where
                        commands = bonelist; 
     
     let frame0 ibonelist i6 i7 i8 i9 =  (frame0a)   
         	  where
              --------------------------------------------------------------------------------------------------------------------------------------
            frame0a = do
                   -----------------------------------------------------------------------------------------------
--
              let theTrix2 crit = chainDistribute crit bonelist crit (lines "1")
                            -- ACCESS functions:
              putStrLn "TEST fomration TYPE "
              let foinnerOrd = head(snd (theTrix2 crit)) --
              let foinner = last (snd(theTrix2 crit))
              let rythm = (32) `elemIndices` (foinnerOrd)
              let prepRyth = if head rythm ==0 then length rythm
                             else (length rythm)+1

              let sortWith aa bb = let fuLength = length foinnerOrd
                             in let dooer aa bb = beRepKEY aa bb []  
                             in dooer aa bb --show fuLength

              putStrLn (show (beRepKEY (show dit) (show dit2) []))

              putStrLn ((show rythm) ++ " break positions in whole bonelist") 
-- ---------------------------------------------------------------------------

              randPunktList <- forM [1..(prepRyth)] (\z -> do
                        let chhos = ((ausw z (concat(snd(theTrix2 crit)))))  
                        let mapRepKEY aa = (sortWith aa) (show z)
                        let forole = (prepRyth -1)
                        roleKEY <- forM [1..prepRyth] (\y -> do -----------------------------------------------------------------Export complete simiYmatrix as Strig
                                let gegenStueck = filter (/=z) [1..prepRyth]
                                let makShow aa = show(mapRepKEY aa)
                                let role = map makShow (map show gegenStueck)
                                return(role) )
                        roleKEY2 <- forM [1..prepRyth] (\y -> do ----------------------------------------------------------------Export as Double 
                                let gegenStueck = filter (/=z) [1..prepRyth]
                                let role = mapRepKEY(show y)
                                return(role) ) 
 
                        let sta1 =  roleKEY2 --(ausw 1 (concat (take 1 roleKEY2)))
                        let sta2 = sum sta1
                        putStrLn ((show sta1)++" "++((show sta2)))
                        return (sta2,sta1))
              putStrLn (show (map fst randPunktList))
              putStrLn (show (map snd randPunktList))
              putStrLn "Track"
----------------------------------------------------------------------------------

-- MAXIMUM 'randPunktList'
--  sortEm := maximum simSums  -> find corresponding line
--  lineorder
--  => most different Line
--                   a                 ab       [0,1]
--                   b --\             ab  --\  [0,1]
--                   c --/             d   --/  [3]
--    natural order: d     simi order: c        [2]
--  => innerlineorder  a1 a2 a3 a4 -> a2 a1 a3 a4
              sortEm <- forM [1..prepRyth] (\pr -> do
                      let toStep e w = ( w `elemIndices` e)
                      let tmaps t w2 = map (toStep (sort (map fst randPunktList))) (ausw t w2)
                      let lineorder = tmaps pr (map fst randPunktList)
                      putStrLn "testin"
                      return (lineorder))
 -- A) comparing the phiMax line to an ordered  phi line: compare (simYval) to  (sort phiMax)
 -- B  comparing phiMax line to the phiMin
 -- C) compare phiMax line to another line
              inlinesortEm <- forM [1..prepRyth] (\pw -> do
                             let toStep e w = ( w `elemIndices` e)
                             let lineorder =  nub(concat(concat sortEm))
                             let prep = zipWith (+) (take (length lineorder) [1,1..]) (lineorder)
                             let tmaps2 z =  (ausw z (concat (map snd randPunktList)))
                             let oi  =  head( (tmaps2 ) pw)
                             let tmaps3 z =  (ausw z ((map snd randPunktList)))
                             let oi2  =  head(ausw pw (concat(map (tmaps3 ) prep)))
                             putStrLn (show oi2)
                             tmap3 <- forM [1..prepRyth] (\pi -> do
                                    let oiDo =  (tmaps3 pi)
                                    let oiDoform = ((concat (ausw pi ( head (ausw pw (map tmaps3 prep))))))
                                    return((oiDoform)))
 
                             let theInner = (concat (take 1 tmap3 )) 
                             return(theInner))
          
              putStrLn "below: sortEm ; inlinesortEm; " 
              putStrLn ((unlines (map show (concat (sortEm)))))
              putStrLn ((show inlinesortEm))
              let boneMax = ausw (maximum(concat(concat sortEm))) bonelist
              putStrLn (show boneMax)
              --run randomPunkt list with input:
 --e.g *> ghCheck = "AAAABBBB"    
 -- COMPARE BONELIST to INPUT:  (a'S of bonlist) to  ghCheck
              let ghCheck = concat (take prepRyth (repeat [crit]))
                            
-- COMPARE bonlist this time :  ghCheck to (a'S of bonelist )
--  =>  randPunktList2 /= randPunktList3 but randPunktist2 = (transpose randPunkList3)   
              randPunktList3 <- forM [1..(prepRyth)] (\z -> do
                        let chhos = ((ausw z (concat(snd(theTrix2 crit))))) 
                        let mapRepKEY aa = beRepKEYRAW aa (show z) bonelist ghCheck []
                        let forole = (prepRyth -1)
                        roleKEY <- forM [1..prepRyth] (\y -> do -----------------------------------------------------------------Export complete simiYmatrix as Strig
                                let gegenStueck = filter (/=z) [1..prepRyth]
                                let makShow aa = show(mapRepKEY aa)
                                let role = map makShow (map show gegenStueck)
                                return(role) )
                        roleKEY2 <- forM [1..prepRyth] (\y -> do ----------------------------------------------------------------Export as Double 
                                let gegenStueck = filter (/=z) [1..prepRyth]
                                let role = mapRepKEY(show y)
                                return(role) ) 
 
                        let sta1 =  roleKEY2 --(ausw 1 (concat (take 1 roleKEY2)))
                        let sta2 = sum sta1
                        putStrLn ((show sta1)++" "++((show sta2)))
                        return (sta1))
              putStrLn "sum matrix row" 
	 -- The input variable ghCheck is sorted into phiMax list.
	 -- ghCheck could end up in any spot of a list function 'sortEmInput' 
	 -- find max different of group 
	 -- ghCheck
	       
              let prepPhiorder = take 1 randPunktList3 
              
              sortEmInput <- forM [1..prepRyth] (\pr -> do
                      let withInput = (map realToFrac (concat prepPhiorder)) -- now we can use maximum again
                      let toStep e w = ( w `elemIndices` e)
                      let tmaps t w2 = map (toStep (sort (map fst randPunktList))) (ausw t w2)
                      let lineorder = last (last (tmaps pr (map fst randPunktList))) -- take phimax order
                      let accessSTEPI = (map snd randPunktList)
                      let foAddgh = head (sort accessSTEPI)
                      --(show (map last (concat sortEm))))  => [1,1,3,2] ; phiMax order
                      let phiMaxorder =  (map last (concat sortEm)) -- => [1,1,3,2] 
                      let phiMsorted i =  (head (ausw i (reverse (sort phiMaxorder)))) -- we want to reverse to count 'downwards' => [3,2,1,1] 
                --      let mapPhi = realToFrac (phiMsorted 1) --[1..prepRyth]
                      let tryOut  fo =  toStep  [((ausw (phiMsorted fo ) (concat prepPhiorder) ))] ((withInput))
                      runTriesRAW <- forM [1..(length phiMaxorder)] (\ol -> do
                              let try = ((ausw (phiMsorted ol) (concat prepPhiorder) )) -- => [[1.5151515151515151]]
                              let fotraceOther i = head(ausw i foAddgh)
                              let traceOther = map fotraceOther (reverse (sort phiMaxorder))
                              let cheGH = maximum (take ol (reverse(sort withInput)))
          -- see the difference between ( [phiMax] )  and   ( ghCheck compared [phiMax] ) 
                              let nowSort = (fotraceOther (phiMsorted ol)) + (head(map realToFrac (ausw (phiMsorted ol) (concat prepPhiorder))))
                              let applyAlgo = if (nowSort)>cheGH  then 
                                                  let ste1 = break (==cheGH) (map realToFrac (concat prepPhiorder)) 
                                                  in concat(((fst ste1)): [(nowSort:(snd ste1))]) --"right" --ste1
                                              else  (map realToFrac (concat prepPhiorder)) --"false"
                                     
                              return (applyAlgo,cheGH))  -- ( ( inserts ghCheck [phiMax]    ,(the changeable phiMax of bonelist))
                      let runTries = map fst runTriesRAW
                      let judgeTries = map length runTries
                      let filterMy = let ste1 = filter (==(maximum judgeTries)) judgeTries
                                     in let seeTries g k = ausw (phiMsorted g) (ausw k runTries)

                                     in if ste1 == [] then bonelist
                                        
                -- proposition : take 1 runTries = [[39.44954128440367,39.44954128440367,1.5151515151515151,41.88212399221574,39.63414634146342],
                --               if                                                             A         >       B
                --               then sort ghCheck right of A
                                        else let fobuildD = (take (phiMsorted 2) (map words bonelist))  --ghCheck
                                                     in let fobuildTail = (take ((-1*(phiMsorted 1))+(length bonelist)) (reverse ((map words bonelist))))
                                                     in let withGH = ("GGGG":(concat fobuildTail))
                                                     in let getMaxIn = (maximum (map snd runTriesRAW)) `elemIndices` (head runTries)
                                                     in let prepMax = zipWith (+) (take (length runTries) [1,1..]) getMaxIn
                                                     in let fobuildHead = take ((phiMsorted 1)-1) ( (map words bonelist)) 

                                    -- phiMsorted => [3,2,1,1] 
                                                        --  if bigger ordered to the right of max
                                                     in if (map realToFrac ((ausw ((phiMsorted 1)+1)(concat(take 1 runTries ))))) > ((map realToFrac (ausw ((phiMsorted 1))(concat(take 1 runTries ))))) 
                                                        then let fobuildD = (take (phiMsorted 1) [bonelist])  --ghCheck
                                                             in (concat fobuildHead)++boneMax++["right"]++(concat fobuildTail) 
                                                        else (concat fobuildHead)++["left"]++boneMax++(concat fobuildTail) 
                                        --else ausw 1 runTries
                --      putStrLn (show judgeTries)
                  --    putStrLn (show (runTries))
                    --  putStrLn ((show(map snd runTriesRAW)))
                                  
                      return ((filterMy,(runTries))) ) 
 
         --  e.g  ["AAABB","AABAB","AAA","BBBAA"]  -> ["AAABB"/"AAABAB","BBBAA","AAA"]  -> sortEm
--                                        -> ( [0,1],  [0,1],    [3],   [2]] ) -> 
--       => sortEM                           
              let findOrdeR = nub (concat(concat (sortEm)))      -- -> [
              putStrLn "FinD ORDER"
              putStrLn (show findOrdeR) 
               -- inp:[String] -> bonelist
       -- the lineorder can be funnelt into a (maybe mother)/= Nothing
       --  a motherType e.g simiSum Order 
              let toStep e w = ( w `elemIndices` e)
              let muster u = ausw u (sort(head inlinesortEm)) -- bonelist -> simiVals
              let withGHcheck = ((concat(map snd sortEmInput))) 
              let muster2 u = ausw u (sort(head withGHcheck)) -- ghCheck: bonelist -> simiVals
              putStrLn (show( muster2 1))
              putStrLn (show(muster 1))
            --  putStrLn (show inlinesortEm)
              putStrLn "with GH" 
           --   putStrLn (show withGHcheck)
              let justIO = inlinesortEm
              let toIter foa = (Just(maybePu ((show [(foAdecide2 (foa))]))))
              let inMap = map toIter (map words justNames)
              let mayer3 r foa = if (foa) == [] then maybePu "empty"
                                else maybePu2 (r) (head (ausw 3 inMap))

              putStrLn "1" --(checkflow [] (toIter justNames))
              let justGene u =  map show(ausw u justIO)
-- A) comparing the phiMax line to an ordered  phi line: compare (simYval) to  (sort phiMax)
-- B  comparing phiMax line to the phiMin
-- C) compare phiMax line to another line

      -- CHOOSE A STRATEGY build on A
      -- choosen: 
      --  1. maximum -> step to next smaller atom ....  -- r shall be mapped
              let enduranceRace8a9_6_20 r u = show (map (toStep (muster u)) (concat (ausw r justIO))  ) -- stepI a. ([phiMax])
              let edR1 r u = enduranceRace8a9_6_20 r u 

    -- function below can crash the whole 'kArmTest5' the error handeling 
    -- can be done via a Punkt ?! ------------------------------------------------####################################################### 20-6-20 
    --                                                                                                               must have li order to work 
              let edRGH r u = show (map (toStep (muster2 u)) (concat(ausw r withGHcheck))  ) -- stepI b.  ghCheck: [phiMax]
            --  putStrLn (show(map (edRGH 1) [1..5]))
              let moreEdR1 r = map (edR1 r) [1..prepRyth]
              
 --           u: the normal order ?     
 --   r:Int ; which to take of sortEm to be mapped with [1..(length bonelist)]
            --    let motherTYPE o u r = map (mayer2 ((ausw r (edR1 r o )))) [(map (edR1 r) u )]
              let areplace r o = ((ausw r (edR1 r o )))
              let motherTYPE o u r = map (mayer2 (concat(ausw r (map show findOrdeR)))) [(map (edR1 r) u )]
              let motherTYPE2 o u r = map (mayer2 (concat(ausw r (map show findOrdeR)))) [(map (edRGH r) u )]

              let mapMo o r = unwords (checkflow [mother] (motherTYPE o [1..prepRyth] r))
              let mapMo2 o r = unwords (checkflow [mother] (motherTYPE2 o [1..prepRyth] r))
              let foexpoMo r w =  unlines (map (edR1 w ) r)
              --let innerLineMother = 
              let exportMother r = unlines(map (foexpoMo r ) r)
              let exportMother2 o = (words (concat(map (mapMo o) [2,3,4])))
              --let tryformation io1 io2 r = formationRAW io1 io2 r --bonelist [(exportMother [1..r])] io r 
              putStrLn "\n test mother functions"
             -- putStrLn (show (tryformation [mother] 1))
          --    putStrLn (show (map muster [1..4]))
          --    putStrLn (show (map muster2 [1..4]))
              putStrLn (unlines(checkflow [mother] (ausw 1 (motherType (justGene 3) (3)))))
   -- Punkt function:
   -- insert your desired mother type to any Punkt
   -- e.g *> let mymotherT03 t = (ausw 1 (motherType (justGene t) (t)))
   --     *> let accesMT03 t= unlines$checkflow [mother] mymotherT3$ t
   --   use function 'tester' below to accomplish that
              let tester t = (unlines(checkflow [mother] (ausw 1 (motherType (justGene t) (t)))))
              let motherType3 foas r = map (mayer3 (head(ausw r (map show justIO)))) ([foas])
              putStrLn (unlines(checkflow [] (ausw 1 (motherType3 (justGene 3) (3)))))
              putStrLn (tester 4)
              let addGH = if addGh == 2 then do
                            let toFrame =  [(edR1 1 1)  ++ "     " ++ (edRGH 1 1)++"\n"++
                                            (edR1 1 2)  ++ "     " ++ (edRGH 1 2)++"\n"++
                                            (edR1 1 3)  ++ "      " ++ (edRGH 1 3)++"\n"++
                                            (edR1 1 4)  ++ "      " ++ (edRGH 1 4)++"\n"++

                                             "                    " ++ (edRGH 1 5)++"\n"++
                                            (edR1 2 1)  ++ "     " ++ (edRGH 2 1)++"\n"++
                                            (edR1 2 2)  ++ "     " ++ (edRGH 2 2)++"\n"++
                                            (edR1 2 3)  ++ "      " ++ (edRGH 2 3)++"\n"++
                                            (edR1 2 4)  ++ "      " ++ (edRGH 2 4)++"\n"++
                                             "                    "++ (edRGH 2 5)++"\n"++
                                            (edR1 3 1)  ++ "      " ++ (edRGH 3 1)++"\n"++
                                            (edR1 3 2)  ++ "      " ++ (edRGH 3 2)++"\n"++
                                            (edR1 3 3)  ++ "     " ++ (edRGH 3 3)++"\n"++
                                            (edR1 3 4)  ++ "       " ++ (edRGH 3 4)++"\n"++
                                             "                    " ++ (edRGH 3 5)++"\n"++ 
                                            (edR1 4 1) ++"\n"++ -- ++ "      " ++ (edRGH 4 1))
                                            (edR1 4 2) ++"\n"++ --  ++ "      " ++ (edRGH 4 2))
                                            (edR1 4 3)  ++"\n"++ -- ++ "       "  ++ (edRGH 4 3))
                                            (edR1 4 4) ++"\n"++ -- ++ "     " ++ (edRGH 4 4)) 
                                            "                    " ++ (edRGH 5 5)]

              --              runs <- forM [1] (\nr -> do
                            C.iframe_c 1 2 "<p>" "wd" toFrame "3"
                  --              return(gh))
                    --        return runs
                            putStrLn ((edR1 1 1)  ++ "     " ++ (edRGH 1 1)) -- maybePu
                            putStrLn ((edR1 1 2)  ++ "     " ++ (edRGH 1 2))
                            putStrLn ((edR1 1 3)  ++ "      " ++ (edRGH 1 3))
                            putStrLn ((edR1 1 4)  ++ "      " ++ (edRGH 1 4))

                            putStrLn ("                    " ++ (edRGH 1 5))
                            putStrLn ((edR1 2 1)  ++ "     " ++ (edRGH 2 1))
                            putStrLn ((edR1 2 2)  ++ "     " ++ (edRGH 2 2))
                            putStrLn ((edR1 2 3)  ++ "      " ++ (edRGH 2 3))
                            putStrLn ((edR1 2 4)  ++ "      " ++ (edRGH 2 4))
                            putStrLn ("                    "++ (edRGH 2 5))
                            putStrLn ((edR1 3 1)  ++ "      " ++ (edRGH 3 1))
                            putStrLn ((edR1 3 2)  ++ "      " ++ (edRGH 3 2))
                            putStrLn ((edR1 3 3)  ++ "     " ++ (edRGH 3 3))
                            putStrLn ((edR1 3 4)  ++ "       " ++ (edRGH 3 4))
                            putStrLn ("                    " ++ (edRGH 3 5)) 
                            putStrLn ((edR1 4 1) )-- ++ "      " ++ (edRGH 4 1))
                            putStrLn ((edR1 4 2) )--  ++ "      " ++ (edRGH 4 2))
                            putStrLn ((edR1 4 3)  ) -- ++ "       "  ++ (edRGH 4 3))
                            putStrLn ((edR1 4 4) ) -- ++ "     " ++ (edRGH 4 4)) 
                         --   putStrLn ("                    " ++ (edRGH 4 5))
                            putStrLn ("                    " ++ (edRGH 5 5))
                          else if addGh == 1 then do
                            putStrLn ((edR1 1 1) )
                            putStrLn ((edR1 1 2) )
                            putStrLn ((edR1 1 3) )
                            putStrLn ((edR1 1 4) )

                            putStrLn ((edR1 2 1) )
                            putStrLn ((edR1 2 2) )
                            putStrLn ((edR1 2 3) )
                            putStrLn ((edR1 2 4) )

                            putStrLn ((edR1 3 1) )
                            putStrLn ((edR1 3 2) )
                            putStrLn ((edR1 3 3) )
                            putStrLn ((edR1 3 4) )

                            putStrLn ((edR1 4 1) )
                            putStrLn ((edR1 4 2) )
                            putStrLn ((edR1 4 3) )
                            putStrLn ((edR1 4 4) ) 

                          else
                            (putStrLn"") 
              putStrLn "Test map mother"
              addGH
 -- BELOW ALL taken out for development but still valid 04-7-20
 
            --  putStrLn (unlines(sort(moreEdR1 1 )))
             -- putStrLn (unines(checkflow [mother] 
          --    putStrLn (mapMo 1 1)
          --    putStrLn (mapMo 1 2)
          --    putStrLn (mapMo 1 3)
          --    putStrLn "Test exportMother"
          --    putStrLn ((exportMother [1..4]))
          --    putStrLn (unlines (exportMother2 1))
             -- putStrLn (unlines(map fst randPunktList))
            --  putStrLn (unlines((map snd (unlines randPunktList)))
    -- Plot-------------------------------------------------------------------
          -- STEP III. of program
              let foe t = head (ausw t bonelist)
          --    let op = atrix0a (foe 1) (foe 2) (foe 3) (foe 4) (foe 2) (foe 1) 1 1
                 ---------------------------------------------------------------------------------------------------
--
    -- a writer cant solve a problem "just list what is this writer program"
    -- from here we work our way back which functions are needed
    -- even code that is a comment can still have a function in syntax and concept 
              let allFunctions selectFunc iO iO2 iO3 adds selectLine= do  putStrLn$unlines$head$ (return(allFUNC2 iO iO2 selectLine)) -- putStrLn "1" --GHC.selecTOR1 [(putStrLn (show((allFUNC iO iO2 iO3 adds selectLine)) ) )] --putStrLn "1"
                                                                       --   putStrLn$unlines$head$ (return(allFUNC2 iO iO2 selectLine))   
                    where
                     ranPL = ((map snd randPunktList),  "randPunktList" ); --(simSums, ofLine);
                     sorEM = ( sortEm,         "sortEm"        );
                     isoEM = ( inlinesortEm,   "inlineSortEm"  );  
                     raPL3 = ( randPunktList3, "randPunktist3" );
                     finDR = ( findOrdeR,      "findOrdeR"     ); -- :562
                     tS  e w  = ( (toStep e w),  "toStep"      ); -- :397
                 --    muuus u = ( (muster u) ,         "muster"        ); -- :569
                   --  wGHc =  (withGHcheck,     "withGHcheck"   ); -- :570
                     ms2 u =((muster2 u),         "muster2" ); -- :571
                     jIO   = (justIO,          "justIO"        ); -- :577
                     tTx2  = (theTrix2,        "theTrix2"      ); -- :313
                     ryt   = (rythm,           "rythm"         ); -- :320
                     pR    = (prepRyth,        "prepRyth"      ); -- :321
                     sWit  = (sortWith,        "sortWith"      ); -- :325
         {-            foInO = (foinnerOrd,      "foinnerOrd"    );  -- :319
                     foIn  = (foinner,         "foinner"       ); -- :320
                     toIt  = (toIter,          "toIter"        ); -- :578
                     iM    = (inMap,           "inMap"         ); -- :579
                     may3  = (mayer3,          "mayer3"        ); -- :581 
                     jGe u  = (justGene u,        "justGene"      ); -- :584
                     may   = (mayer,           "mayer"         ); -- :191
                     foAde = (foAdecide,       "foAdecide"     ); -- :176
                     fAde2 = (foAdecide2,      "foAdecide2"    ) ; -- :182
                -- :263   :1675  all functions in this range are in frame 
                     gauss = (gaussRate,       "gaussRate"     ); -- :241 
                     m2    = (mayer2,          "mayer"         ); -- :192
                     jNam  = (justNames,       "justNames"     ); -- :196
                     mT    = (motherType,      "motherType"    ); -- :197
                     fACR  = (fnACCRAW,        "fnACCRAW"      );  -- :210
                     cFo   = (checkFo,         "checkFo"       );  -- :208
                   --  baas  = (basis,           "basis"         ); -- :211
                     foCh  = (foChain,         "foChain"       ); -- :237
                     mkPal = (makePalette,     "makePalette"   );  -- :328
                     fomoN = (formation,       "formation"     );  -- :214
                     asD2  = (asDot2,          "asDot2"        );  -- :217
                     jsG   = (justGoneRAW2,    "justGoneRAW2"  ); -- :222
                     fMT   = (formT,           "formT"         ); -- :233 -}
                     jG    = (justGone2,       "justGone2"     ); -- :331    Punkt 
                     expoMum l = (exportMother l , "exportMother" );
                     lisOlists w = [w] 
                  --   mam = map expoMum ((map lisOlists [1..(selectLine)])); 
                   --  alABOVE fstOsnd = head (ausw selectFunc (map fstOsnd mam)); 
           --[ranPL,sorEM,isoEM,raPL3,finDR,(tS e w){-,(muuus u),wGHc,(ms2 u),-}jIO,tTx2,ryt,pR,sWit,foInO,foIn,toIt,iM,may3,jGe u,may,foAde,fAde2,gauss,m2,jNam,mT,fACR,cFo,baas,foCh,mkPal,fomoN,asD2,jsG,fMT,jG])
                     foFuncPair fox aDD =  (checkflow [] [mayer2"jack" fox] >>= (\x -> checkflow [] [(mayer2 "jack2" [(show x ++ aDD)])]));  --Just 3 >>= (\x -> Just (show x ++ "!"))
                     foFuncPair2  o r  =  (  [mayer2 "otto" [(mapMo o r)] ] >>= (\x ->  [(mayer2 "otto" [(show x ++"   " ++(mapMo2 o r))])])); 
                     punktUpFuncPair fox1 fox aDD io = [( mayer2 (unlines(foFuncPair fox1 "")) (checkflow io [(maybePu (concat(foFuncPair fox aDD)))]))];
                    
                  --   strives io io2 r aDD ifo = let preIO3 = (checkflow io2  [(maybePu2 "dot" (Just(head(ausw ifo (punktUpFuncPair [(alABOVE fst)] [(alABOVE snd)] aDD io)))))] ) --((head (checkflow [] (io3)))) 
                                   --         in preIO3 --(checkflow io3 [(want ((alABOVE kiez),(alABOVE kiez)))]) -- get name or functionality 
                     fmrTEST io r lis1 lis2 normFu  = formTestRAW io r lis1 lis2 normFu  --[(foFuncPair fox aDD)]
                     fmrTEST2 io e e2 forLine =  checkflow  io [(Punkt  (head(checkflow [] [(basis4 e2 forLine)])) (Just (basis2 e 1 )) (Just (basis2 e 3 )) (Just (basis2 e 4 )) (Just (basis2 e 5 )) (Just (basis2 e 6))) ]
                     mapfoFunc2 o = checkflow [] (concat(map (foFuncPair2 o) (fst finDR)))
                     fmrTEST3RAW io o r e2 forLine =  checkflow  io [(Punkt  (head(checkflow [] [(basis4 e2 forLine)])) (Just (head(foFuncPair2 o r ))) Nothing Nothing Nothing Nothing )]
                     fmrTEST3 io o r forLine = ((fmrTEST3RAW io o r (mapfoFunc2 o) forLine), "specialSort can get missing part???")
                     allFUNC2RAW io io2 forLine = fmrTEST2 io (ausw selectFunc [(fst (expoMum forLine)),show(fst ranPL),show(fst sorEM),show(fst (fmrTEST3 io forLine (head forLine) (head forLine)))])  (ausw selectFunc [(snd (expoMum forLine)),(snd ranPL),(snd sorEM),(snd (fmrTEST3 io2 forLine 1 (head forLine)))]) (head forLine) 
                     allFUNC2 io io2 forLine = (allFUNC2RAW io io2 [forLine]) 
           --   putStrLn (unlines(head(allFunctions 1 [] [] [] "adding" [1])))111
              (allFunctions 1 [] [] [] ""  1)
              (allFunctions 1 [mother] [] [] "a comment" 2)
             -- (allFunctions 1 [mother] [mother] [] "" 7)
              (allFunctions 1 [mother] [] [] "" 4)
              (allFunctions 1 [mother] [] [] "" 5)
              (allFunctions 1 [] [mother] [] "" 1)
              (allFunctions 1 [mother] [mother] [] "" 3)
            ---  (allFunctions 1 [father] [mother] [] "" 3)
           --   (allFunctions 2 [] [] [] "" 2)
              (allFunctions 2 [mother] [mother] [] "" 1)
              (allFunctions 3 [mother] [] [] "" 1)
              (allFunctions 3 [] [] [] "" 1)
              (allFunctions 3 [mother] [mother] [] "" 2)
              (allFunctions 3 [mother] [mother] [] "" 3)
              (allFunctions 3 [mother] [father] [] "" 3)
              (allFunctions 1 [mother] [mother] [] ""  2)
             -- (allFunctions 2 [] [] [] ""  1)
             -- (allFunctions 2 [] [] [] ""  2)
              putStrLn "Done" 
                  
     (frame0 bonelist (mofaList) connectWrist dit dit2) 
------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- added 12-09-2020 export from inside kArmTrack 
-- same as above with only ONE output selceted via allFunctiosn
--addGh:Int ; 1 == add new line to a bonelist: ghCheck and write111111111
-- e.g> let li = ["AAABB","AABAB","AAA","BBBAA"]
--       let pi = Punkt "M" Nothing Nothing Nothing Nothing Nothing
-- e.g>  kArmWORK 2 ["AAABB","AAABBAAABAB","AAABAB","AAA","AAABBBAA","BBBAA"] li 1 pi 1 1 [] "DD"
kArmWORK addGh liT bonelist mofaList connectWrist dit dit2 mCommand crit= do
     let allAcc foPun =  (checkflow [] [(foPun)])
 
     let foAdecide foA = if foA==[] then Nothing
                         else (Just (maybePu foA)) 

-- make a function that is a [(Maybe Punkt)]-> that by itself is the definiton of
-- mother :: Punkt -> Maybe Punkt 
-- this function below shall lead to => a motherTYPE that is depending on the type of simiyritYvalue
     let foAdecide2 foA = let boa rt t = (Just (maybePu2 rt t)) --let whereBreak = chainDistribute crit bonelist crit (lines "1")
                          in let mapMaybePun k = let ste1 k rt = (boa (head(ausw k rt))) ((Just (maybePu (head (ausw k rt)))) ) 
                                                 in ste1 k foA -- e.g foA = ["vb","vb2","vb3"]
                          in let preMoa = length foA
                          in let eindelijk = do (map mapMaybePun [1..preMoa]) 
                          in 
                          if foA==[] then Nothing
                          else let chssd i = maybePu2 (head(ausw i foA))  (((boa (head(ausw i foA)) (head(ausw i eindelijk)))))  
                               in Just (show[(chssd 1)])
     let mayer foa = if foa == [] then [""]
                              else lines(show [(foAdecide2 foa)])
     let mayer2 r foa = if (foa) == [] then maybePu "empty"
                                       else maybePu2 (r) (Just(maybePu ((show [(foAdecide2 (foa))]))))
     let justNames = ["name1","name2","name3"]
     let motherType foas r = map (mayer2 (head(ausw r (justNames)))) ([foas])

    -- putStrLn "TEST mayer2"
     let formTestRAW io r e e2 normalFunction =  let punktOrPg = checkflow  io [(Punkt  (head(checkflow [] [(basis4 e2 r)])) (Just (basis2 e 1 )) (Just (basis2 e 3 )) (Just (basis2 e 4 )) (Just (basis2 e 5 )) (Just (basis2 e 6))) ]
                 in let choosBy = length(head (group(punktOrPg)))
                 in if choosBy ==2 then normalFunction
                                   else punktOrPg 
 

     let formTest io r e normalFunction = formTestRAW io r e liT normalFunction

     let fnACCRAW cou = if unPoint == ("\"notM\"") then fst cou
                        else snd cou
            where 
             unPoint = (show(head(words(unwords(checkflow [] [connectWrist]))))) ;
 
 -- import 'roaming' data into kArmTrack5     |  
 --  all data in one clunky data-type :
     let checkFo g = if (length g) == 0 then ""
                     else "MOTHER MODE: on"  

     let basis mm foA = Punkt (fnACCRAW(nACCRAW (unwords(allAcc (connectWrist))) ["When set M will work:"++" now sleeping", checkFo mm ] ) ) foA foA foA foA foA
    -- EXAMPLE Punkt new format connect two [Maybe Punkt] list1 and list2
     let formationRAW io1 io2 rd = (head(head(map words(checkflow io1 [(basis (checkflow io1 [(basis4 liT (preX rd))]) (Just (maybePu(unwords(formTest io2 (preX rd) [show(F.fourierMQ6NOPAN123 rd )] (words(show(sin rd))))))  ))]))))
     let formation io2 rd = [(formationRAW [mother] io2 (realToFrac rd))] --(preX rd) list1 (checkflow [] [(basis4 list2 rd)])) 
--formTest io  rd bonelist io2 rd
   --  let formation2 io1 io2 rd  = checkflow [] [(basis (checkflow io1 [(basis4 liT (preX rd))]) (Just (maybePu(unwords(formTest io2 (preX rd) [show(F.fourierMQ6NOPAN123 rd )] (words(show(sin rd))))))  ))]
     let asDot2 inp = ((map mayer (map words inp)))

 -- RETRIEVE the DATA turn any 
 -- type:  [Maybe Punkt] -> String
 -- with brute force.
     let justGoneRAW2 io t  = let prep1 = ((map ord (((head(ausw t((formation io t))))))))
         -- take always second
                           in let prep2 = head ((ord '[') `elemIndices` (prep1))
         -- take always second , fixed
                           in let prep3 = head  ((ord ']') `elemIndices` (prep1))

                           in let prep4 = drop prep2 (take prep3 ((head((ausw t ((formation io t))))))) 
                           in let filAll = filter (/=92) (filter (/=34) (filter (/=93) (filter (/=91) (map ord (prep4))))) 
                           in map chr filAll
     let justGone2 io t = justGoneRAW2 io t  --(asDot2 ([(show(formation [] 10))])) 
     --putStrLn (show(map scanChar(formationRAW [mother] [mother] (realToFrac 1))))  --------------------------------------Punkt data: show pg values
     let formT io r z = (formTest io r liT z)
     --putStrLn (show (formT [father] 2 bonelist )) ---------------------------------------------------------------data for Punkt  mother father ...progVars
   --  let basis mm foA = basisRAW fnACCRAW nACCRAW connectWrist checkFo foA 
     -- take from deze [String] e.g -> 
     let foChain = length bonelist
     let makePalette pick1 pick2 punktList togoToList togtoList2  = noSense togoToList togtoList2
                       where
                          dada fopick fodeze = head (ausw (read fopick) fodeze);
                          noSense deze deze2 = Punkt (dada pick1 deze) (Just (maybePu (dada pick2 deze2))) Nothing Nothing Nothing Nothing;
     let gaussRate eins zuI = similaritYvalue eins zuI
     -- ---------------------------------------------------------------------------------
     let beRepKEYRAW pick1 pick2 onelist twoList punktList = (crunchtoSimi pick2) 
                      where
                  --      commands =   [bone1,bone2,bone3,bone4,bone5]; 
                        compar = head (ausw (read pick2) twoList);
                        paletteKEY fpic2 twoList astrList = makePalette pick1 fpic2 astrList onelist twoList;
                        cleanPaletteRaw fpic2 twoList astrList = checkflow [] [(paletteKEY fpic2 twoList astrList )];  -- go to pick2 
                        crunchtoSimi fpic2  = let sta1 d = map ord d  
                                              in let sta2New = let ste1 = (map realToFrac (sta1 (unwords(cleanPaletteRaw fpic2 twoList punktList))))
                                                                in drop 1 (take (length ste1) ste1)
                                              in let sta3New = map realToFrac (sta1 ((compar))) -- *****************************SELECT EACH LINE OF GUESS
                                              in gaussRate sta3New sta2New

     let beRepKEY pick1 pick2 punktList = (beRepKEYRAW pick1 pick2 commands commands punktList  )-- makePalette pick1 pick2 punktList commands
                      where
                        commands = bonelist; 
     
     let frame0 nEbonelist i6 i7 i8 i9 =  (frame0a)   
         	  where
              --------------------------------------------------------------------------------------------------------------------------------------
            frame0a = do
                   -----------------------------------------------------------------------------------------------
--
              let theTrix2 crit = chainDistribute crit bonelist crit (lines "1")
                            -- ACCESS functions:
       --       putStrLn "TEST fomration TYPE "
              let foinnerOrd = head(snd (theTrix2 crit)) --
              let foinner = last (snd(theTrix2 crit))
              let rythm = (32) `elemIndices` (foinnerOrd)
              let prepRyth = if head rythm ==0 then length rythm
                             else (length rythm)+1

              let sortWith aa bb = let fuLength = length foinnerOrd
                             in let dooer aa bb = beRepKEY aa bb []  
                             in dooer aa bb --show fuLength

         --     putStrLn (show (beRepKEY (show dit) (show dit2) []))

          --    putStrLn ((show rythm) ++ " break positions in whole bonelist") 
-- ---------------------------------------------------------------------------

              randPunktList <- forM [1..(prepRyth)] (\z -> do
                        let chhos = ((ausw z (concat(snd(theTrix2 crit)))))  
                        let mapRepKEY aa = (sortWith aa) (show z)
                        let forole = (prepRyth -1)
                        roleKEY <- forM [1..prepRyth] (\y -> do -----------------------------------------------------------------Export complete simiYmatrix as Strig
                                let gegenStueck = filter (/=z) [1..prepRyth]
                                let makShow aa = show(mapRepKEY aa)
                                let role = map makShow (map show gegenStueck)
                                return(role) )
                        roleKEY2 <- forM [1..prepRyth] (\y -> do ----------------------------------------------------------------Export as Double 
                                let gegenStueck = filter (/=z) [1..prepRyth]
                                let role = mapRepKEY(show y)
                                return(role) ) 
 
                        let sta1 =  roleKEY2 --(ausw 1 (concat (take 1 roleKEY2)))
                        let sta2 = sum sta1
            --            putStrLn ((show sta1)++" "++((show sta2)))
                        return (sta2,sta1))
            --  putStrLn (show (map fst randPunktList))
            --  putStrLn (show (map snd randPunktList))
            --  putStrLn "Track"
----------------------------------------------------------------------------------

-- MAXIMUM 'randPunktList'
--  sortEm := maximum simSums  -> find corresponding line
--  lineorder
--  => most different Line
--                   a                 ab       [0,1]
--                   b --\             ab  --\  [0,1]
--                   c --/             d   --/  [3]
--    natural order: d     simi order: c        [2]
--  => innerlineorder  a1 a2 a3 a4 -> a2 a1 a3 a4
              sortEm <- forM [1..prepRyth] (\pr -> do
                      let toStep e w = ( w `elemIndices` e)
                      let tmaps t w2 = map (toStep (sort (map fst randPunktList))) (ausw t w2)
                      let lineorder = tmaps pr (map fst randPunktList)
            --          putStrLn "testin"
                      return (lineorder))
 -- A) comparing the phiMax line to an ordered  phi line: compare (simYval) to  (sort phiMax)
 -- B  comparing phiMax line to the phiMin
 -- C) compare phiMax line to another line
              inlinesortEm <- forM [1..prepRyth] (\pw -> do
                             let toStep e w = ( w `elemIndices` e)
                             let lineorder =  nub(concat(concat sortEm))
                             let prep = zipWith (+) (take (length lineorder) [1,1..]) (lineorder)
                             let tmaps2 z =  (ausw z (concat (map snd randPunktList)))
                             let oi  =  head( (tmaps2 ) pw)
                             let tmaps3 z =  (ausw z ((map snd randPunktList)))
                             let oi2  =  head(ausw pw (concat(map (tmaps3 ) prep)))
              --               putStrLn (show oi2)
                             tmap3 <- forM [1..prepRyth] (\pi -> do
                                    let oiDo =  (tmaps3 pi)
                                    let oiDoform = ((concat (ausw pi ( head (ausw pw (map tmaps3 prep))))))
                                    return((oiDoform)))
 
                             let theInner = (concat (take 1 tmap3 )) 
                             return(theInner))
          
        --      putStrLn "below: sortEm ; inlinesortEm; "  -- 1
        --      putStrLn ((unlines (map show (concat (sortEm)))))-- 2 ----------------------------------- THOSE TWO LINES COULD BE MAIN OUTPUT
        --      putStrLn ((show inlinesortEm))
              let boneMax = ausw (maximum(concat(concat sortEm))) bonelist
        --      putStrLn (show boneMax)
              --run randomPunkt list with input:
 --e.g *> ghCheck = "AAAABBBB"    
 -- COMPARE BONELIST to INPUT:  (a'S of bonlist) to  ghCheck
              let ghCheck = concat (take prepRyth (repeat [crit]))
                            
-- COMPARE bonlist this time :  ghCheck to (a'S of bonelist )
--  =>  randPunktList2 /= randPunktList3 but randPunktist2 = (transpose randPunkList3)   
              randPunktList3 <- forM [1..(prepRyth)] (\z -> do
                        let chhos = ((ausw z (concat(snd(theTrix2 crit))))) 
                        let mapRepKEY aa = beRepKEYRAW aa (show z) bonelist ghCheck []
                        let forole = (prepRyth -1)
                        roleKEY <- forM [1..prepRyth] (\y -> do -----------------------------------------------------------------Export complete simiYmatrix as Strig
                                let gegenStueck = filter (/=z) [1..prepRyth]
                                let makShow aa = show(mapRepKEY aa)
                                let role = map makShow (map show gegenStueck)
                                return(role) )
                        roleKEY2 <- forM [1..prepRyth] (\y -> do ----------------------------------------------------------------Export as Double 
                                let gegenStueck = filter (/=z) [1..prepRyth]
                                let role = mapRepKEY(show y)
                                return(role) ) 
 
                        let sta1 =  roleKEY2 --(ausw 1 (concat (take 1 roleKEY2)))
                        let sta2 = sum sta1
                     --   putStrLn ((show sta1)++" "++((show sta2)))
                        return (sta1))
          --    putStrLn "sum matrix row" 
	 -- The input variable ghCheck is sorted into phiMax list.
	 -- ghCheck could end up in any spot of a list function 'sortEmInput' 
	 -- find max different of group 
	 -- ghCheck
	       
              let prepPhiorder = take 1 randPunktList3 
              
              sortEmInput <- forM [1..prepRyth] (\pr -> do
                      let withInput = (map realToFrac (concat prepPhiorder)) -- now we can use maximum again
                      let toStep e w = ( w `elemIndices` e)
                      let tmaps t w2 = map (toStep (sort (map fst randPunktList))) (ausw t w2)
                      let lineorder = last (last (tmaps pr (map fst randPunktList))) -- take phimax order
                      let accessSTEPI = (map snd randPunktList)
                      let foAddgh = head (sort accessSTEPI)
                      --(show (map last (concat sortEm))))  => [1,1,3,2] ; phiMax order
                      let phiMaxorder =  (map last (concat sortEm)) -- => [1,1,3,2] 
                      let phiMsorted i =  (head (ausw i (reverse (sort phiMaxorder)))) -- we want to reverse to count 'downwards' => [3,2,1,1] 
                --      let mapPhi = realToFrac (phiMsorted 1) --[1..prepRyth]
                      let tryOut  fo =  toStep  [((ausw (phiMsorted fo ) (concat prepPhiorder) ))] ((withInput))
                      runTriesRAW <- forM [1..(length phiMaxorder)] (\ol -> do
                              let try = ((ausw (phiMsorted ol) (concat prepPhiorder) )) -- => [[1.5151515151515151]]
                              let fotraceOther i = head(ausw i foAddgh)
                              let traceOther = map fotraceOther (reverse (sort phiMaxorder))
                              let cheGH = maximum (take ol (reverse(sort withInput)))
          -- see the difference between ( [phiMax] )  and   ( ghCheck compared [phiMax] ) 
                              let nowSort = (fotraceOther (phiMsorted ol)) + (head(map realToFrac (ausw (phiMsorted ol) (concat prepPhiorder))))
                              let applyAlgo = if (nowSort)>cheGH  then 
                                                  let ste1 = break (==cheGH) (map realToFrac (concat prepPhiorder)) 
                                                  in concat(((fst ste1)): [(nowSort:(snd ste1))]) --"right" --ste1
                                              else  (map realToFrac (concat prepPhiorder)) --"false"
                                     
                              return (applyAlgo,cheGH))  -- ( ( inserts ghCheck [phiMax]    ,(the changeable phiMax of bonelist))
                      let runTries = map fst runTriesRAW
                      let judgeTries = map length runTries
                      let filterMy = let ste1 = filter (==(maximum judgeTries)) judgeTries
                                     in let seeTries g k = ausw (phiMsorted g) (ausw k runTries)

                                     in if ste1 == [] then bonelist
                                        
                -- proposition : take 1 runTries = [[39.44954128440367,39.44954128440367,1.5151515151515151,41.88212399221574,39.63414634146342],
                --               if                                                             A         >       B
                --               then sort ghCheck right of A
                                        else let fobuildD = (take (phiMsorted 2) (map words bonelist))  --ghCheck
                                                     in let fobuildTail = (take ((-1*(phiMsorted 1))+(length bonelist)) (reverse ((map words bonelist))))
                                                     in let withGH = ("GGGG":(concat fobuildTail))
                                                     in let getMaxIn = (maximum (map snd runTriesRAW)) `elemIndices` (head runTries)
                                                     in let prepMax = zipWith (+) (take (length runTries) [1,1..]) getMaxIn
                                                     in let fobuildHead = take ((phiMsorted 1)-1) ( (map words bonelist)) 

                                    -- phiMsorted => [3,2,1,1] 
                                                        --  if bigger ordered to the right of max
                                                     in if (map realToFrac ((ausw ((phiMsorted 1)+1)(concat(take 1 runTries ))))) > ((map realToFrac (ausw ((phiMsorted 1))(concat(take 1 runTries ))))) 
                                                        then let fobuildD = (take (phiMsorted 1) [bonelist])  --ghCheck
                                                             in (concat fobuildHead)++boneMax++["right"]++(concat fobuildTail) 
                                                        else (concat fobuildHead)++["left"]++boneMax++(concat fobuildTail) 
                                        --else ausw 1 runTries
                --      putStrLn (show judgeTries)
                  --    putStrLn (show (runTries))
                    --  putStrLn ((show(map snd runTriesRAW)))
                                  
                      return ((filterMy,(runTries))) ) 
 
         --  e.g  ["AAABB","AABAB","AAA","BBBAA"]  -> ["AAABB"/"AAABAB","BBBAA","AAA"]  -> sortEm
--                                        -> ( [0,1],  [0,1],    [3],   [2]] ) -> 
--       => sortEM                           
              let findOrdeR = nub (concat(concat (sortEm)))      -- -> [
         --     putStrLn "FinD ORDER"
           --   putStrLn (show findOrdeR) 
               -- inp:[String] -> bonelist
       -- the lineorder can be funnelt into a (maybe mother)/= Nothing
       --  a motherType e.g simiSum Order 
              let toStep e w = ( w `elemIndices` e)
              let muster u = ausw u (sort(head inlinesortEm)) -- bonelist -> simiVals
              let withGHcheck = ((concat(map snd sortEmInput))) 
              let muster2 u = ausw u (sort(head withGHcheck)) -- ghCheck: bonelist -> simiVals
      --        putStrLn (show( muster2 1))
        --      putStrLn (show(muster 1))
            --  putStrLn (show inlinesortEm) --chainDistribute
       --       putStrLn "with GH" 
           --   putStrLn (show withGHcheck)
              let justIO = inlinesortEm
              let toIter foa = (Just(maybePu ((show [(foAdecide2 (foa))]))))
              let inMap = map toIter (map words justNames)
              let mayer3 r foa = if (foa) == [] then maybePu "empty"
                                else maybePu2 (r) (head (ausw 3 inMap))

        --      putStrLn "1" --(checkflow [] (toIter justNames))
              let justGene u =  map show(ausw u justIO)
-- A) comparing the phiMax line to an ordered  phi line: compare (simYval) to  (sort phiMax)
-- B  comparing phiMax line to the phiMin
-- C) compare phiMax line to another line

      -- CHOOSE A STRATEGY build on A
      -- choosen: 
      --  1. maximum -> step to next smaller atom ....  -- r shall be mapped
              let enduranceRace8a9_6_20 r u = show (map (toStep (muster u)) (concat (ausw r justIO))  ) -- stepI a. ([phiMax])
              let edR1 r u = enduranceRace8a9_6_20 r u 

    -- function below can crash the whole 'kArmTest5' the error handeling 
    -- can be done via a Punkt ?! ------------------------------------------------####################################################### 20-6-20 
    --                                                                                                               must have li order to work 
              let edRGH r u = show (map (toStep (muster2 u)) (concat(ausw r withGHcheck))  ) -- stepI b.  ghCheck: [phiMax]
            --  putStrLn (show(map (edRGH 1) [1..5]))
              let moreEdR1 r = map (edR1 r) [1..prepRyth]
              
 --           u: the normal order ?     
 --   r:Int ; which to take of sortEm to be mapped with [1..(length bonelist)]
            --    let motherTYPE o u r = map (mayer2 ((ausw r (edR1 r o )))) [(map (edR1 r) u )]
              let areplace r o = ((ausw r (edR1 r o )))
              let motherTYPE o u r = map (mayer2 (concat(ausw r (map show findOrdeR)))) [(map (edR1 r) u )]
              let motherTYPE2 o u r = map (mayer2 (concat(ausw r (map show findOrdeR)))) [(map (edRGH r) u )]

              let mapMo o r = unwords (checkflow [mother] (motherTYPE o [1..prepRyth] r))
              let mapMo2 o r = unwords (checkflow [mother] (motherTYPE2 o [1..prepRyth] r))
              let foexpoMo r w =  unlines (map (edR1 w ) r)
              --let innerLineMother = 
              let exportMother r = unlines(map (foexpoMo r ) r)
              let exportMother2 o = (words (concat(map (mapMo o) [2,3,4])))
              --let tryformation io1 io2 r = formationRAW io1 io2 r --bonelist [(exportMother [1..r])] io r 
          --    putStrLn "\n test mother functions"
             -- putStrLn (show (tryformation [mother] 1))
          --    putStrLn (show (map muster [1..4]))
          --    putStrLn (show (map muster2 [1..4]))
            --  putStrLn (unlines(checkflow [mother] (ausw 1 (motherType (justGene 3) (3)))))
   -- Punkt function:
   -- insert your desired mother type to any Punkt
   -- e.g *> let mymotherT03 t = (ausw 1 (motherType (justGene t) (t)))
   --     *> let accesMT03 t= unlines$checkflow [mother] mymotherT3$ t
   --   use function 'tester' below to accomplish that
              let tester t = (unlines(checkflow [mother] (ausw 1 (motherType (justGene t) (t)))))
              let motherType3 foas r = map (mayer3 (head(ausw r (map show justIO)))) ([foas])
          --    putStrLn (unlines(checkflow [] (ausw 1 (motherType3 (justGene 3) (3)))))
          --    putStrLn (tester 4)
             -- Plot-------------------------------------------------------------------
          -- STEP III. of program
              let foe t = head (ausw t bonelist)
          --    let op = atrix0a (foe 1) (foe 2) (foe 3) (foe 4) (foe 2) (foe 1) 1 1
                 ---------------------------------------------------------------------------------------------------
--
            --  (allFunctions (read fiIO) fiO2 (mayer2 "" [someCtrl]) [] "" (read fiselectLine))
              --(foFuncPair (read fiIO) fiO2 (mayer2 "" [someCtrl]) [] "" (read fiselectLine))
              let foFuncPairNEW o r  =  (  [mayer2 "otto" [(mapMo o r)] ] >>= (\x ->  [(mayer2 "King" [(show x ++"   " ++(mapMo2 o r))])]));
              let fmrTESTNEW io e e2 forLine =  checkflow  io [(Punkt  (head(checkflow [] [(basis4 e2 forLine)])) (Just (basis2 e 1 )) (Just (basis2 e 3 )) (Just (basis2 e 4 )) (Just (basis2 e 5 )) (Just (basis2 e 6))) ]
              let fmrTESTNEW2 io e e2 forLine =  (Punkt  (head(checkflow [] [(basis4 e2 forLine)])) (Just (basis2 e 1 )) (Just (basis2 e 3 )) (Just (basis2 e 4 )) (Just (basis2 e 5 )) (Just (basis2 e 6))) 
              let mapfoFuncNEW o = checkflow [] (concat(map (foFuncPairNEW o) (findOrdeR)))
                  -- plug Punkt of 'foFuncPairNEW' into variable Punkt arcitecture structure
                  -- from a [Punkt] take a Punkt and a [(Maybe Punkt)] see below 
                  --  [Punkt] -> Maybe Punkt ? -> String 
              let fmrTEST3RAWNEW io o r e2 forLine =  checkflow  io [(Punkt  (head(checkflow [] [(basis4 e2 forLine)])) (Just (head(foFuncPairNEW o r ))) Nothing Nothing Nothing Nothing )]
              let fmrTEST3NEW io o r forLine = ((fmrTEST3RAWNEW io o r (mapfoFuncNEW o) forLine), "specialSort can get missing part???")
              let allFUNC2RAWNEW io io2 forLine selectFunc = fmrTESTNEW io (ausw selectFunc [(exportMother forLine),unwords(map show(map snd randPunktList)),show(sortEm),show(fmrTEST3NEW io forLine (head forLine) (head forLine))])  (ausw selectFunc [(exportMother forLine),show(map snd randPunktList),(show sortEm),(snd (fmrTEST3NEW io2 forLine 1 (head forLine)))]) (head forLine) 
              let allFUNC2NEW io io2 forLine selectFunc = (allFUNC2RAWNEW io io2 [forLine] selectFunc) 
 -- show the given bonelist compared to adGH e.g. "ZZZ"  
            --  (map unwords[(allFUNC2NEW [mother] [mother] (1) (2)),((allFUNC2NEW [mother] [mother] (1) (3)))])
 -- same as above sorted 
--              putStrLn (unlines(allFUNC2NEW [mother] [mother] (1) (3)))
 -- the labels for comments of above
  --            putStrLn (unlines(allFUNC2NEW [mother] [mother] (1) (4)))
  --
   -- outcome1 : a [fractional]
              --(map fst randPunktList)
              ((map fst randPunktList),(randPunktList3) )
             -- (last(map snd randPunktList))  
           --   withGHcheck 
           --   putStrLn "1" --(foFuncPairNEW 1 1) 
                  
     (frame0 bonelist (mofaList) connectWrist dit dit2) 
--------------------------------------------------------------------


----------------------------------------------------------------------------------
--see the different states of flowState above
-- e.g checkflow [mother] (flowState fotrack3 head) -> the optimized OUTPUT row ala [0.52,0.52,0.59,0.52,0.59,0.59,0.59]
checkflow lis f = let ste1 lis f= publishPunkt f lis
                  in let ste2 = map (ste1 lis) f
                  in ste2

----------------------------------------------------------------------------------
--see the different states of flowState above
-- e.g checkflow [mother] (flowState fotrack3 head) -> the optimized OUTPUT row ala [0.52,0.52,0.59,0.52,0.59,0.59,0.59]
checkflowInt lis f = let ste1 lis f= publishPunktRAW f lis
                     in let ste2 = map (ste1 lis) f
                     in ste2 
 
    --putStrLn "Wrote Punkt: startWert"
---------------------------------------------------------------------------

frmDoubletoInt das = let st1 = map ord das
                 in let st2 = break (==46) st1 --(e.g [55,32,54] -> ([55],[32,54] ))
                 in let checkDigitBEFRkommata = length (fst st2) -- $break(==32)$map ord$das
                 in let cDBk = checkDigitBEFRkommata
                 in let toInt = map scanChar (show cDBk)
                 in if cDBk == 0 then map chr (filter (/=46) (snd st2))
                    else if cDBk == 1 then map chr ((fst st2))
                    else map chr ((fst st2)   )

auswW l t = (drop(l-1) (take l t))
-- CONNECT FROM KA-MASCHINE II
-- Aim II is to manipulate an SVG PATH READING from a whole path  (psyched!)

            
--           t :laengeset; c: 1 wird nur eingesetz um form s.o zu ersetzen
--           z: vorkommen des Wertes       
formWe2 t c z = 100/(t* (c/c)*(1/z))



--
--                e.gs
--            
--crit: String   "0.56" ??? 

-- e.g Colored*> let gh r = chainDistribute r ["2","2","1"] r (lines "1")
-- ARISING STRUCTURES
   -- KETTENVERTEILUNG aka CHAIN-DISTRIBUTION
       -- in which order do Values occur are they groupd and in which occurance 
       -- e.g Colored*> let example = ["A","A","B",BA",BA","AABB"..]
       --  [["A","A"],["B"],["BA",BA"]..]
       --  using alphabet A==1, B==2.. 
       --  -> [1,1],[2],[21,21]..]
chainDistribute crit dipfade2 criterium pt = let provideVals = [show(map ord crit)]
                         in let occurance k = k `elemIndices` dipfade2  
                      --   in let criteriums crit = crit `elemIndices` (dropWhile (\(val) -> val == crit ) ( dipfade2))
                         in let fodip4 =  (map ord (unwords dipfade2))
                              --  map ord : ['0'..'9' ] -> [48..57] 
               --  get all numbers in input string 
                      --   in let aFiltNum = (filter(<57) (filter (>48) (map ord ((unwords((concat(head cdMAP))))))   ))--(map ord proces)))
                --  get all capital letters in input string 
                       --  in let aFiltcapLETT = (filter(<90) (filter (>65)(map ord proces)))
                  --  get all small letters in input string 
                       --  in let aFiltsmaLETT = (filter(<122) (filter (>97)(map ord proces)))

                   --      in let fowere = map ord (concat dipfade2)
                         in let wereWHat  = ((head(map ord crit))) `elemIndices` (map ord (unwords dipfade2)) 
                         in let foZip =  take (length wereWHat) [1,1..]
                         in (occurance crit, [fodip4,(zipWith (+) foZip wereWHat)]) --fodip4 --group aFiltNum --roces --cdMAP --parsE aFilt --(map head (map words((concat(head cdMAP)))))


                  --       in let wereWHat  = ((head(map ord crit))) `elemIndices` (map ord (unwords dipfade2)) 
                    --     in wereWHat --fodip4 --group aFiltNum --roces --cdMAP --parsE aFilt --(map head (map words((concat(head cdMAP)))))



--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------- ######################################### CONTROLS: PRINTING 
-- EXPORT visiualize 
-- e.g 'M.writeWXCloudNODE (ptc0 3)(ptc1 3)(ptc2 3)(ptc3 3)(ptc 3) (ptc5)'
-- or  'M.writeWXCloud4 (ptc0 50)(ptc1 50)(ptc2 50)(ptc3 50)(ptc 50) (ptc5)'
-- let basis mm foA = basisRAW fnACCRAW nACCRAW (allAcc connectWrist) checkFo foA 

------------------------------------------------------------------------------------------------------------------------
basis2 foAL m = maybePu (head (ausw m foAL ))         -- ACCESS functions to any bonelist line 
basis3 foAL =  ((map ( (basis2 foAL)) [1..(length foAL)]))
basis2New foAL x m = maybePu (head (ausw m (foAL x))) 
-- Upload functions and apply them to the data
-- -- when buiding the final 'Punkt' structure these are different
-- computation that can be used with '[father,mother...' 
-- a) turn bonelist into Punkt
-- b) turn pg functions into Punkt via 'uniqueClassRAW' 
formationB e = Punkt "formation" (Just (basis2 e 1 )) (Just (basis2 e 2 )) (Just (basis2 e 3 )) (Just (basis2 e 4 )) (Just (basis2 e 5 )) 

-- store data in String because there are only 5 other spots left
-- when reading a longer list that wont help thus store in Punkt "String"
basis4 foAL r = maybePu (show(checkflow [] [((basis2 foAL r))]))
-- map befehle
-- befehle:  [(F.fourierMQ6NOPAN123),(F.fourierMQ5NOPAN123),(F.fourierMQ4NOPAN123),(F.fourierMQ4TRACE)] 

-- 20-6-20 Enter bonelist 
-- io:: [mother,father...   ; choose connect or not 
-- r :: Char ; which line of the  just connected Punkt to get. 
-- e :: bonelist ; a previous order 
-- normalFunctions :: the Punkt just connected 

runner x = do  --scanChar
        sin x 
    --     f <- (f getLine)    --head [(sin x),(cos x)])
      --   r <- (x)
        -- (return (do r)) --putStrLn (show e) --show(return e)
-- import your functions into kArmTest5 via formation
preX x = head(map scanChar (frmDoubletoInt (show x)))


seven= ["    - - -  \n"++
        "        -   \n"++
        "      - - - \n"++
        "        -  " ]   
      


--------------------------------------------------------------------------


--------------------------------------------------------------------------
-- Gaussian influenced - Aehnlichkeitswarscheinlichkeit
---this function rates the similarity of two
-- lists. 
-- gibt die Prozentzahl der Einerstelle 
-- der beiden Listen aus

foaehnli1 a b = (a-b)
foaehnli2 a b = (b-a)
--umrechnen in % mit a < b und b > a  
aehnlichF a b = let a1 = if a > b then ((foaehnli1 a b)/ (a/100) )
                         else if a<b then  ((foaehnli2 a b)/ (b/100) )
                         else 0
                in let b1 g h = ((g) / ((h)/100)) 
                in a1 

 
similaritYvalue li la = let a = la -- z.b. [11,12,34,44]
                        in let b = li 
                        in let c1 w = sum w
                        in let c2 = c1 a--length
                        in let c3 = c1 b--length
                        in  aehnlichF c2 c3 -- in let d = 




-- data structure StepIV
einTyp f = [((plugit f 2),(stelle2er f)),((plugit f 3),(stelle3er f)),((plugit f 5) ,(stelle5er f))] 

einTyp2 f = [((plugit2 f 2),(stelle2er f)),((plugit2 f 3),(stelle3er f)),((plugit2 f 5) ,(stelle5er f))] 
einTyp3 f g = [(plugit2 f g)]

-- root needed to set path to folder
-- change format for Linux
root = "c:/stack/SimilaritYWriter20/"
-- Global Variables
xXAllWal = "textS/dininSpez1kompl1.txt"
xXVals = "textS/aaKA30.txt"
xXSimus = "textS/aaWX30.txt"
ausw c t = drop (c-1) (take c t)


bogus = "Greenthump"
-- this builds our sheep family tree
-- HYRACHY IN DATA-TYPE
--          |ACCESS FUNCTIONS  
-- ___________|___________________________________________________________________________|
-- LEVEL 0   :|    nACC                                                                   |
-- LEVEL 1 in:|    nACCRAW -> fst -> tracker1                                             |
--            |            -> snd -> findArm,findHand                                     |
-- LEVEL 1    |     tracker1        |    findArm,findHand                                 |   
-- LEVEL 1 output =>fst: "tracker!" OR    snd:  ["findArm","hand"])                       |                      
-- LEVEL 2    |                     |    innerAccessRAW "1" this OR inner..RAW "2" this   |
-- LEVEL 2    |   e.g 
-- -- as INNERACCESS function retieving data of each single ACCESSFUNCTIONS          
-- within the string of a Punkt
-- e.g: *Main> Co.innerAccessRAW "2" (unwords ["findArm","hand\n"])
--      " hand\n"
innerAccessRAW on this = let convToBit = map ord this
                   in let chopUp1 = (break (<=32) convToBit)
                   in let chopUp2 = (break (>=32) convToBit)

                   in if on=="1" then 
                        --let theFst = fst (break (>32) ((fst chopUp1)))	
                        map chr (fst chopUp1)
                      else
                        (map chr (snd chopUp1))

innerAccess on this = (innerAccessRAW on) this
----------------------------------------------------------------------
-----------------------------------------------------------------------
--fill 'AccesFuctions' with data
-- theName: String; Fill AccesFuctions with Data
-- set to show tracker1 see below
nameACCESSFUNCTION on theName input= let whichAccesFunction = (innerAccess on theName)
                            in let theData = whichAccesFunction++" "++(unlines input)
                            in theData


--short
nACC theName input = (nameACCESSFUNCTION "1" theName input)
nACCRAW tracker1 input = break (==' ') (nACC tracker1 input)
--nACCTAG =  
---------------------------------------------------------------------
----ACCESS FUNCTIONS-------------------------------------------------
-- getFunctionByName "father" = father
-- getFunctionByName "mother" = mother
-- getFunctionByName "mother2" = mother2
-- getFunctionByName "loopNumber" = loopNumber
-- getFunctionByName "minMaxTrueOrFalse" = minMaxTrueOrFalse
accesFWX l nlist = let aw1 n = (ausw n nlist)
                 in let wielanGg  = [1..l]
                 in  ([wielanGg ,[(l+1)..(l*2) ]])


checkType ffi = [(map length (group(sort(ffi)))), (ffi)]

    --            in let f = ((minimum inpu) `elemIndices` (inpu)) 
      --          in inpu --f 
-----------------------------------------------------------------
-----------------------------------------------------------------
--list for accesFuncWX below
-- exports:  [(filtern b a),(filtern b c)]
-- die (simulierten Vals, einen zweiten stream )
-- DEFINE WX MAXIMA kann viele streams in einem
-- Koordinatensystem plotten 
-- wird in 
outPutMaxima333 goghi dipfa fodp1 fodp2 bob = 
                let a =  (concat goghi) -- quelle simulierte vals
                    b = (map show bob) -- laenge val liste 
                    c = (concat dipfa)
                    d = (concat fodp1)
                    e = (concat fodp2)
                    kerry x y = zipWith (\x y -> [x,y]) x y -- a-- (zip a b)
                    fofiltern x y= show (kerry x y)
                    filtern  x y = let tussenstap  = map ord (fofiltern x y)
                                       tss2 = filter (/=34) tussenstap
                                   in map chr (tss2)
                in [(filtern b a),(filtern b c),(filtern b d),(filtern b e)]
-----------------------------------------------------------------
-----------------------------------------------------------------
--- Extraordinary function chooses which AND howmany functions of the list above will be put in the output
-- l: [Int] ~ oder auch welche kombination von funktionen
-- bob laenge val liste
-- ghogi quelle1 simukieter val
-- dipfa quelle2 
accesFuncWX33 l goghi dipfa fodp1 fodp2 bob =
                  let aw1 n = concat(F.chooseMQ n (outPutMaxima333 goghi dipfa fodp1 fodp2 bob))
                  in let wielanGg  = [1..l]
                  in let aw2 = map aw1 wielanGg
                  in let foaw1 = show bob
                  in let enExp a b sqale2 = (M.aCompleteWX3 a b foaw1 sqale2) -- diese display nach compiliren  vs aCompleteWX2 schreibt display in file     
                     --  in let aw3 =  ceiling (l/2)	
                  in let aw4 = wielanGg --([wielanGg ,[(l+1)..(l*2) ]])
                  in let aw5 = "0.0" 
                  in let aw6 = "100"              
                  in enExp aw2 aw4 aw6 --enExp


         
scanChar :: Char -> Int

scanChar c | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
           | otherwise = -1

--scanString :: String -> Int a -> Double 
scanString = let ste = (go 0) 
             in zipWith (/) [1] 
    where go a [] = a
          go a (x:xs) | 0 <= sc && sc <= 9 = go (10*a+sc) xs
                      | otherwise = 0
              where sc = scanChar x
	 


---------------------------------------------------------------------------
-- transform any PUNKT conctruction into a string
-- Punkt a -> String a -> show a 
--architect: Punkt ; a whole Overview of the DataType tree with all its Branches
-- searchTrail: ACCESFUNCTIONS e.g. [mother,mother,father] 
publishPunkt architect searchTrail = let firstAccess = mysee architect searchTrail 
               in let ste1 = map ord (show firstAccess)
               in let breakCriteria = (32 `elemIndices` ste1)
     -- leersteln komme nur vor wenn ein term wie "Just blue" appears 
     -- this in order ot get rid of the Just
               in let findeLeerstelln = snd (break (==32) ste1)
               in let seeIfBreaksNeeded = if (length breakCriteria<=0) then ste1
                                          else findeLeerstelln 
               in let ste2 = filter (/=34) seeIfBreaksNeeded
               in  let mapToChar = map chr ste2
               in mapToChar
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- multipliziert Zahlen damit Sie in das SpurenSystem passen
-- Vorsicht nur bei Aehnlichen z.b. 0.59 0.94 0.22 ....
--  nicht 0.45 3.4 ....
getRid x = let a = (map ord x)
           in let b = map chr (filter (/=46) ( snd (break (<=46) a)))
           in let c = b 
           in c

--------------------------------------------------------------------------






