-- this module provides:
-- --------------------------------------
-- 9-6-2020
-- Standing true to my premise after having worked out a methodoogy 
-- of simiYval networks there shall be two giants to step on 
-- 1. What are the Eulercaracteristics of the graphs dessribed by kArmTest5
-- 2. What is the dimensionality with regard to roughness(Bernstein??)
-- I. the PUNKT TYPE 
-- can store data in:
-- Monadic non-dterministic DATA tye 
-- ----------------------------------
-- e.g let ones = Punkt (nACC "tracker1" ["FindMovment","what"]) Nothing Nothing Nothing Nothing Nothing
-- 
--  nACC nACCRAW, checkflow as main access functions
--
--  DATA rating: using the 12.6.20 version with similariTYvalue plus its source functions
--
--    into 'kArmTest5'
--
---
-- VIEW all concepts in Colored_2_3_5_Counter.hs                                       --

module Colored_2_3_5_Counter20 (
      einTyp
    , getRid --t transorming input vals 0,01 ..0,99
    , Punkt
   -- , mutateSVG1
    , nACC -- :115
    , nACCRAW -- same as above + extra variable: 'on' ; decide tracker1 
    , father
    , mother
    , mother2
    , loopNumber
    , minMaxTrueOrFalse
   -- , processFst
    , publishPunkt
-- functions related to interconnectivity between modules
    , checkflow
  --  , flowState
  --  , bewertung 
    , Punkt -- a data type
   -- , playingFi2 -- the first Main TEST of a quick svg path simulator 
   -- , trackDiagram
  --  , trackBody -- experimantal DATA Punkt sort
    , innerAccess
    , innerAccessRAW
   -- , trackPoint
   -- , trackPoint2
    , similaritYvalue
   -- naiveSim
    , kArmTest5 -- new Main 
      ) where

import Data.List
import Data.Char
import Control.Monad
import System.Environment
import System.IO
--import Data.ByteString.Lazy
--import qualified Data.ByteString.Read as W hiding (writeFile,readFile, putstrLn,read,map,unlines,lines)

import UsefulFunctions19
import DataTypePunkt
import qualified Path2Val2 as HT
import qualified FourierFieldCLASSIFIER as F
import qualified WriteWXmaximaJuicy as M

     --   " Truth (Wahrheit) over certainty (Gewissheit)"
--                  (Illobrandt von Ludwiger)
--   " Lets start a framework that derives truth from certainty, 
--     based on truth"
---- \
---- /  truth -> certainty -> truth
---- glossary ideas: the colores
--     iPunkts:= imaginary Punkts used for own reasoning
--
--       - one of infinity?
--     1. layer of concept: The Haskell string below is not easily understood 
--                          
--inner lexicon :
-- similaritYvalue :: (Fractional p, Ord p, Foldable t1, Foldable t2) => t2 p -> t1 p -> p
--  
--        to describe certain Types we wil use ':=' 
--        which is equivalent to '::' 
--        but ':=' /= '::' thus
--
--simiYvals = [similaritYvalue] := pick1 a [list] -> pick2 a [list] -> compare (pick1 a) to (pick2 a)   
--        there is an equivalent unconsistent (not relyibe) way to describe
--                          the ineherit concept of this project.    
--                          e.g Colored*> 
--

------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- STEP 1: Examine
-- There is an syntactic Level and a conceptual Level
-- ===============================================================================================================================================
--                                       SYNTAX                  |             CONCEPT
-- ===============================================================================================================================================
--  hints:                 all Functions in 'e.g-sections' have  |  The 'e.g-sections' should be able to be called via ghc
--                         to be take out of the do functions    |  without a doupt that helps human understanding
--                         in 'chainDistribute'    AND         |  see if atom is already in list , dile:
--                              'kArmTest5'                      |
-----------------------------------------------------------------
--  given:  row-width (length atom bonelist, 'zeilenlaenge )   
--                 pick1:String; a of (a,b) -> compare a to b    | one feasible way is to 'weigh' Strings is with simiaritYvalue^
--                 pick2:String; b of (a,b) -> compare a to b    | The aim is to rerieve a reliable function that will destinct between
--                                                               | any two strings. 
--
--          via ( beRepKEY pick1 pick2 punktList )               |           punktist :: Punkt -> [Punkt] -> not used
--
--                                                                           ausw pick1 bonelist ->  atombonelist       
--
-- e.g Colored*>  ( beRepKEY "1" "2" [] )           
--
--


-- HYRACHY IN 'kArmTest5' 'Punkt'DATA-TYPE
--          |ACCESS FUNCTIONS  
-- ___________|___________________________________________________________________________|
-- LEVEL 0   :|    nACC                                                                   |
-- LEVEL 1 in:|    nACCRAW -> fst -> "M"                                                  |
--            |            -> snd -> "notM"                                               |
-- LEVEL 1    |     tracker1        |    findArm,findHand                                 |   
-- LEVEL 1 output =>fst: "tracker!" OR    snd:  ["findArm","hand"])                       |                      
-- LEVEL 2    |                     |    innerAccessRAW "1" this OR inner..RAW "2" this   |
-- LEVEL 2    |   e.g 
--
-- FUNCTION IN 'kArmTest5'                             'kArmTest5'
-- ACCESS FUNCTIONS  
-- ___________|___________________________________________________________________________|
-- LEVEL 0   :|   [A]       [B]                                A             B           |
-- LEVEL 1 in:|   ideal     guess                             ideal       guess
--            |   [a]    ->   b                                [a]
-- LEVEL 1    |    =>  similar in [a]  |    findArm,findHand                                 |   
-- LEVEL 1 output  =>  simi [A] ~ b                      |                      
-- LEVEL 2    |                     |    innerAccessRAW "1" this OR inner..RAW "2" this   |
-- LEVEL 2    |   e.g 
--
  
 -- "The real power of cathegory-theory is kind of to isolate what you care about...
 --  a group of homomorphisms is always a function between sets..."
 --  Fabrizio Romano Genovese 
 --
 --e.g *> let li = ["AAABB","AABAB","AAA","BBBAA"]
 --       let pi = Punkt "M" Nothing Nothing Nothing Nothing Nothing
 --    *  let kArmTest5 li 1 pi 1 1 [] "AAA"
kArmTest5 bonelist mofaList connectWrist dit dit2 mCommand crit= do
     let allAcc p = show(checkflow [] [p])           
     
     let allAcc foPun =  (checkflow [] [(foPun)])
     let checkFo g = if (length g) == 0 then ""
                     else "MOTHER MODE: on"  
     let fnACCOUT cou = if unPoint == ("\"notM\"") then unwords [""] -- cou
                        else snd cou
                    where unPoint = (show(head(words(unwords(checkflow [] [connectWrist]))))) ;

     let fnACCRAW cou = if unPoint == ("\"notM\"") then fst cou
                        else snd cou
                    where 
             unPoint = (show(head(words(unwords(checkflow [] [connectWrist]))))) ;
     let maybePu rt = Punkt rt Nothing Nothing Nothing Nothing Nothing
 -- Punkt function that inherits ancestory
 -- *> type: maybePu:: String -> Maybe Punkt -> Punkt
     let maybePu2 rt koA = Punkt rt koA Nothing Nothing Nothing Nothing
     let foAdecide foA = if foA==[] then Nothing
                         else (Just (maybePu foA)) --let whereBreak = chainDistribute crit bonelist crit (lines "1")

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
                          else let chssd i = maybePu2 (head(ausw i foA))  (((boa (head(ausw i foA)) (head(ausw i eindelijk))))) -- (Just (maybePu (unwords(checkflow [] eindelijk)))) 
                               in Just (show[(chssd 1)]) 
 
     let basis mm foA = Punkt (fnACCRAW(nACCRAW (unwords(allAcc connectWrist)) ["When set M will work:"++" now sleeping", checkFo mm ] ) ) foA foA foA foA foA
 {-
     let mixUpTray0 showClass pushTPosition fiPalette testData = sirRetrun --beRep1 (read showClass) (show(words(unwords testData))) -- (sirRetrun oriPosition pushTPosition)
                         where
                           palette = fiPalette -- [bone1,foExp,(show(makeBreak sl)),anEx,(show aRate)] ;
                           goodChoice doer = head (ausw doer palette);
                           beRep1 doer showData = (Punkt (fnACCRAW (nACCRAW (showData) [(showClass),(goodChoice doer),"YES MONAD:_"++show dit])) (Just pushTPosition) Nothing Nothing Nothing Nothing);
                               -- testData =  ((map snd (map snd (head commaBreakGuess)))) ;
                           sirRetrun =  
                                  let ans showClass = if showClass==bone1 || showClass=="5"
                                                      then do 
                                                         (beRep1 5 (show testData))
                                                      else if showClass=="1" || showClass=="2" || showClass=="3" || showClass=="4"
                                                      then do
                                                          let reAd = read showClass
                                                          (beRep1 reAd (show testData))
                                                      else
                                                          pushTPosition
                                  in ans showClass; -}
    -- take from deze [String] e.g -> 
     let foChain = length bonelist
     let makePalette pick1 pick2 punktList togoToList togtoList2  = noSense togoToList togtoList2
                       where
                          dada fopick fodeze = head (ausw (read fopick) fodeze);
                          noSense deze deze2 = Punkt (dada pick1 deze) (Just (maybePu (dada pick2 deze2))) Nothing Nothing Nothing Nothing;
     let gaussRate eins zuI = similaritYvalue eins zuI
 -- ############################# The gaussian Rate is wrapped into two functions 
   -- beRepKEYRAW ; pick1 value of onelist and compare it to an snd pick2 of another list
   -- There is an allAcc function without any purpose that could be used lateron
    -- Rate any two things bone1 with 
    -- ---------------------------------------------------------------------------------
     let beRepKEYRAW pick1 pick2 onelist twoList punktList = (crunchtoSimi pick2) -- makePalette pick1 pick2 punktList commands
                      where
                  --      commands =   [bone1,bone2,bone3,bone4,bone5]; 
                        compar = head (ausw (read pick2) twoList);
                        paletteKEY fpic2 twoList astrList = makePalette pick1 fpic2 astrList onelist twoList;
                        cleanPaletteRaw fpic2 twoList astrList = allAcc (paletteKEY fpic2 twoList astrList );  -- go to pick2 of list goTo
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
--   STEP 2)    chainDistribution   Choose (if bonelist>1                   |    Punkt (simiYval a b )

--                      e.g ..*> let gh = chainDistribute r ["2","2","1"] r (lines "1")
--                              
--                            *> (beRepKEY pick1 pick2 punktList) 


--   STEP 3)    chainDistribute ->  OCCURANCE   ->  randPunktList -> sortEm -> 
--              => edR1                             | the function to be mapped in motherTYPE
--              => motherTYPE                       | a f
--              *> let edR1 r u = enduranceRace8a9_6_20 r u 
--
--               u: the normal order ?  
--              *> let motherTYPE o u r = map (mayer2 ((ausw r (edR1 r o )))) [(map (edR1 r) u )]

--       --                            
---------------------------------------------------------------------------------------------------------------------------------------------------------------------

---------------------------------------------------------------------------------------------------------------------------------------------------------------------
--STEP 2: CHAIN DISTRIBUTION
-- rate with simiYvalue to compare 
-- all atoms of all lines to the same metric
--  read         complement    rate with                   
--  bonelist      fst ..last    simiYvalue              
--
-- a a a a       a1 a2 a3      a a a     a01 a01 a01           |a0 b0 c0|           b0 a0 c0     
-- b b b b  --\  b0 b2 b3 --\  b b b --\ a01 a01 a01 --\ MONAD |a1 b1 c2| --\ MAYBE a1 c1 1b --\
-- c c c c  --/  c0 c1 c3 --/  c c c --/ a3  a3  a3  --/       |a2 b1 c3| --/  e.g  b2 c2 a2 --/
-- d d d d       d0 d1 d2      d d d     a2  a2  a2            |a4 b4 c4|           c3 a3 b2 
--
--           |a0 b0 c0|           b0 a0 c0     
-- --\ MONAD |a1 b1 c2| --\ MAYBE a1 c1 1b --\
-- --/       |a2 b1 c3| --/  e.g  b2 c2 a2 --/

--
--
-- KETTENVERTEILUNG aka CHAIN-DISTRIBUTION                                   |  -- in which order do Values occur are they groupd and in which occurance 
--e.g *> let proposition = "AABBBB"                                          |  -- a proposion is a string that will probe the bonelist
--    *> let bonelist =["AAABB","AABAB","AABB","BBBAA"]                       |
--    *> let fochain = group  (map ord(unwords bonelist))                                        
-- [[65,65,65],[66,66],[32],[65,65],[66],[65],[66],[32],[65,65],[66,66],[32],[66,66,66],[65,65]]| ::= ord  [["A","A"],["B","B"],["BA",BA"]..]
--    
--     OCCURANCE OF ALL DIGITS
--    *> let occORD = map ord fochain                                        | -bonelist in ord, 
--    *> [65,65,65,66,66,32,65,65,66,65,66,32,65,65,66,66,32,66,66,66,65,65] | all needed data chain distributions can be found in this format
--                                                                         
--
--     OCCURRANCE POSITION
--    *> let occPOS= proposition `elemIndices` bonelist                
--          | if [] not occure 
--          | else e.g [0,1]
--
--
--     OCCURANCE PROPOSITION                                     | in all ATOMS of all lines of bonelist
--
--    *> let gh r = chainDistribute r bonelist r (lines "1")
--    *> gh "AAABB"
--    *> ([3],[[65,65,65,66,66,32,65,65,66,65,66,32,65,65,66,66,32,66,66,66,65,65],[4,5,9,11,15,16,18,19,20]])

--            dataFormat :==   ( occurance position,  map ord input ,    occurance fst atom ( "A" of "AABBB" -> [1,2])   )
--                       :== (  [occurance posi.] , [[ map ord input] , [occur.proposi.]]   )
--
              let theTrix crit = chainDistribute crit bonelist crit (lines "1")
              -- ACCESS functions:
              let foinnerOrd = head(snd (theTrix crit))
              let foinner = last (snd(theTrix crit))
              let rythm = (32) `elemIndices` (foinnerOrd)
              let prepRyth = if head rythm ==0 then length rythm
                             else (length rythm)+1

              let sortWith aa bb = let fuLength = length foinnerOrd
                             in let dooer aa bb = beRepKEY aa bb []  
                             in dooer aa bb --show fuLength

              putStrLn (show (beRepKEY (show dit) (show dit2) []))

              putStrLn ((show rythm) ++ " break positions in whole bonelist") 

-- COMBINE 'mapRepKEY' with ' chainDistribution '
-- e.g    fst 'randPunktList' := (compare pick1) [pick2] -> [compare[pick1..pick2]]
--            => roleKEY  -> [[String]] 
--            => roleKEY2 -> [[Double]] -> sum roleKEY2 ->
--            -> roleKEY2 := in a line of bonelist -> turn every (String a) into (Double a) ->
--                   -> (with simiYval a) -> [(simiVal a)]
-- =>  everyLineOrder
--
--                    |  every line has its inner order 
--                    |        I.         |      II.
--                    |   map ord line    |    simiVal a b                 |
--                    | map ord [a0..c0]  | simiVal a0 ([a1..3] )          |
--                                        | simiVal a1 ([a0..a3] \\ a1)    |
--                                        | simiVal a2 ([a0..a3] \\ a2)    |
--                                        | simiVal a3 ([a0..a2] )         |
--                    |   work 'bonelist' |    NOT USED                    |
--                       ===============      =========
-- =>  boneListOrder
--
--                    |  every boneList has its inner order 
--                    |        I.         |      II.
--                    |   map ord line    |    simiVal a b                 |
--                    | map ord [a0..c0]  | sim0 = simiVal a0 ([a1..3] )          | [0.0,0.0,19.877675840978593,0.3048780487804878]
--                                        | sim1 = simiVal a1 ([a0..a3] \\ a1)    | [0.0,0.0,19.877675840978593,0.3048780487804878]
--                                        | sim2 = simiVal a2 ([a0..a3] \\ a2)    | [19.877675840978593,19.877675840978593,0.0,20.121951219512198]
--                                        | sim3 = simiVal a3 ([a0..a2] )         | [0.3048780487804878,0.3048780487804878,20.121951219512198,0.0]
--                  'chainDistribution'   | simSums = [(sum sim0)..(sum sim3)]    | [20.18255388975908,20.18255388975908,59.877302901469385,20.731707317073173]
--                  ===================   |  maximum simSums                      |  show most differing order vs minimum most similar
--                                        |   'randPunktList'                     |
--                                            =================
-- ---------------------------------------------------------------------------
--                                         

              randPunktList <- forM [1..(prepRyth)] (\z -> do
                        let chhos = ((ausw z (concat(snd(theTrix crit))))) 
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
 -- Where are atoms on :  phiMax :: [Double] -> max ([(sum[Double])] ->[Double]   
 --                       phiMax = max (sum simYval )               |  calculating the most different line due to simiVal
 --                       also simYval :: [Double]
 --foas:     | u:[Doulbe] -> WHEREELEMENT  ------------------------------------------------------------------------------------------"THE FIRST ORDER :)"
 --                          The where element rpresent is the first step after choosing atrategy. In this case amximm is the strategy so we count down.
 --                          We know the limit of how many steps to go is the length of bonelist  
 --                           n -1 or +1 
 --                         depeding if we introduce a new atom
 --                          into the list of all atoms , inother words if the value we are comparing to is element of the list we would ony need to go  
 --                          n-1 
 --                          steps and vice versa. 
 --                          strategy is being put
 --                          frequenty into a fold . In other words if we start with phiMax (the maximum simval line of bonelist)
 --                          the next smaller eement is being found via all those `elemIndices` functions.
 --                          Important is the order that emerggece

 --   two new strategies arise: 
 --                     
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
                      
              sortAncestor <- forM [1..prepRyth] (\pr -> do
                      let lineorder =  nub(concat(concat sortEm))
                      let prep = zipWith (+) (take (length lineorder) [1,1..]) (lineorder)

                      let toStep e w = ( w `elemIndices` e)
                      let holdOn t = concat (ausw t (inlinesortEm)) 
                      let tmaps t v = map (toStep (ausw t [holdOn pr])) (ausw t v)
                      let lineorder t = tmaps t [(holdOn pr)] --tmaps pr 
                      putStrLn "testin"
                      return (show (holdOn pr)))
              putStrLn "below: sortEm ; inlinesortEm; sortAncestors" 
              putStrLn ((unlines (map show (concat (sortEm)))))
              putStrLn ((show inlinesortEm))
              putStrLn ""
              putStrLn ((show sortAncestor))
               

----------------------------------------------------------------------------------

----------------------------------------------------------------------------------
--MAP ANCESTORY     start with the pre-ordered phiMax 
--  e.g  ["AAABB","AABAB","AAA","BBBAA"]  -> ["AAABB"/"AAABAB","BBBAA","AAA"]  -> sortEm
--                                        -> ( [0,1],  [0,1],    [3],   [2]] ) -> 
--       => sortEM                           
              let findOrdeR = nub (concat(concat (sortEm)))       
              let mayer2 r foa = if (foa) == [] then maybePu "empty"
                                else maybePu2 (r) (Just(maybePu ((show [(foAdecide2 (foa))]))))
               -- inp:[String] -> bonelist
       -- the lineorder can be funnelt into a (maybe mother)/= Nothing
       --  a motherType e.g simiSum Order 
              let justNames = ["name1","name2","name3"]
              let toStep e w = ( w `elemIndices` e)
              let muster u = ausw u (sort(head inlinesortEm))
              let justIO = inlinesortEm
              let toIter foa = (Just(maybePu ((show [(foAdecide2 (foa))]))))
              let inMap = map toIter (map words justNames)
              let mayer3 r foa = if (foa) == [] then maybePu "empty"
                                else maybePu2 (r) (head (ausw 3 inMap))

              putStrLn "1" --(checkflow [] (toIter justNames))
              let justGene u =  map show(ausw u justIO)
              let motherType foas r = map (mayer2 (head(ausw r (justNames)))) ([foas])
-- A) comparing the phiMax line to an ordered  phi line: compare (simYval) to  (sort phiMax)
-- B  comparing phiMax line to the phiMin
-- C) compare phiMax line to another line

      -- CHOOSE A STRATEGY build on A
      -- choosen: 
      --  1. maximum -> step to next smaller atom ....  -- r shall be mapped
              let enduranceRace8a9_6_20 r u = show (map (toStep (muster u)) (concat (ausw r justIO))  )
              let edR1 r u = enduranceRace8a9_6_20 r u 

 --           u: the normal order ?     
 --   r:Int ; which to take of sortEm to be mapped with [1..(length bonelist)]
            --    let motherTYPE o u r = map (mayer2 ((ausw r (edR1 r o )))) [(map (edR1 r) u )]
              let areplace r o = ((ausw r (edR1 r o )))
              let motherTYPE o u r = map (mayer2 ((ausw r (edR1 r o )))) [(map (edR1 r) u )]

              let mapMo o r = unwords (checkflow [mother] (motherTYPE o [1..prepRyth] r))
              putStrLn "\n test mother functions"
              putStrLn (unlines(checkflow [mother] (ausw 1 (motherType (justGene 3) (3)))))
              let tester t = (unlines(checkflow [mother] (ausw 1 (motherType (justGene t) (t)))))
              let motherType3 foas r = map (mayer3 (head(ausw r (map show justIO)))) ([foas])
              putStrLn (unlines(checkflow [mother] (ausw 1 (motherType3 (justGene 3) (3)))))
              putStrLn (tester 4)
              putStrLn (edR1 1 1)
              putStrLn (edR1 1 2)
              putStrLn (edR1 1 3)
              putStrLn (edR1 1 4)
              putStrLn (edR1 2 1)

              putStrLn (edR1 2 2)
              putStrLn (edR1 2 3)
              putStrLn (edR1 2 4)
              putStrLn (edR1 3 1)
              putStrLn (edR1 3 2)
              --putStrLn (newToStep 2 3)
          -- EMERGENT structure
          -- [Just \[\\\[40.36697247706422,40.36697247706422,0.0,40.54878048780488]\\\]\]  THE REAl 3 but Rated 4 
          --
    --           [[ ],[ ],[0],[]]     
    --           [[ ],[ ],[0],[]]
    --           [[ ],[ ],[ ],[]]
    --           [[0],[0],[ ],[]]
              putStrLn (edR1 3 4)
              putStrLn (edR1 4 1)
              putStrLn (edR1 4 2)
              putStrLn (edR1 4 3)
              putStrLn (edR1 4 4)
              putStrLn "Test map mother"
              putStrLn (mapMo 1 1)
--              putStrLn (mapMo 2)
  --            putStrLn (mapMo 3)
    --          putStrLn (mapMo 4)
      --        putStrLn (mapMo 5)
        --      putStrLn (mapMo 6)
          --    putStrLn (mapMo 7)
            --  putStrLn (mapMo 8)

             -- putStrLn (newToStep 2 4)
             --
            -- ARISING: 
            -- a line that yields [[0],[0],[0],[0],[0]
            -- There might be regression in the functions that+
            -- comes out of above. -the phiMax strategy , starting from max simiYalist counting downwards
            -- looking of te overal destribtions of zeros it seems that dpends argey of the mentioned function !? 
          -- Plot-------------------------------------------------------------------
          -- 3. Part of program
              let foe t = head (ausw t bonelist)
          --    let op = atrix0a (foe 1) (foe 2) (foe 3) (foe 4) (foe 2) (foe 1) 1 1
              putStrLn "readY to plot"
              let pop = (ptc0  5)
              --M.writeWXCloudNODE (pop) (ptc2 5) (ptc3 5) (ptc4 5) (ptc5 5) (ptc6 5)
              M.writeWXCloudNODE (ptc2 5) (ptc2 15) (ptc2 25) (ptc4 2) (ptc4 3) (ptc4 5)
              M.writeWXCloud4 (ptc2 5) (ptc2 25) (ptc2 50) (ptc4 5) (ptc4 25) (ptc4 50)
              putStrLn "1" --(show pop) --(pop (head(map ord "1")))  ---------------------------------------------------------------------------------
 --run randomPunkt list with input:
 --e.g *> ghCheck = "AAAABBBB"    
 -- COMPARE BONELIST to INPUT:  (a'S of bonlist) to  ghCheck
              let ghCheck = concat (take prepRyth (repeat [crit]))
              randPunktList2 <- forM [1..(prepRyth)] (\z -> do
                        let chhos = ((ausw z (concat(snd(theTrix crit))))) 
                        let mapRepKEY aa = beRepKEYRAW aa (show z) ghCheck bonelist []
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
                        return (sta2))
              
              putStrLn (show (randPunktList2)++"\n")
-- COMPARE bonlist thistime :  ghCheck to (a'S of bonelist )
--
--  =>  randPunktList2 /= randPunktList2 but randPunktist2 = (transpose randPunkList3)   
              randPunktList3 <- forM [1..(prepRyth)] (\z -> do
                        let chhos = ((ausw z (concat(snd(theTrix crit))))) 
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
                        return (sta2))
              putStrLn "sum matrix row" 
              putStrLn (show (randPunktList3))
----------------------------------------------------------------------------------------------------------
--
---------------------------------------------------------------------------------------------------------
              let mayer foa = if foa == [] then [""]
                              else lines(show [(foAdecide2 foa)])


              let asDot inp = ((map mayer (map words inp)))

 -- RETRIEVE the DATA turn any 
 -- type:  [Maybe Punkt] -> String
 -- with brute force.
              let justGoneRAW t som = let prep1 = ((map ord (concat(concat ((ausw t som))))))
         -- take always second
                             in let prep2 = head (ausw 2 ((ord '[') `elemIndices` (prep1)))
         -- take always second , fixed
                             in let prep3 = head (ausw 2 ((ord ']') `elemIndices` (prep1)))

                             in let prep4 = drop prep2 (take prep3 (unlines(concat ((ausw t som))))) 
                             in let filAll = filter (/=92) (filter (/=34) (filter (/=93) (filter (/=91) (map ord prep4)))) 
                             in map chr filAll
              let justGone t = justGoneRAW t (asDot bonelist) 
              putStrLn (show (justGone 2))
              putStrLn "furter"

              putStrLn (justGoneRAW 1 ([(map tester  [1..4])])) 
              let pointeMA w2 w = Punkt (show(justGone w)) (Just(maybePu(show(justGone ((w2)-1))))) Nothing Nothing Nothing Nothing
              putStrLn (unlines (checkflow [] ([pointeMA 3 2])))
              let sortPunkts w2 =  map (pointeMA w2) [1..(length bonelist)]
              let pointeMA2 w2 w = Punkt (show(justGone w)) (Just(maybePu(show(justGone ((w2)-1))))) Nothing Nothing Nothing Nothing
              let sortPunkts2 w2 =  map (pointeMA2 w2) [1..(length bonelist)]
              putStrLn (unlines(concat (asDot bonelist)))
              let hiveList = let ste1 w = (Just(maybePu(show(justGone (w-1)))))
                             in map ste1 [1..(length bonelist)] 
              putStrLn "TEST: 9.6.20   ; not finished"
              putStrLn (head(ausw 3 (checkflow [mother] (sortPunkts 3))))
              let raw w =  (head((checkflow [mother] (w))))
              let fromOccur =  (((concat(sortEm)))) 
              let checLength z = concat(ausw z fromOccur)

              let outputPunktOrder z =  let innerMap z = let innerOrder2 z b =  ausw b (checLength z)
                                                         in let innerLengt z = [1..(length(checLength z))]
                                                         in let withRaw z = map (innerOrder2 z) (innerLengt z) 
                                                         in let apointer w2 = (pointeMA2 w2) (w2+1)
                                                         in
                                                         if (length(checLength z))>1 then 
                                                              map apointer (concat (withRaw z)) -- (pointeMA2 (withRaw z --map withRaw (concat(sortPunkts z)) --((map sortPunkts2 (innerLengt z))) --map raw [1,2] --(concat(withRaw z))
                                                         else  map apointer (concat (withRaw z)) --map withRaw (concat(sortPunkts z)) --withRaw z-- ((map sortPunkts2 (checLength z)))
                                        in (innerMap z) --sortPunkts z -- map raw (innerMap z) --innerMap 2 --map innerMap [1..(length fromOccur)] --innerMap z
             -- putStrLn (head(ausw 3 (checkflow [mother] (hiveList))))
              putStrLn (show (outputPunktOrder 3))

              allTogether <- forM [1..(prepRyth)] (\z -> do
                        let chhos = ((ausw z (concat(snd(theTrix crit))))) 
                        let mapRepKEYI aa = (sortWith aa) (show z)
                        let forole = (prepRyth -1)
                        roleKEY <- forM [1..prepRyth] (\y -> do
                                let gegenStueck = filter (/=z) [1..prepRyth]
                                let makShow aa = show(mapRepKEYI aa)
                                let role = (map show gegenStueck)
                                return(role) )
                        
                        putStrLn (show (take 1 roleKEY))
                       -- let sta1 =  roleKEY --(ausw 1 (concat (take 1 roleKEY2)))
                       -- let sta2 = sum sta1
                       -- putStrLn (show sta1) 
                       -- putStrLn (show sta2)
                       -- let foPoin = sta2--basis crit (foAdecide (show chhos) )
                        --innerPoints <- forM 
                        return ())

              let aliste7 u = take (length (checLength u)) [1,1..] 
              putStrLn (show (zipWith (+) (aliste7 4) (checLength 4)))
              putStrLn (unlines(checkflow [] ((outputPunktOrder 4))))
              let altgh2 i = let steA i = show (zipWith (+) (aliste7 i) (checLength i))
                             in let steB i =  (checkflow [] ((outputPunktOrder i)))
                             in (((steA i))++" bonelist content: " ++ (unwords (steB i)))
              let altgh3 i = let steA i = show (zipWith (+) (aliste7 i) (checLength i))
                             in let steB i =  (checkflow [mother] ((outputPunktOrder i)))
                             in (((steA i))++"atom bonelist: " ++ (unwords (steB i)))
              let neFunc l = do
                   does <- forM [1..prepRyth] (\l -> do
                       let alines=   ((altgh2 l)) -- ++" "++(altgh3 4)))
                       let aliasType = (altgh3 (last [(prepRyth)]))
                       let narrowGate r = if r<=l then "Occurance: "++(show l)++" bonelist item: "++alines
                                          else "Occurance: "++(show l)++ " bonelist item: "++aliasType
                       return (narrowGate l))
                   does 
            
              putStrLn (unlines(neFunc 2))
              putStrLn "End"

       --------------------------------------------------------------------------------------------
       --------------------------------------------------------------------------------------------
             -- let forWrite = let commaWeg = map (\c -> if c==',' then ' ' ; else c)

               --              in  ( combo) --commaWeg ((((filter (/='"') combo)))
             -- writeFile "testInhalt4.txt" (forWrite) --putStrLn (show (length manyMsplit))
  --            putStrLn (unlines line1Id) --(unwords tideal)
    --          putStrLn (show whereTO)
      --        putStrLn (show(length(unlines tguess)))
        --      putStrLn line1Ideal 
     (frame0 bonelist (mofaList) connectWrist dit dit2) 


 ----------------------------------------------------------------------------------------------------

----------------------------------------------------------------------------------
--see the different states of flowState above
-- e.g checkflow [mother] (flowState fotrack3 head) -> the optimized OUTPUT row ala [0.52,0.52,0.59,0.52,0.59,0.59,0.59]
checkflow lis f = let ste1 lis f= publishPunkt f lis
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




------------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------------------
-- NAIVE SIMULATION - the modue below is the easiest simulation and does not need further simulation  ************************** Updated 03-09-2019
-- can interact via rate.txt  with KA-MAschiene -> write to Path -> write to SVG
-- called via 'playingFi mother2
-- the one blow consists of naiveSim2 theSnd playingFi2 mother 
naiveSim2 theSeto fotime df = 
     --theSeto <- readFile "c:/stack/KAAnsatz19/src/testInhalt36.txt"                             -- File B 
     let ausw r t = drop (r-1) ( take r t)
     in let foSeto = (replaceE theSeto)
     in let sovielPara = let st1 = length (lines theSeto)
                        in [1..st1]
     --foRandTime <- getCurrentTime
     in let parameters =  lines ((getInts fotime (length (lines foSeto)) 1))
    -- theparameter <- forM ([1]) (\df -> do  -- The Simulated Vals to  
     in let aSeto df = (concat((ausw df (parameters))) )
     in let multi = (aSeto df )
     in let momulti =  (take 3 (head (auswW df (words(replaceColon (filter (/=']') (filter (/='[') multi)))))))
     in let continueLoop = ((filter (/='"') momulti)) 
        --                return (continueLoop)) --(momulti))
     --print ( theparameter)
     in continueLoop 
   --------------------------------------------------------------------------------------------------
--AN EXAMPLE PROBLEM :
-- a product:    stage1          stage2         stage3   
--       I.    
--       II.
--       III.  
-----------------------------------------------------------------------------
-- provide DATA ####################################################################################### EXPLAINED !! 20-5-20
-- Framework for trackArmTest5 (tAT5)
-- the tat5 works only Data on a 'detailed level' to use this computation in a framework
-- that linkes the BONES , SKELETTON structures with tat5 
-- THE DATA STRUCTURE -----------------------------------------------------------
-- 1. fourierMQ6PANOPAN
-- 2. fourierMQ5NOPAN
-- 3. fourierMQ4NOPAN
-- 4. fourierMQ4TRACE
-- 5. wohlGeor3

-- I) rate bone with 1,2,3,4,5
df1 compar pub = map (uniquEClassRAW compar) [1..pub] -- [pg1,pg2,pg2,pg4,pg5] 
------------------------------------------------------
-- II) rate within df1: 4. with 1,2,3,5  
df2 compar pub = map (toleranceNet compar) [1..pub]
------------------------------------------------------
-- III) get a random value of 1,2,3,4
df3 compar pub = map (toGeor3 compar) [1..pub]
fotoGeor3 compar punktList pg = (wohlGeorNet03 compar pg) punktList

-- serve 5 possible random values that are OUTPUT of each Line called
-- the output is used to find solutions.
toGeor3 compar punktList = ((concat (map (fotoGeor3 compar punktList) [1,2,3,4,5])))
------------------------------------------------------


------------------------------------------------------
wohlGeor1 compar punktList = map (wohlGeorNet0 compar) [1..punktList]

wohlGeor2 compar punktList = map (wohlGeorNet01 compar) [1..punktList]

wohlGeor3 compar punktList = let inpu = (toGeor3 compar punktList) -- wohlGeor3
                       in let a = head$F.chooseMQ 1 inpu
                       in let b = head$F.chooseMQ 2 inpu
                       in let c = head$F.chooseMQ 3 inpu
                       in let d = head$F.chooseMQ 4 inpu
                       in let e = head$F.chooseMQ 5 inpu
                       in let foCombos func  = map realToFrac (map ord (show(func)))
                       in let way1 =[(similaritYvalue (foCombos a) (foCombos b)),(similaritYvalue (foCombos a) (foCombos c)),(similaritYvalue (foCombos a) (foCombos d)),(similaritYvalue (foCombos a) (foCombos e )),(similaritYvalue (foCombos e) (foCombos d)),(similaritYvalue (foCombos e) (foCombos c))]
                       in let way2 =[(similaritYvalue (foCombos e) (foCombos d)),(similaritYvalue (foCombos e) (foCombos c)),(similaritYvalue (foCombos e) (foCombos b)),(similaritYvalue (foCombos e) (foCombos a))]
                       in way1


theGeors o = let auswa = head (F.chooseMQ o [(wohlGeor1),(wohlGeor2)])
             in auswa

theVarias p = let auswa = head (F.chooseMQ p [(progVar1 ),(progVar2 ),(progVar3 ),(progVar4 ),(progVar5 ),(progVar6 )])
              in auswa

theTrix w  = let auswa = head (F.chooseMQ w [(amatrix ),(amatrixDif ),(amatrixa ),(amatrixDifa ),(amatrixb ),(amatrixDifb )])
             in auswa


--o:Int choose wohlGeors 
--t:Int choose atom in line
--t:Int choose line
atrix0R foGeo o t foprog p n  = (F.chooseMQ t (concat ((foGeo o) (foprog p) n)))
atrix0R2 t foprog p n  = (F.chooseMQ t ((wohlGeor3 (foprog p) n)))

--------------------------------------------------
-- wohlGeor1 <--> wohlGeor2 <-> wohGeor1
--   I              I+II         III
atrix0 t n = atrix0R theGeors 1 t theVarias 1 n
atrix1 t n = atrix0R theGeors 2 t theVarias 2 n
atrix2 t n = atrix0R theGeors 1 t theVarias 3 n

atrix3 t m = (F.chooseMQ t (wohlGeor3 (progVar1 ) m))
--    IV   <->      set to progVar3   <->  VI
atrix4 t n = atrix0R theGeors 1 t theVarias 4 n
atrix5 t n = atrix0R theGeors 2 t theVarias 5 n
atrix6 t n = atrix0R theGeors 1 t theVarias 6 n
---------------------------------------------------
--------------------------------------------------
-- wohlGeor1 <--> wohlGeor1 <-> wohGeor1
--   I              I+II         III
--   question for project: isomorphsims ?!:) over various pg functions eqivalent to 
atrix0a t n = atrix0R theGeors 1 t theVarias 1 n
atrix1a t n = atrix0R theGeors 1 t theVarias 2 n
atrix2a t n = atrix0R theGeors 1 t theVarias 3 n

atrix3a t m = (F.chooseMQ t (wohlGeor3 (progVar1 ) m))
--    IV   <->      set to progVar3   <->  VI
atrix4a t n = atrix0R theGeors 1 t theVarias 4 n
atrix5a t n = atrix0R theGeors 1 t theVarias 5 n
atrix6a t n = atrix0R theGeors 1 t theVarias 6 n
---------------------------------------------------
--------------------------------------------------
-- wohlGeor2 <--> wohlGeor2 <-> wohGeor2
--   I              I+II         III
atrix0b t n = atrix0R theGeors 2 t theVarias 1 n
atrix1b t n = atrix0R theGeors 2 t theVarias 2 n
atrix2b t n = atrix0R theGeors 2 t theVarias 3 n

atrix3b t m = (F.chooseMQ t (wohlGeor3 (progVar1 ) m))
--    IV   <->      set to progVar3   <->  VI
atrix4b t n = atrix0R theGeors 2 t theVarias 4 n
atrix5b t n = atrix0R theGeors 2 t theVarias 5 n
atrix6b t n = atrix0R theGeors 2 t theVarias 6 n
---------------------------------------------------

amatrix n m = concat [(atrix0 n m),(atrix1 n m),(atrix2 n m)]
amatrixDif n m = concat [(atrix4 n m),(atrix5 n m),(atrix6 n m)]
amatrixa n m = concat [(atrix0a n m),(atrix1a n m),(atrix2a n m)]
amatrixDifa n m = concat [(atrix4a n m),(atrix5a n m),(atrix6a n m)]
amatrixb n m = concat [(atrix0b n m),(atrix1b n m),(atrix2b n m)]
amatrixDifb n m = concat [(atrix4b n m),(atrix5b n m),(atrix6b n m)]

-------------------------------------------------------------
--COMPUTE UNIQUE POINTCLOUD WITH progVar3 ; added 25-4-20 (first approach)

-- atrix constant based on MQ function does not change with string
atrixCo t m = (F.chooseMQ t (wohlGeor3 (progVar3 ) m))
-- Makes own metric
-- one value selected each line is sorted
-- with -order1 which is ascending
-- the othe two rows wil be sorted like order1
foorder k t  = F.chooseMQ k (sort (amatrix2 t t))
amatrix2 n m = concat [(atrixCo 1 m),(atrixCo 2 m),(atrixCo 3 m)]

-- where is value k in each line t
findes k t = let inpu = foorder k t 
                in let inpu2 = (head inpu) `elemIndices` (amatrix2 t t)
                in inpu2
-- sort them
changs t k = F.chooseMQ ((head(findes k t))+1) (amatrix2 k t)
changs2 w t k = F.chooseMQ ((head(findes k t))+1) ((theTrix w) k t)

{-
------------------------------------------------------------------
-- ALL THE SAME DONT WORK  ********************************!!!!!!!!!!!!!!!!!!!!!!!!!
---- COMPUTE UNIQUE POINTCLOUD WITH progVar1 ; added 2-6-20
atrixCo2 t m = (F.chooseMQ t (wohlGeor3 progVar1 m)) --added 2-6-20 
foorder2 k t  = F.chooseMQ k (sort (amatrix3 t t))

amatrix3 n m = concat [(atrixCo2 1 m),(atrixCo2 2 m),(atrixCo2 3 m)] --added 2-6-20 
findes2 k t = let inpu = foorder2 k t  --added 2-6-20 
                in let inpu2 = (head inpu) `elemIndices` (amatrix3 t t)
                in inpu2 
changsI t k = F.chooseMQ ((head(findes2 k t))+1) (amatrix3 k t)
changs4 w t k = F.chooseMQ ((head(findes2 k t))+1) ((theTrix w) k t)
chgLineI t = concat(map (changsI t) [1..3]) 
chgLine3 w t = let a w t k=  (head(changs4 w t k ))
               in map (a w t) [1..3]
pointCloudPG105  = drop 2 (map (chgLine3 1) [1..50])
pointCloudPG106  = drop 2 (map (chgLine3 3) [1..50])
pointCloudPG107  = drop 2 (map (chgLine3 5) [1..7])
ptcPG1_5  = pointCloudPG105 
ptcPG1_6  = pointCloudPG106 
ptcPG1_7  = pointCloudPG107 
-----------------------------------------------------------
---- COMPUTE UNIQUE POINTCLOUD WITH progVar2 ; added 2-6-20
atrixCo2b t m = (F.chooseMQ t (wohlGeor3 progVar2 m)) --added 2-6-20 
foorder2b k t  = F.chooseMQ k (sort (amatrix3b t t))

amatrix3b n m = concat [(atrixCo2b 1 m),(atrixCo2b 2 m),(atrixCo2b 3 m)] --added 2-6-20 
findes2b k t = let inpu = foorder2b k t  --added 2-6-20 
                in let inpu2 = (head inpu) `elemIndices` (amatrix3b t t)
                in inpu2 
changsIb t k = F.chooseMQ ((head(findes2b k t))+1) (amatrix3b k t)
changs4b w t k = F.chooseMQ ((head(findes2b k t))+1) ((theTrix w) k t)
chgLineIb t = concat(map (changsIb t) [1..3]) 
chgLine3b w t = let a w t k=  (head(changs4b w t k ))
               in map (a w t) [1..3]
pointCloudPG105b  = drop 2 (map (chgLine3b 1) [1..50])
pointCloudPG106b  = drop 2 (map (chgLine3b 3) [1..50])
pointCloudPG107b  = drop 2 (map (chgLine3b 5) [1..7])
ptcPG1_5b  = pointCloudPG105 
ptcPG1_6b  = pointCloudPG106 
ptcPG1_7b  = pointCloudPG107 
-----------------------------------------------------------
-}
-- WHAT HAPPENED here:
-- Experiment every atom of bonelist is compared to a 'formation'
--  pg1 -> MQ6;  pg2 -> MQ5;  pg3 -> MQ4 ; pg4 -> TRACE
--
--   input x -> MQ6 x              -> MQ6  |
--           -> MQ5 x              -> MQ5  | --\ val 1=(x1)
--           -> MQ4 x              -> MQ4  | --/  [ MQ6(x1),MQ5(x1),MQ4(x1),TRACE(x1)]
--           -> MQ4 (MQ5(MQ6 x))   -> TRACE|
chgLine t = concat(map (changs t) [1..3]) 
chgLine2 w t = let a w t k=  (head(changs2 w t k ))
               in map (a  w t) [1..3]

pointCloud04 n = ( map chgLine [1..n])
pointCloud05 n = drop 2 (map (chgLine2  1) [1..n])
pointCloud06 n = drop 2 (map (chgLine2  3) [1..n])
pointCloud07 n = drop 2 (map (chgLine2  5) [1..n])
--                e.gs
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
pg1 x = sin x --(F.fourierMQ6NOPAN123 x)
pg2 x = cos x --(F.fourierMQ5NOPAN123 x)
pg3 x = cos x --(F.fourierMQ4NOPAN123 x)
pg4 x = sin x --(F.fourierMQ4TRACE x)
pg11 x = show x --show(F.fourierMQ6NOPAN123 x)
pg22 x = show x --show(F.fourierMQ5NOPAN123 x)
pg33 x = show x --show(F.fourierMQ4NOPAN123 x)
pg44 x = show x --show(F.fourierMQ4TRACE x)
pgFun x = x

-- a program Variables
-- when buiding the final 'Punkt' structure these are different
-- computation that can be used with '[father,mother...' 
progVar1 = "AAABB" --"wrist" --koa = koa
progVar2 = "AAABBAABAB"
progVar3 = "AABAB"
progVar4 = "AAA"
progVar5 = "AAABBBAA"
progVar6 = "BBBAA"

--order1 =  if 
pointCloud01 n  = let toMap e = last((map (amatrix e) [1..50]))
                  in map toMap [1..n]
-- based on wohlGeor3 -> constant-> any string -> aways same points depending on pg functions
pointCloud02 n = drop (n-1) (map (amatrix2 n) [1..50])

pointCloud03 n  = let toMap e = last((map ((theTrix 2) e) [1..50]))
                  in map toMap [1..n]
pointCloud03a n  = let toMap e = last((map ((theTrix 4) e) [1..50]))
                  in map toMap [1..n]
pointCloud03b n = let toMap e = last((map ((theTrix 6) e) [1..50]))
                  in map toMap [1..n]


----------------------------------------------------------


--  DOMAIN:
--  pointCoud1                    JustI or JustI and JustII or JustII -> (fst or fst/snd or snd Compare)
--                     A                   |         B
--        => thing 1 -> atrix0 -> progVar1 |=> thing3 -> atrix0 -> progVar1 
--        => thing1 thing2                 | => thing3 thing4 
--        => thing2                        |=> thing4  
--
-- pointCloud2          A (thing1 thing1/thing2 thing2)
--        Constant based on matrix2  always 50!!!
--        wohlGeorNet03 ->  wohlGeor3 -> atrixCo ->  matrix2
--        =>mq6 pg1 -> with pg2 mq5
--                  -> with pg3 mq4
--                  -> with pg2 TRACEmq4
--
--                  -> with loop (tomporariy set to pg3 mq4)
--
-- -- pointCloud3   JustI or JustI and JustII or JustII -> (fst or fst/snd or snd Compare)
--                  B (thing3 thing3/thing4 thing4)
--        --    ->  matrix
--        =>mq6 pg1 -> with pg2 mq5
--                  -> with pg3 mq4
--                  -> with pg2 TRACEmq4
--
--                  -> with loop (tomporarily set to pg3 mq4)

-- pointCloud4      sort like pointCloud3
--               constant -> matrix2
--               -> each line of matrix2 sort 
--               ->get smallest ones first 
--
-- pointCloud5      sort like pointCloud3
--               constant -> matrix2
--               -> each line of matrix2 sort 
--               ->get smallest ones first 



foptc e = take e [1,4..]  --ptc1
ptc0  e = pointCloud01  e
-- constant based on Mqs
ptc2 e = ( pointCloud02 e)

ptc3RAW e = pointCloud03 e
-- ptc3 : variable function 
-- decide how many points to plot via function variable 'e'
ptc3 e = (ptc3RAW e)


ptc3a e = pointCloud03a e
ptc3b e = pointCloud03b e

ptc4 e = pointCloud04 e
ptc5  n = pointCloud05 n 
ptc6 n = pointCloud06 n 
ptc7  n = pointCloud07 n 
------------------------------------------------------
-- MINIMA I,II,III
fodf1 compar pub = let inpu2 =   map (wohlGeorNeRAW compar) [1..pub] -- same as get tolerance
                   in let toAds t = take t [1,1..]
                   in let toPubs = toAds (length [1..pub])
                   in zipWith (+) (toPubs) (concat inpu2)

fodf2 compar pub = let inpu2 = map (toleranceNew compar) [1..pub]
                   in let toAds t = take t [1,1..]
                   in let toPubs = toAds (length [1..pub])
                   in zipWith (+) (toPubs) (concat inpu2)

fodf3 compar pub = let inpu fopu = wohlGeor3 compar fopu
                   in let inpu2 fopu = (minimum(inpu fopu)) `elemIndices` (inpu fopu)
                   in let toAds t = take t [1,1..]
                   in let toPubs = [1..pub]
                   in zipWith (+) (toAds (length toPubs)) (concat(map inpu2 toPubs))

seven= ["    - - -  \n"++
        "        -   \n"++
        "      - - - \n"++
        "        -  " ]   
      
-- WHERE minima rate bone         
sumdf1 compar pub = (minimum(map sum (df1 compar pub))) `elemIndices` (map sum(df1 compar pub))
-- WHERE minima rate within
sumdf2 compar pub = (minimum(map sum (df2 compar pub))) `elemIndices` (map sum(df2 compar pub))


checkType ffi = [(map length (group(sort(ffi)))), (ffi)]

    --            in let f = ((minimum inpu) `elemIndices` (inpu)) 
      --          in inpu --f 
-----------------------------------------------------------------
-----------------------------------------------------------------
--list for accesFuncWX below
-- exportiert:  [(filtern b a),(filtern b c)]
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
--- Extraordinary function chooses which AND howmany functions of the list above will be put in output
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


vb x = let as = accesFuncWX33 4 [(map pg44 [1..(x)])] [(map pg33 [1..(x)])] [(map pg22 [1..(x)])] [(map pg11 [1..(x)])]  [1..x] "100"
       in writeFile "tutu.wxm" as
         
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

--------------------------------------------------------------------------
--Source Code Experiment MQ-Network 20-5-20
-- see also conceptColored_Counter20.svg for overview
-- more explainations above :df1
-- a List is given of the Form 
--      "wrist" -> similaritYvalue "wrist" 
-- e.g uniquEClass01 "wirst" 1 1 
uniquEClassReallyRAW compar punktList befehle = 
                   let sta1 d = map ord d  
                  -- in let sta1New = map realToFrac (sta1 (show(F.fourierMQ6NOPAN2 fstOsnd )))
                   in let sta2New = map realToFrac (sta1 (show(head(ausw 1 befehle ) punktList )))
                   in let sta3New = map realToFrac (sta1 (show(head(ausw 2 befehle ) punktList )))
                   in let sta4New = map realToFrac (sta1 (show(head(ausw 3 befehle ) punktList )))
                   in let sta5New = map realToFrac (sta1 (show(head(ausw 4 befehle )punktList )))
                   in let staLoop = map realToFrac (sta1 (show(wohlGeorNet02 compar 1 punktList )))

                   in let staCompare = map realToFrac (sta1 ((compar))) -- *****************************SELECT EACH LINE OF GUESS
                   in [(similaritYvalue staCompare sta2New),(similaritYvalue staCompare sta3New),(similaritYvalue staCompare sta4New),(similaritYvalue staCompare sta5New),(similaritYvalue staCompare staLoop)]


uniquEClassRAW compar pk = uniquEClassReallyRAW compar pk [(F.fourierMQ6NOPAN123),(F.fourierMQ5NOPAN123),(F.fourierMQ4NOPAN123),(F.fourierMQ4TRACE)] 
--similaritYValue "wrist" (
uniquEClass01 compar punktList = uniquEClassRAW compar punktList 
--
toleranceNet compar punktList = let inpu = uniquEClass01 compar punktList 
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
wohlGeorNeRAW compar punktList = let inpu = uniquEClass01 compar punktList
                   in let f = ((minimum inpu) `elemIndices` (inpu)) 
                   in f --F.chooseMQ f [b,c,d,e]--if b>c && c>d && d>e then [punktList]
                     -- else []
                     --
toleranceNew compar punktList=
                let inpu = toleranceNet compar punktList
                in (minimum inpu) `elemIndices` (inpu) 

gettolerance compar punktList = map (wohlGeorNeRAW compar) [1..punktList]
wohlGeorNet0 compar punktList = let inpu = uniquEClass01 compar punktList
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
wohlGeorNet01 compar punktList = let inpu = toleranceNet compar punktList
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

wohlGeorNet02 compar fopu punktList = --let inpu = uniquEClass01 compar 1
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

wohlGeorNet03 compar fopu punktList = --let inpu = uniquEClass01 compar 1
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

wohlGeorNet compar punktList = let inpu = uniquEClass01 compar punktList
                   in let b=  head(F.chooseMQ 1 inpu) 
                   in let c = head(F.chooseMQ 2 inpu)
                   in let d = head(F.chooseMQ 3 inpu)
                   in let e = head(F.chooseMQ 4 inpu)
               --    in let d = head(F.chooseMQ 5 inpu)
                   in if b>c && c>d && d>e then [punktList]
                      else []
findOrdung compar until= map (wohlGeorNet compar) [1..until] 

--------------------------------------------------------------------------


--------------------------------------------------------------------------
-- Gaussian influenced - Aehnlichkeitswarscheinlichkeit
--- this function rates the similarity of two
-- Lists vergleicht Aehnlichkeit und gibt 
-- die Prozentzahl der einer stelle 
-- der beiden listen aus
--
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




--
einTyp f = [((plugit f 2),(stelle2er f)),((plugit f 3),(stelle3er f)),((plugit f 5) ,(stelle5er f))] 

einTyp2 f = [((plugit2 f 2),(stelle2er f)),((plugit2 f 3),(stelle3er f)),((plugit2 f 5) ,(stelle5er f))] 
einTyp3 f g = [(plugit2 f g)]

root = "c:/stack/PixelGitJuicy/src/"
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
--Fill AccesFuctions with Data
-- theName: String; Fill AccesFuctions with Data
-- set to shwo tracker1 see below
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





