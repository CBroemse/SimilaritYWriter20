-- this module provides:
-- --------------------------------------
-- four lines of thought
--
--  I. a. compare all atoms of [String] called bonelist and order them due to similaritYvalue
--     b. insert a new line (ghCheck) into a. do the same as above.
-- II. an inherent data type 'Punkt' :: String -> Maybe String
--     that shall fit to I,III and IV. Does error handling , 
--     can be search tree 
-- III. library to plot wxmaxima graphs . functions to compare
--      a. two lines of boneist with each other.
--      b. compare a to periodic functions called pg1 functions
--         e.g  pg1 x = sin x 
-- IV. a 2_3_5_Counter to compare I, II and III with each other

---------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- ABSTRACT:
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
------------------------------------------------------------------------------------



-- Monadic non-deterministic DATA tye 
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
    , tsRAW 
    , iframe_c -- write into storyTeller to html 
    , iframe_cRAW --
    , accesFuncWX33 -- active 28-8-2020 plot 2d
    , vb -- exampe 2d
    , vb2 -- working example
    , runK -- plot one li list with kArmTest5
    , range211 -- experiment2 case1 all smal letters set 
    , range -- experiment2 case 0 daZip
    , vow0 -- a version of vowel function as square matrix M1
    , ausw  
    , progLiT -- export some Prog Vars to TheAtoB.." and Main 
    , ptc0,ptc2,ptc3,ptc4,ptc5,ptc6,ptc7,ptc8,ptc9
    , fofina2 -- write ptc of wxm to svg  -- 01-10-2020
    , foBrad -- according to this metric change above coordinate points
    , egTriangle -- a test triange
    , aHexa
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

--import "ipa.Text.IPA" as I

--
--     1. layer of concept: 
--                          
--inner lexicon :
-- similaritYvalue :: (Fractional p, Ord p, Foldable t1, Foldable t2) => t2 p -> t1 p -> p
--  
--        to describe certain Types we wil use ':=' 
--        which is equivalent to '::' 
--        but ':=' /= '::' thus
--
--simiYvals = [similaritYvalue] := pick1 a [list] -> pick2 a [list] -> compare (pick1 a) to (pick2 a)   
--        this is an equivalent unconsistent (not relyibe) way to describe the concept of this project.    

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

  
 -- "The real power of cathegory-theory is kind of to isolate what you care about...
 --  a group of homomorphisms is always a function between sets..."
 --  Fabrizio Romano Genovese
 --
 --        --   " Truth (Wahrheit) over certainty (Gewissheit)"
--                  (Illobrandt von Ludwiger 'Heim-theory')

---- glossary ideas:
--     the colores := takes the a modified step of sieve of Eratosthens
--                    2 - 3 - 5 and viusualizes structures 
--     iPunkts:= imaginary Punkts used for own reasoning

 --
 --e.g *> let li = ["AAABB","AABAB","AAA","BBBAA"]
 --       let pi = Punkt "M" Nothing Nothing Nothing Nothing Nothing
 --    *  let kArmTest5 li 1 pi 1 1 [] "AAA"
runK d = do 
   let ghd t d = (ausw t d)
   let leng d = length d
   dooer <- forM [1..1] (\fd -> do
        let act = (head (ghd fd d))
        let pi = Punkt "M" Nothing Nothing Nothing Nothing Nothing
        kArmTest5 act 1 pi 1 1 [] "AAA"
        return ())
   return dooer
        
-- a program Variables:
-- the data to be fed into plot via kArmTrack5 
progVarRAW t r = do 
    let df = head (ausw t r)
    df
progVar1 = "0*x + y + 0*z = 3"-- "AaEe0" --"abcde0" --"abcdef" --"abcdef" --show(tussenStap 1 1) --"abcdef" --"abcde0" --"AaEe0" --"AaEe" --"Aa" --"Cesar" --"AAABB" --range --"1" --range211 --"a" --"The world" --"AAABB" 
progVar2 = "0*x + y + 0*z = 33*x + 0 + 0*z = 6" --"AaEeI" --"fghij0" -- "ghijkl" -- "abcdefghijkl" --(show (tussenStap 1 1))++(show(tussenStap 1 2)) -- "abcdefghijkl" --"fghij0" --"AaEeI" --"AaEeIi000" --"AaEe" --"CeaserDelight" --"AAABB"++range --"AB" --range --"ab" --range --"ab" --"The world is everything" --"AAABBAABAB"
progVar3 = "3*x + 0 + 0*z = 6" --"i0000" --"klmno0" --"mnoprr" --"ghijkl" --(show(tussenStap 1 2)) --"mnopqr" --"klmno0" --"i0000" --"Ee" --"Deight" --range --"b" --range --"b" --"is everything" --"AABAB"
progVar4 = "0*x + 0*y + z = 2" --"OoUu0" --"pqrst0" --"stuvwx" --"mnopqr" --(show(tussenStap 1 3)) -- "stuvwx" --"pqrst0" --"OoUu0" --"OoUu" --"Ii" --"Elf" --"AAA" --"c" --range --"b" --tht is" -- find the "a" in wxms "that is" --"AAA"
progVar5 = "0*x + 0*y + z = 2x + y + z = 11" --"OoU0Y" --"uvwxy0" --"yz0000" --"mnopqrstuvwx" --(show (tussenStap 1 3))++(show(tussenStap 1 4)) -- "yz0000000000" --"uvwxy0" --"OoUuY" --"OoUuYy000" --"AAAaaaBBBbbbA+Aaa" --"IiOo" --"ElfFool" --"AAABBBAA" --"cd" --range --"bc" --"tht is the cse" --"that is the case" --"AAABBBAA"
progVar6 = "x + y + z = 11" --"y0000" --"z00000" --"000000" --"yz0000" --(show(tussenStap 1 4)) --"000000" --"z00000" --"y0000" --"y000" -- "BBBbbbA+Aaa" --"uvwxyz" --"uvwxyz" --"Oo" --"Fool" --"BBBAA" --"d" --range --"c" --"the cse" --"the case" --"BBBAA"

progLiT = [progVar1,progVar2,progVar3,progVar4,progVar5,progVar6]
-------------------------------------------
--active 26-8-2020
--a workwing example experiment 2 
-- plot WXmaxima of 
-- case0
foRange = [1..128]
rangeRAW  c = chr c
range   = map rangeRAW foRange
-- case1
foRang2a = [1..128] \\ [32..123]  -- problmes with control set in computation
foRange2 = sort (96 : ( 95 : ( 94 : ( 93 : ( 92 : ( 91 : foRang2a))))))
range211   = map rangeRAW foRange2

-- case2
forange221   = [[65,97],[69,101],[73,105],[79,111]]
range221   = map rangeRAW (concat forange221)

-- test1 vowels
vow0 =  (lines "AaE0\neIi0\nOou0\nU000")


-- case4
-- write a destictable MQ function 
-- without pv functions 
-- import from FourierCLASSIFIERS
functionalizeMQ3 n fstOsnd = F.fofourierRAW n fstOsnd [F.fopanfourier1MQ3,F.fopanfourier2MQ3,F.fopanfourier3MQ3]
fun3 n fstOSnd = functionalizeMQ3 n fstOSnd 
ffourierMQ3 x n fstOSnd = (sin (head (map realToFrac (fun3  n fstOSnd)))*x)
fourierMQ3 x n =   (ffourierMQ3 x n 1) + (ffourierMQ3 x n 2) + (ffourierMQ3 x n 3)
mapFour w r =  (fourierMQ3 r) w
tussenStap  r z = last $ map (mapFour r) [1..z]
fotestExp2MQ w r = (tussenStap r w)
preptestExp2MQ r = map (fotestExp2MQ r ) [1..3]
testExp2MQ r = map preptestExp2MQ [1..r]
------------------------------------------


kArmTest5 bonelist mofaList connectWrist dit dit2 mCommand crit= do
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
              putStrLn ((edR1 1 1) )-- ++ "     " ++ (edRGH 1 1))
              putStrLn ((edR1 1 2) )-- ++ "     " ++ (edRGH 1 2))

              putStrLn ((edR1 1 3) )-- ++ "      " ++ (edRGH 1 3))

              putStrLn ((edR1 1 4) )-- ++ "      " ++ (edRGH 1 4))

      --        putStrLn ("                    " ++ (edRGH 1 5))
              putStrLn ((edR1 2 1) )-- ++ "     " ++ (edRGH 2 1))

              putStrLn ((edR1 2 2) )-- ++ "     " ++ (edRGH 2 2))

              putStrLn ((edR1 2 3) )-- ++ "      " ++ (edRGH 2 3))

              putStrLn ((edR1 2 4) )-- ++ "      " ++ (edRGH 2 4))
              -- putStrLn ("                    "++ (edRGH 2 5))
              putStrLn ((edR1 3 1) )-- ++ "      " ++ (edRGH 3 1))
              putStrLn ((edR1 3 2) )-- ++ "      " ++ (edRGH 3 2))

              putStrLn ((edR1 3 3) )-- ++ "     " ++ (edRGH 3 3))

              putStrLn ((edR1 3 4) )-- ++ "       " ++ (edRGH 3 4))

            --  putStrLn ("                    " ++ (edRGH 3 5)) 
            --  --putStrLn (newToStep 2 3)
          -- EMERGENT structure
          -- [Just \[\\\[40.36697247706422,40.36697247706422,0.0,40.54878048780488]\\\]\]  THE REAl 3 but Rated 4 
          --
    --           [[ ],[ ],[0],[]]     
    --           [[ ],[ ],[0],[]]
    --           [[ ],[ ],[ ],[]]
    --           [[0],[0],[ ],[]]
    --
    --           thanks to Miran Lipovaca
    --           zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
--zipWith' _ [] _ = []  
--zipWith' _ _ [] = []  
--zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- 
--         Just 3 >>= (\x -> Just (show x ++ "!"))
              putStrLn ((edR1 4 1) )-- ++ "      " ++ (edRGH 4 1))
              putStrLn ((edR1 4 2) )--  ++ "      " ++ (edRGH 4 2))

              putStrLn ((edR1 4 3) )-- ++ "       "  ++ (edRGH 4 3))

              putStrLn ((edR1 4 4) )-- ++ "     " ++ (edRGH 4 4)) 

            --  putStrLn ("                    " ++ (edRGH 4 5))
             -- putStrLn ("                    " ++ (edRGH 5 5))

              putStrLn "Test map mother"
            --  putStrLn (unlines(sort(moreEdR1 1 )))
             -- putStrLn (unines(checkflow [mother] 
              putStrLn (mapMo 1 1)
              putStrLn (mapMo 1 2)
              putStrLn (mapMo 1 3)
              putStrLn "Test exportMother"
              putStrLn ((exportMother [1..4]))
              putStrLn (unlines (exportMother2 1))
             -- putStrLn (unlines(map fst randPunktList))
            --  putStrLn (unlines((map snd (unlines randPunktList)))
    -- Plot-------------------------------------------------------------------
          -- STEP III. of program
              let foe t = head (ausw t bonelist)
          --    let op = atrix0a (foe 1) (foe 2) (foe 3) (foe 4) (foe 2) (foe 1) 1 1
              putStrLn "readY to plot"
             -- let pop = (ptc0  5)
              --M.writeWXCloudNODE (pop) (ptc2 5) (ptc3 5) (ptc4 5) (ptc5 5) (ptc6 5)
           --   M.writeWXCloudNODE (ptc3b 1) (ptc3b 2) (ptc3b 3) (ptc3b 4) (ptc3b 5) (ptc3b 6)
           --   M.writeWXCloudNODE (ptc3 2) (ptc3 4) (ptc3 6) (ptc3 8) (ptc3 20) (ptc3 50)  -- nested graph ? 
           --   M.writeWXCloudNODE (ptc6 5) (ptc6 25) (ptc6 50) (ptc6 100) (ptc6 125) (ptc6 150) -- ??? 
         --   M.writeWXCloudNODE (ptc3 5) (ptc7 25) (ptc8 150) (ptc9 100) (ptc7 125) (ptc3 50) --  a plaine  CHANGEABLE with progVars
            --  M.writeWXCloudNODE (testExp2MQ 1) ((testExp2MQ 2)) (transpose (testExp2MQ 3)) (testExp2MQ 4) (transpose(testExp2MQ 5)) (transpose (testExp2MQ 6)) --(ptc7 25) (ptc8 150) (ptc9 100) (ptc7 125) (ptc3 50)
              M.writeWXCloudNODE (ptc6 5) (ptc6 5) (ptc6 50) (ptc6 100) (ptc6 125) (ptc6 150) -- interesting
            --  M.writeWXCloudNODE (ptc7 5) (ptc7 25) (ptc7 50) (ptc7 100) (ptc7 125) (ptc7 150) -- half 'crown'
             -- M.writeWXCloudNODE (ptc8 5) (ptc8 25) (ptc8 50) (ptc8 100) (ptc8 125) (ptc8 150) -- similar to above
           --   M.writeWXCloudNODE (nub(ptc6 5)) (nub(ptc6 25)) (nub(ptc6 50)) (nub(ptc6 100)) (nub(ptc6 125)) (nub(ptc6 5))    -- similar to above 
           --   M.writeWXCloudNODE (ptc0 5) (ptc0 25) (ptc0 50) (ptc0 100) (ptc0 125) (ptc0 150) -- ???

--              M.writeWXCloudNODE (ptc2 5) (ptc2 15) (ptc2 25) (ptc4 2) (ptc4 3) (ptc4 5)
              M.writeWXCloud4 (ptc2 5) (ptc2 25) (ptc2 50) (ptc4 5) (ptc4 25) (ptc4 50)
              
              putStrLn "Done" 
     (frame0 bonelist (mofaList) connectWrist dit dit2) 


-- 9-6-2020
-- Standing true to my premise after having worked out a methodoogy 
-- of simiYval networks there shall be two giants to step on 
-- 1. What are the Eulercaracteristics of the graphs dessribed by kArmTest5
-- 2. What is the dimensionality with regard to roughness(Bernstein??)

 ----------------------------------------------------------------------------------------------------

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
toGeor3 compar punktList = (concat (map (fotoGeor3 compar punktList) [1..(length liT)]))
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
--    IV   <->      IV++VI  <->  VI
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
--    IV   <->      IV++VI  <->  VI
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
--    IV   <->      IV++VI   <->  VI
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
-- ALL ORKS THE SAME DONT WORK  =>  constant 
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
pointCloud05 n = (map (chgLine2  1) [3..n])

pointCloud06 n =   (map (chgLine2  2) [3..(realToFrac n)])
pointCloud07 n =  (map (chgLine2  3) [3..(realToFrac n)])
pointCloud08 n = (map (chgLine2  4) [3..n])
pointCloud09 n = (map (chgLine2  6) [3..n])

workptc9 ptc n = let be f = nub (ptc n) --([(ste1 f),(ste2 f),(ste3 f)])
             in be n
   where
    ste1 f= head (ptc f);
    ste2 f = head(ausw 2 (ptc f));
    ste3 f = last (ptc f);  
---------------------------------------------------------------------------------
---------------------------------------------------------------------------------
-- programmer: 
-- turn a 3d Pointcloud into 3 destinct 2d representations
-- functions exported to main module:
-- tsRAW -- take 100 and nub out (delete all occurances>1) 
-- pp t -- get nubbed lines 
-- daZip -- reduce matix
tsRAW pt = [(nub(pt 25))] -- map group(transpose(nub(pt 10)))


ts = tsRAW ptc6 
maxa t =head$last$group$sort$concat$concat$ausw t ts
picks p = concat(concat(ausw p (transpose ts)))
runM t = take (length (picks t)) (repeat (maxa t))
pp t = concat$concat$ausw t ts
daZip t = zipWith(/) (runM t) (pp t)
qs t = ( "1.0") `elemIndices` (map show (daZip t))
rep r = map (\c -> if c==r then "0"; else c)
rep2 r = map (\c -> if c==r then "0"; else c)

getrep t = rep "1.0" (map show(daZip t))
getrep2 s t = rep s (map show(daZip t))
source line = (group((getrep line)));
--------------------------------------------------------------------------------
forTs r = tsRAW r
ts2 r = (forTs r) 
maxa2 t r =head$last$group$sort$concat$concat$ausw t (forTs r)
 
picks2 t r = concat(concat(ausw t (transpose (ts2 r))))
runM2 t r = take (length (picks2 t r)) (repeat (maxa2 t r))
pp2 t r = concat$concat$ausw t (ts2 r)
daZip2 t r = zipWith(/) (runM2 t r) (pp2 t r)
qs2 t r= ( "1.0") `elemIndices` (map show (daZip2 t r))
rep2B r = map (\c -> if c==r then "0"; else c)
rep22B r = map (\c -> if c==r then "0"; else c)

getrepB t r = rep2B "1.0" (map show(daZip2 t r))
getrep2B s t r = rep2B s (map show(daZip2 t r))
sourceB t line = (group((getrepB t line)))

--------------------------------------------------------------------------------

myMonad = do
      let metric r line = ausw r (group(sort(concat(concat(ausw line ts)))));
      let dats r line = ausw r (group(getrep line))
      let sol r line = length (concat(metric r line))
      pin <- forM [1,2,3] (\pu -> do
          let fosol = length (group(getrep pu)) 
          checklin <- forM [1..(fosol)] (\chk -> do
                 let maak = metric chk pu
                 let retrun = if (map length (maak)) == [2] && (concat maak)/=[1.0,1.0] then [1.0,1.0]
                              else (concat maak) --[(head (metric 1 line)),head(ausw 2 (metric 2 line)),["0","0"]]
                 return(retrun))
          let binder = ( (concat checklin))
          --putStrLn checklin
          return(checklin))
          
      (pin)      

choosM t = (concat(concat(ausw t myMonad) ))
build2d = nub(concat [fw1,fw2,fw3])
   where 
      frameW t g = transpose [(choosM t),(choosM g)];
      fw1 = frameW 1 2;
      fw2 = frameW 1 3;
      fw3 = frameW 2 3;


myMonad2 ptC= do
      let metric r line = ausw r (group(sort(concat(concat(ausw line (ts2 ptC))))));
      let dats r line = ausw r (group(getrepB line ptC))
      let sol r line = length (concat(metric r line))
      pin <- forM [1,2,3] (\pu -> do
          let fosol = length (group(getrepB pu ptC)) 
          checklin <- forM [1..(fosol)] (\chk -> do
                 let maak = metric chk pu
                 let retrun = if (map length (maak)) == [2] && (concat maak)/=[1.0,1.0] then [1.0,1.0]
                              else (concat maak) --[(head (metric 1 line)),head(ausw 2 (metric 2 line)),["0","0"]]
                 return(retrun))
          let binder = ( (concat checklin))
          --putStrLn checklin
          return(checklin))
          
      (pin)      

choosM2 t ptC = (concat(concat(ausw t (myMonad2 ptC)) ))
build2d2 ptC = nub(concat [fw1,fw2,fw3])
   where 
      frameW t g = transpose [(choosM2 t ptC),(choosM2 g ptC)];
      fw1 = frameW 1 2;
      fw2 = frameW 1 3;
      fw3 = frameW 2 3;
     
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

--tsRAW

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
formTest io r e normalFunction = formTestRAW io r e liT normalFunction

formTestRAW io r e e2 normalFunction =  let punktOrPg = checkflow  io [(Punkt  (head(checkflow [] [(basis4 e2 r)])) (Just (basis2 e 1 )) (Just (basis2 e 3 )) (Just (basis2 e 4 )) (Just (basis2 e 5 )) (Just (basis2 e 6))) ]
                 in let choosBy = length(head (group(punktOrPg)))
                 in if choosBy ==2 then normalFunction
                                   else punktOrPg 

runner x = do  --scanChar
        sin x 
    --     f <- (f getLine)    --head [(sin x),(cos x)])
      --   r <- (x)
        -- (return (do r)) --putStrLn (show e) --show(return e)
-- import your functions into kArmTest5 via formation
preX x = head(map scanChar (frmDoubletoInt (show x)))
pg1 x = F.fourierMQ6NOPAN123 x--(sin (read((head((formTest [father] (preX x) [show(sin (x))] (words(show(sin 2))))  ))))) -- (((([(formTest [mother] 1 [show(F.fourierMQ6NOPAN123 (realToFrac (show x)))] (words(show(sin (x)))))]  )))) -- head(ausw 1 [(F.fourierMQ6NOPAN123 x)]) 
pg2 x = (F.fourierMQ5NOPAN123 x)
pg3 x = (F.fourierMQ4NOPAN123 x)
pg4 x = sin x --(F.fourierMQ4TRACE (x))
pg11 x = show x --show(F.fourierMQ6NOPAN123 x)
pg22 x = show x --show(F.fourierMQ5NOPAN123 x)
pg33 x = show x --show(F.fourierMQ4NOPAN123 x)
pg44 x = show x --show(F.fourierMQ4TRACE x)
pgFun x = x

--order1 =  if 
pointCloud01 n  = let toMap e = last((map (amatrix e) [1..50]))
                  in map toMap [1..n]
-- based on wohlGeor3 -> constant-> any string -> aways same points depending on pg functions
pointCloud02 n =  (map (amatrix2 n) [1..(realToFrac n)])

pointCloud03 n  = let toMap e = last((map ((theTrix 2) e) [1..50]))
                  in map toMap [1..n]
pointCloud03a n  = let toMap e = last((map ((theTrix 4) e) [1..50]))
                  in map toMap [1..n]
pointCloud03b n = let toMap e = last((map ((theTrix 6) e) [1..50]))
                  in map toMap [1..n]



     --pg111 
    -- putStrLn (unlines(checkflow [father] (formation (pg1 1) 1)  ))
----------------------------------------------------------
liT = ["AAABB","AABAB","AAA","BBBAA"] --["A","A","A","A","A","A"] --["AAABB","AABAB","AAA","BBBAA"]

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
ptc8  n = pointCloud08 n 
ptc9  n = pointCloud09 n  
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
-- e-g> vb x =  accesFuncWX33 4 [(map pg44 [1..(x)])] [(map pg33 [1..(x)])] [(map pg22 [1..(x)])] [(map pg11 [1..(x)])]  [1..x] "100"

accesFuncWX33 l goghi dipfa fodp1 fodp2 bob =
                  let aw1 n = concat(F.chooseMQ n (outPutMaxima333 goghi dipfa fodp1 fodp2 bob))
                  in let wielanGg  = [1..l]
                  in let aw2 = map aw1 wielanGg
                  in let foaw1 = show bob
                  in let enExp a b sqale2 = (M.aCompleteWX2 a b foaw1 sqale2) -- diese display nach compiliren  vs aCompleteWX2 schreibt display in file     
                     --  in let aw3 =  ceiling (l/2)	
                  in let aw4 = wielanGg --([wielanGg ,[(l+1)..(l*2) ]])
                  in let aw5 = "0.0" 
                  in let aw6 = "1500"              
                  in enExp aw2 aw4 aw6 --enExp


vb x = let as = accesFuncWX33 4 [(map pg44 [1..(x)])] [(map pg33 [1..(x)])] [(map pg22 [1..(x)])] [(map pg11 [1..(x)])]  [1..x] "100"
       in writeFile "tutu.wxm" as


vb2 x = let as = accesFuncWX33 4 [(map (F.fourierMQ3 1) [1..(x)])] [(map (F.fourierMQ3 2) [1..(x)])] [(map (F.fourierMQ3 3) [1..(x)])] [(map pg11 [1..(x)])]  [1..x] "1500"
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

------------------------------------------------------------------------------------------------------------------------
-- WRITE STEP IV SVG (under development) 29-09-2020 ------------------------------------------------------------------
-- legos: [String] ; html/svg/script buiding parts for svg  example1: svgStrings0
-- wander where will rect go now just x (work in one line)
-- anchor: (Double,Double) ;first starting value for plot (x,y)
fofina anchor = do
           let xX z = mieX z
           let yY z = miY z
 
           
           aMonada <- forM [1..3] (\os -> do 
                let conLongituda = if os ==1 then [1,2]
                                   else if os==2 then [1..3]
                                   else [1..5]
                             --   let defaultSwitch = if os /= 1 && os /= 2 then 
               -- the5th <- 
                interna <- forM conLongituda (\as -> do
                       let fogenID = dar as conLongituda
                       let genID = fogenID*(1+(os-1))+as
                       let xx = xX as
                       let yy = yY as
                       ocinquo <- forM [1..5] (\cn -> do

                            let alD = if cn == 1 then   "<g> " ++ snd (aType 7 (xx+75) (yy) genID)++ ("</rect>\n")++(legos genID (xx+15) yy (colorList 1)) ++" </g>\n"

                                      else if cn == 2 then  fst (aType 4 (xx+165) (yy) genID)++ ("</rect>\n")++(legos genID (xx+15) yy (colorList 1))

 
                                      else if cn == 3 then  snd (aType 7 (xx+105) (yy) genID)++ ("</rect>\n")++(legos genID (xx+15) yy (colorList 1))
                                      
                                      else if cn == 4 then  fst (aType 4 (xx+165) (yy) genID)++ ("</rect>\n")++(legos genID (xx+15) yy (colorList 1))

                                      else "<g> " ++ snd (aType 7 (xx+165) (yy) genID)++ ("</rect>\n")++(legos genID (xx+15) yy (colorList 1)) ++" </g>\n"

                                    --  else  snd (aType 7 (xx+215) (yy) genID)++ ("</rect>\n")++(legos genID (xx+15) yy (colorList 1)) 
                            return (alD))
                            
                       let moveOn =  fst (aType 4 (xx) (yy) genID)++ ("</rect>\n")
                       --let moveOn2 =  fst (aType 4 (xx+75) (yy) genID)++ ("</rect>\n")++(legos genID (xx+75) yy (colorList 1))

                       let the5th = ((unlines ocinquo) ++"<g transform=\"translate(100,0)\">\n" ++(moveOn)) ++"\n </g>\n"
                       let altituda  = if os == 1 && even os==False && as == 1 then fst (aType 4 xx yy genID)++ "</rect>\n"++(legos genID (xx+15) yy (colorList 1)) 
                                              else if os ==1  || even os == True && as == 2 then snd (aType 7 (xx) (yy) genID)++ ("</rect>\n") 
                                              else if os ==2 && even os == False || as == 1 then snd (aType 7 (xx+65) (yy) genID)++ ("</rect>\n")++(legos genID (xx+15) yy (colorList 1)) 
                                              else if os ==2 && even os == True || as == 2 then fst (aType 4 (xx+65) (yy) genID)++ ("</rect>\n")++(legos genID (xx+15) yy (colorList 1))  
                                              else if os ==2 && even os == False || as == 3 then snd (aType 7 (xx+65) (yy) genID)++ ("</rect>\n")++(legos genID (xx+15) yy (colorList 1)) 

                                              else (the5th) --snd (aType 7 (xx+155) (yy) genID)++ ("</rect>\n")
                                            --  else the5th --snd (aType 7 (xx+135) (yy) genID)++ ("</rect>\n")

                       
                                           --else fst (aType 4 (xx+270) (yy) genID)++ ("</rect>\n") 
                       return (altituda ))
                return (interna))
           return (aMonada)
           (aMonada)
     
  where
     dar is so = head$ ausw is so;
     colorList c = dar c ["green","red","blue","darkgray","black","white","lime"];
    -- rectAnimate = dar 1 legos; 
    
     wanderLust foAnc w  = foAnc + (dar w [30,60..]) --wander --  [30,60,90...  -- only xs ; where will go which of the 10 rect in each line
     mieX w= wanderLust (fst anchor) w;
     miY w = (snd anchor) +(dar w [0..]);
     no1 id fox foY foColor= ("<rect id=\"rec"++show id++"\" x=\""++show( fox )++"\" y=\"" ++show(foY) ++"\" width=\"30\" height=\"30\" style=\"fill:"++foColor++"\">\n");
     setterAni id fox foY c = [(no1 id fox foY (colorList c)),(svgStrings0)]; 
     aType c fox foY id = ((no1 id fox foY (colorList c)), (unlines (setterAni id fox foY c))) ;-- c int chooe, w : x or y; 	


headSvg = ("<?xml version=\"1.0\" standalone=\"no\"?>\n"++
          "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n"++ 
          "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n\n"++
          "<svg width=\"100%\" height=\"100%\" version=\"1.1\"\n"++
          "xmlns=\"http://www.w3.org/2000/svg\">\n\n")

svgStrings0 = ("<animateColor attributeName=\"fill\" attributeType=\"CSS\" from=\"lime\" to=\"red\" begin=\"1s\" dur=\"5s\"  repeatCount=\"indefinite\" />\n"++
               "    <set attributeName=\"fill\" attributeType=\"CSS\" from=\"lime\" to=\"lime\" begin=\"5\" dur=\"10s\"  repeatCount=\"repeat\" />\n"++
               "    <set attributeName=\"fill\" attributeType=\"CSS\" from=\"lime\" to=\"lime\" begin=\"10\" dur=\"15s\"  fill=\"repeat\" />")    

svgStreamText inText = "<rect id=\"go\" x=\"20\" y=\"350\" width=\"300\" height=\"650\" style=\"fill:green;fill-opacity:0.2\"/>\n"++(inText)
     
finalmente = let capital = concat[(fofina (10,10)),(fofina (10,45)),(fofina(10,80))]  
             in do 
                   putStrLn (unlines$concat$capital)
                   let aColored235 = (headSvg++(unlines$concat$capital)++"</svg>\n")
                   writeFile "p5SimY/svgS/theSVGData2.svg" (aColored235)

legos id cx cy col = ("<ellipse id=\""++show id++"\" cx=\""++ show cx ++"\" cy=\""++ show cy++"\" rx=\"9\" ry=\"12\" stroke=\"black\" stroke-width=\"2px\" style=\"fill:"++ col ++"\">\n"++
                      "<animateColor attributeName=\"fill\" attributeType=\"CSS\" from=\"blue\" to=\"lime\" begin=\"1s\" dur=\"6s\"  repeatCount=\"indefinite\" />\n"++
                      "</ellipse>\n")

-- bradleyFin = 
------------------------------------------------------------------------------------- End write StepiV under development
--
--------------------------------------------------------------------------------------------------------------------------
-- coordinate point for SVG display  
foBrad = [[(16,99),(42,109),(70,117),(96,128),(124,137),(148,148)],{- y=0 first front left-}
         [(52,86),(79,97),(105,106),(131,116),(160,126),(183,134)], {- y = 1 snd from front-} 
         [(85,74),(112,83),(139,93),(166,102),(192,112),(216,122)],
         [(120,62),(147,70),(174,81),(201,91),(228,100),(252,109)],
         [(154,48),(182,58),(209,69),(235,77),(263,87),(287,96)],
         [(190,35),(270,66),(216,46),(245,55),(298,75),(322,83)]]

aHexa =  [[(70,117)],{- y=0 first front left-}
         [(183,134),(52,86)], {- y = 1 snd from front-} 
         [(154,48),(287,96)],
         [(216,46)]]


--bradleyTransform =  
-- similar to above but WRITE hexagons ............................. 29-09-2020 ------------------------------------------------------------------
-- legos: [String] ; html/svg/script buiding parts for svg  example1: svgStrings0
-- wander where will rect go now just x (work in one line)
-- anchor: [(Double,Double)] ;first starting value for plot (x,y)
-- variable list length e.g from a triangle to plot
egTriangle = [[(10.0,26.470588235294116),(10.0,15.0),(6.636306217783966,15.0)]]

--metricChoose minX maxX minY maxY g v = (maxX/maxBradley)*maxX
-- will work with both 'foBrad' or 'egTriangle' 
--
fofina2	 anchor = do
           exp3Header <- readFile "textS/Experiment3Header.txt"
           exp3Tail <- readFile "textS/Experiment3Tail.txt"
           let el cx cy rx ry foCol=  ("<ellipse cx=\""++cx++"\" cy=\""++cy++"\" rx=\""++rx++"\" ry=\""++ry++"\" stroke=\"black\" stroke-width=\"2px\" style=\"fill:"++foCol++"\">\n"++ "</ellipse>\n")
         -- all:String ; "87,0 174,50 174,150 87,200 0,150 0,50 87,0"
         -- mind the empty spaces determine shape
         --  3 triangle or 6 hexagon and so on.
           let graphs all = "<polyline id=\"hexagon\" points=\""++ all ++"\" class=\"hexa\" onclick=\"changeFill()\"/>"
           layerNO <- forM [1,2] (\ly -> do 
                let plugCol = colorList ly 
                aMonada <- forM [1..(length anchor)] (\os -> do 
                let conLongituda =  (tk os anchor)
                let readMore = if length anchor == 1 then 1 
                               else (length conLongituda)
                innRead <- forM [1..(length conLongituda)] (\cs -> do 
                     let gtFst = show$fst$head$ausw cs conLongituda
                     let gtSnd = show$snd$head$ausw cs conLongituda
                     let inPlug = el gtFst gtSnd (unwords( map fst$dotSize ly)) (unwords(map snd$dotSize ly)) (concat plugCol)                    
                     return (inPlug))
                return(innRead))
    -- should be set to 200 or move whole field
                 
                let zooSvg = (mapField ly)++(concat aMonada)++(words ("</g>\n</g>\n"))
                return(zooSvg))
           let anchor2 =  nub(ptc6 10)
        -- turn ptc into 2d drop z coordinate for now
           let dropZet =  map tail$ reverse anchor2 --drop 2 $ map reverse anchor2 --(map show $map reverse(map (drop 1 )(map reverse anchor2)))              -- should be set to 200 or move whole field
           let fopol =  unwords (map (filter (/=']')) (map (filter (/='[')) (map show dropZet)))  
           let polyLine = graphs fopol    
           let zooSvg2 = ((words("<g transform=\"translate(0,200)\">\n"++"<g>\n"))++(words ("</g>\n</g>\n")))
           let zooSvg = exp3Header++ (unwords$concat$ layerNO)++(polyLine)++exp3Tail

----------------------------------------------------
           let droZ = [(dropZet)]
           minkowskiNO <- forM [1] (\ly -> do 
                --let plugCol = colorList ly 
                withM <- forM [1..(length droZ)] (\os -> do 
                let getPairPtc =  (tk os dropZet)
                innRead <- forM [1..2] (\cs -> do 
                     -- read coordinate points could have dropped x or y instead
                     let readXorY = "1" 
                     let bn i = words (take i $ show $ head (ausw cs getPairPtc))
                     let clean i=  words((filter (/='[') (unwords (bn i))))
                     let moreRAW i=  words((filter (/=',') (unwords (clean i))))
                     let more = moreRAW 5
                     --let pWCThing = map read more
                                     --else head(tk cs getPairPtc)
                     let fitMore = map ord (unlines more) 
                     let filT   = let wehhere =  (46 `elemIndices` fitMore) -- where is the '.' 
                                  in zipWith (+) [1..(length wehhere)] wehhere --map (tk t ) wehhere 
                     let digiTs = let xHead = head (unwords$map show filT) 
                                  in let yHead = last (unwords$map show filT) 
                                  in if xHead == '4' then 
                                                     let partPlus =words (take 4 $ show $ head (ausw cs getPairPtc))
                                                     in (take 5 (show (head (ausw cs getPairPtc) *100)))     

                                      else if xHead == '3' then 
                                                     let partPlus =words (take 6 $ show $ head (ausw cs getPairPtc))
                                                     in (take 5 (show (head (ausw cs getPairPtc) *100)))     
                                      else  "0"++(take 4 (show (head (ausw cs getPairPtc) *1)))  
               -- '''''''''''''''''########################################################################i for different 
               --            
                     let findElemB = ((minkowskiAdd2  10.0 "1" ["2.9","2.8","0.01"] (unlines more) ptc6 )) 

                     --let gtFst = (readXorY cs) --head$ausw cs getPairPtc
                    -- let gtSnd = show$snd$head$ausw cs getPairPtc
                 --    let inPlug = el gtFst gtSnd (unwords( map fst$dotSize ly)) (unwords(map snd$dotSize ly)) (concat plugCol)                    
                     return (map show filT )) -- (findElemB))
                return(innRead))
    -- should be set to 200 or move whole field
                 
                let minko = withM --(mapField ly)++(map show (concat withM))++(words ("</g>\n</g>\n"))
                return(minko))
        

           writeFile "zooSvg.svg" (zooSvg)                            
          -- putStrLn (show aMonada)
           putStrLn "transformed to hexagon"
           putStrLn (polyLine)
           putStrLn (unwords (map show dropZet))
           putStrLn (show anchor2)
           putStrLn (unwords$concat$concat$concat$minkowskiNO) 
           let digiTs = let xHead = head (unwords$concat$concat$concat$minkowskiNO) 
                        in let yHead = last (unwords$concat$concat$concat$minkowskiNO) 
                        in xHead --if xHead == 3 then 
           putStrLn ([digiTs])   
  where
     dar is so =  ausw is so;
     colorList c = dar c ["lightblue","lime","blue","darkgray","black","white","lime"];
     dotSize d = dar d [("5","7"),("10","12"),("5","7")];
     fieldPosi fp = (" <g transform=\"translate(0,"++ fp ++")\">\n<g>\n");
     mapField d = ausw d $ map fieldPosi ["0","200"];

    -- rectAnimate = dar 1 legos; 
    
-------------------------------------------------------------------------------------

-------------------------------------------------------------------------------------
-- get screen posi "c:/stack/SimiaritYWriter20/src/textS/theSVGHeader.txt"
textSvg = do 
    atext <- readFile "c:/stack/SimilaritYWriter20/src/textS/story01.txt"
    justSvg <- readFile"c:/stack/SimilaritYWriter20/src/p5SimY/svgS/storyTellerRAW.txt"
   -- work Int->
    let hadrian = length (lines atext)
    let adbr f = (head$ausw f (lines atext))
    let littleArrow = map adbr [1..hadrian]
   -- work [String] -> descriptional notes
    let fillTeller = (concat(take 192 (lines justSvg))) ++(atext) ++(concat (drop 196 (take 201 (lines justSvg))))
    let cUrryfame txtcontent= ("<textarea name=\"message\" rows=\""++ show hadrian++"\" cols=\"50\">\n"++
                    ""++txtcontent ++"\n</textarea>\n<br><br>\n\n")
                       
    putStrLn (show hadrian) --let workWith = (take 118 (lines buildSeen))
    writeFile "textS/story01.html" fillTeller --(justSvg++"\n"++(svgStreamText ((cUrryfame(unlines littleArrow)) ))++"\n"++"</svg>\n")
    putStrLn "wrote: textS/story01.html" 
-------------------------------------------------------------
--

-------------------------------------------------------------
-- data entry accessTo iframe_c
-- t: Int; chooses textarea , set to have 2 textareas
-- toWrite: String ; search word in Text area and possible access points
-- chAr : Char ; 'r' or 'w' or 'd'  ; read write or delete lines in filesystem
-- e.g> iframe_c 1 1 "<p>" "wd" [("ptc3\n")++(show (tsRAW ptc3))++"\n"] "4"
iframe_c t railW toWrite chAr ko token= iframe_cRAW t railW toWrite chAr ko token

-- railW: Int if==1 one will add new lines to 'token selected' html 1 0f 9
--            else will drop fst list entry  'token selected' html 1 0f 9

--fiLe:String ;which file to read needed for saVe function
--token: Int ; keep track of state of 'evaToWrit' 
iframe_cRAW t railW toWrite chAr ko token= do
    let setJump = if (read token)<9 then 0 -- with evalToWrite make data closed pipe jump from 9 back to 0
                  else (read token) -1
    
    let buyTicket fotoken = (("textS/indat23720/filesystemDATA/filesystem")++(show setJump)++".html")

    aRawHtmlTxt <- readFile (buyTicket token)
    let solo = length (lines aRawHtmlTxt)
    let interSearch foToWrite = do
              access <- forM [1..solo] (\so -> do
                  let pull = (concat (ausw so (lines aRawHtmlTxt)))
                  let aC = (foToWrite) `elemIndices` (words pull)
                  let withLength = if (length aC)== 0 then 0 --["",""]
                                   else 1 --[(show so),","]
                  return (withLength))
            --  putStrLn  (show access)
              let serf = (1) `elemIndices` (access)
           --   putStrLn (show serf)
              return (serf)
    let examplRAW1 t = head(ausw t (interSearch "<textarea"))  -- [195,225]
    let examplRAW2 t = head(ausw t (interSearch "</textarea>"))
    let exampl0 te k = ausw te [(examplRAW1 k),(examplRAW2 k)] -- switch textareas now 
    let exampl t = [(examplRAW1 t),(examplRAW2 t)]
   
    let typesafer n = let had1 =   (head$head$exampl t) --map ord (exampl))
                      in let had2 = (head$last$exampl t) 
                      in if n<(had1) then had1+1
                         else if n >(had2) then had2-1
                         else n 
    let saferRaw t n foHtml = drop (typesafer t) (take (typesafer n) (lines foHtml))
    let safer t n = saferRaw t n (aRawHtmlTxt)
    let actual = (interSearch (toWrite)) -- head or last for boundaries
    let distance = length$head$actual     
    let turns = if distance>2 then head$ ausw (distance-1) (head actual)
                else last (last actual)

    let sfrField = safer ((head$head$exampl t)+1) ((head$last$ exampl t)-1 )
    let sfrField2 = safer ((head$head$exampl t)+1) ((last$head$ exampl t)-1 )

    let theWrights g = do 
                          (writeFile ("textS/indat23720/filesystemDATA/"++(evalToWrit ("filesystem"++token++".html"))) g)
                          putStrLn ("wrote: "++ ("textS/indat23720/filesystemDATA/"++(evalToWrit ("filesystem"++token++".html")))) 
        
    let railOut = if chAr=="w" then safer ((head$head$exampl t)+1) ((last$head$exampl t)-1)
                  else if chAr=="du" then sfrField\\ (safer ((head$head$exampl t)-1) ((head$head$actual)-1))-- "du" delete upwards
                                     
                  else if chAr=="dl" then sfrField \\ ((ausw ((head$head$actual)+1) (lines aRawHtmlTxt))) -- ( --"dl" delete line
        
                  else if chAr=="dd" then sfrField \\ ((safer (head$head$actual) (head$last$exampl t)))  --"dd" delete downwards
                  else if chAr == "wu" then let step1 = (safer ((head$head$actual)-2) ((head$head$actual)-1))  --"wu" write insert ko upwards 
                                               in lines ((unlines step1 )++ unlines ko ++(unlines(safer (last$head$actual) (head$last$exampl t)))) --((fst step1)) --lin
                  else if chAr == "wd"  then let goPe =  drop ((head$head$exampl t)+1) (take ((last$head$exampl t)-6) (lines aRawHtmlTxt)) 
                                             in goPe ++ ko --lines ((unlines sfrField2) ++ (unlines ko))
            
                  else if chAr =="ra" then safer ((head$head$exampl t)+1) ((last$head$exampl t)-1)
 --"ra" read all 
                  else if chAr =="ru" then let step1 = (safer ((head$head$actual)-2) ((head$head$actual)-1))  --"ru" read, insert ko upwards 
                                          in lines ((unlines step1 )++ unlines ko ++(unlines(safer (head$head$actual) (head$last$exampl t)))) --((fst step1)) --lin
                  else if chAr =="rl" then safer ((head$head$exampl t)+1) ((last$head$exampl t)-1)
                  else if chAr =="rd" then let step1 = (safer ((head$head$exampl t)+1) ((head$head$actual)+1))  --"rd" read downwards until begin length input lines
                                          in lines ((unlines step1 )++ unlines ko ++(unlines(safer ((head$head$actual)+2) (head$last$exampl t)))) 

                  else let step1 = (safer ((head$head$exampl t)+1) ((last$last$ actual)+1))  --"wd" write downwards until begin length input lines
                       in lines ((unlines step1 )++ unlines ko ++(unlines(safer ((head$head$actual)+2) (head$last$exampl t))))  

    let railSystemRAWdrop  foRail =  ((unlines (take ((head$head$exampl t)+1) (lines aRawHtmlTxt)))  ++ (unlines foRail) ++"\n <p>\n"++ (unlines (drop ((head$last$exampl t)) (take solo (lines aRawHtmlTxt)))))
    let railSystemRAW  foRail =  ((unlines (take ((head$head$exampl t)+1) (lines aRawHtmlTxt)))  ++ (unlines foRail) ++"\n \n"++ (unlines (drop ((head$last$exampl t)) (take solo (lines aRawHtmlTxt)))))

    let railSystem  = if railW == 1 then railSystemRAW railOut  
                      else railSystemRAWdrop railOut  
    putStrLn (show (exampl t))  
    putStrLn  (chAr++" to filesystem.html")
    putStrLn  ("searched word: "++ show toWrite)  

    putStrLn ("occourrance list"++show actual) 
  --  putStrLn (unlines sfrField)
    putStrLn (unlines(railOut))
    let junctionSystem = if chAr == "du" || chAr=="dl"||chAr=="dd"||chAr=="wu"|| chAr =="wd" || chAr =="wd2" then do
                               theWrights railSystem
                         else do 
                               writeFile ("textS/indat23720/filesystem.html") (railSystem)
                               putStrLn ("Just READ file system \"textS/indat23720/filesystem.html\"") 
    junctionSystem
    
evalToWrit astrinG = if tzBool>0 then prsRoot++(head tz3)++(show tzInt)++"."++(last tz3)
                     else prsRoot++(head tz3)++("1.")++(last tz3)
     where
    poc1 fOsnd = reverse( fOsnd(break (=='/') (reverse(astrinG))));--prevent '/' cause trouble
    prsInput = poc1 fst;
    prsRoot = poc1 snd;  
    tz0 = (map ord prsInput);
    tz = (filter (>47) (filter (<58)  tz0));
    tzExp = (map chr tz);
    tzBool = length tzExp;
    tzRootSource  = filter (==47); 
    tz1 = tz0 \\ tz;
    tz2 = map (\c -> if c=='.' then ' '; else c);
    tz3 = words (tz2 (map chr tz1));
    tzInt = if tzBool==0 then 1
            else if (read tzExp)<9 then (read tzExp)+1
          --  else if (read tzExp)==9 then 0 

            else 0 ;



-- basis
