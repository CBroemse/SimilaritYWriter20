-- this module provides:
-- --------------------------------------
-- III. library to plot wxmaxima graphs . functions to compare
--      a. two lines of bonelist with each other.
--      b. compare a to periodic functions called pg functions
--         e.g  pg1 x = sin x 

              --

module TheAtoBplotter (
    
      defSearch  -- enter pg functions and progVars
    , defSearchRAW  
    --, runKRAW  -- like below but set to pick 4 variables of a bonelist
    --, runKBASE -- enter a [li,li..], decide to plot, choose search for a bonelist 
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
 
--"primam sediem a niguem sidicari "
 --e.g *> runKRAW 1 1 [["AAA","BBB","CCC"],["DDD","FFF","HHH"]]
 ----- #####################################################################

theDs f g = concat$ausw f ((concat g))
-- df2 ttt offOn target plot addGh ghAdd n (theDs ttt d) (get1) (get2) (get3)  (get4) 1 (theDs ttt d)
-- =  runKAXIOM offOn target plot addGh ghAdd n (theDs ttt d) (get1) (get2) (get3)  (get4) 1 (theDs ttt d)



runKBASE offOn target plot addGh ghAdd n d get1 get2 get3 get4 = do
       allforIV <- forM [1..(length target)] (\four4 -> do
            let fg = runKAXIOM offOn target plot addGh ghAdd n (theDs four4 d) (get1) (get2) (get3) (get4) 1 (theDs four4 d) (four4) [(read(show four4))]
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
       writeFile (root++"/index2.html") (theListIV)



-- 4-8-2020  ***************************************************************************updated write ptc buttons
-- e.g let myTest at = runKAXIOM 1 [1,2] 2 2 at 2 (li3) 1 2 3 4 3 (head(ausw 1 li3))
-- toca : Int ; give token to change length of pg  functions points n 
--               write toHtml set to write index2.html via 'defSearch', under development
runKAXIOM offOn target plot addGh ghAdd n d get1 get2 get3 get4 ht ulu bog toca=  do
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
              let vanAllen = defSearch offOn d plot addGh ghAdd whichLine1 wL1n2 wL2 wL3 wL3n4 wL4 n ulu (ulu) ht (evalToWrite("foyourRun"++show bog++".txt")) ht toca
              vanAllen
              return([vanAllen]))
          return(dooer)
   hold 
-- tsRAW  

        
-- program variables:
defSearch offOn target plot addGh ghAdd pV1 pV2 pV3 pV4 pV5 pV6 n bonelist pipebone ht forRunHtml htm toca = (defSearchRAW offOn target plot addGh ghAdd pV1 pV2 pV3 pV4 pV5 pV6 50 50 50 50 n bonelist pipebone ht forRunHtml htm) toca  
--e.g> defSearchRAW "AAABB" "AAABBAAABAB" "AAABAB" "AAA" "AAABBBAA" "BBBAA" 1 1 1 1 1 li
--
--pipebone: variable for runKBASE, n-(length target)-many, of a [bonelist]
defSearchRAW offOn target plot addGh ghAdd pV1 pV2 pV3 pV4 pV5 pV6 ptc0Len ptc3Len ptc3aLen ptc3bLen n bonelist pipebone ht forRunHtml htm toca= do

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
       let pg4 x = cos x --(F.fourierMQ4TRACE (x))
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

      -- plot only one function to follow its course and change how many points to plot (track path !!!)***********************************
      --    *> let theOlmegs = ptcs = liT
      --  e.g> olmeg m = (head(ausw m liT))  
                   M.writeWXCloudNODE (nub(ptc4 5)) (nub(ptc4 25)) (nub(ptc4 50)) (nub(ptc4 75)) (nub(ptc4 84)) (nub(ptc4 100)) -- similar to above 
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
          --
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
                 if ht==1 then do
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
                                    substituteAlt = [(nub(ptc4 10)),(nub(ptc4 25)),(nub(ptc4 50)),(nub(ptc4 75)),(nub(ptc4 84)),(nub(ptc4 100))]; -- similar to above
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
                  
                 else 
                     
                     putStrLn (unwords(return("")))

       (writeHtmlIn target wHtml (show(daZip 1 ptc0)) (show(daZip 2 ptc0)) (show(daZip 3 ptc0))  "Time?") 
       --return (cd n)          
                                  --putStrLn "1"-}
     --  let chuckList x = [(ptc0 x),(ptc2 x),(ptc3 x)]
      -- print (concat(chuckList n))

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
 {-
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
    -- even code that is a comment can sti have a function in syntax and concept 
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
              --(allFunctions 3 [mother] [father] [] "" 4)

              putStrLn "Done" 
                  -}
     (frame0 bonelist (mofaList) connectWrist dit dit2) 


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





