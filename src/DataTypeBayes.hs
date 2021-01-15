module DataTypeBayes(
 -- main
   baysianRAW
 , basis4 -- store data in Baysian string   
 , checkType  -- how useful ? 
 , formationB -- cell1 , cell2 with ptc9 ? 
 , nameBayesTypeString -- is liSolutions
 , nameBayesTypeInt  -- help to name bayes types with Ints
 , nameBayesDomain -- most basic bayes metric so far

 -- update self updating html that has some values
 -- only import from 'Col..20.hs'
 , 
  ) where

import Data.List
import Data.Char
import Control.Monad
import System.Environment
import System.IO

import UsefulFunctions19
import DataTypePunkt
import qualified Colored_2_3_5_Counter20 as COL
---------------------------------------------------------
--friend front-end
-- stringBT: String, which name to remember as liSolution(BayesType String)
-- see example 'triggerWord' below
-- build single standing or cells on top of 'triggerWord'
nameBayesTypeString g domain stringBT = triggerWordRAW g domain stringBT
---------------------------------------------------------
-- TEST WHICH BAYSIAN TYPE IS CELL
-- AND IF IT CARRIES AN INT VAUE 
bayesCELLI g domain = nameBayesTypeString g domain "CELLONE" 
--  where
bayesAstigmatic a b = if (map chr (rulerB a)) == ("cell") then (replaceC b) ++ (map chr (rulerA a)) --nameToInt
             else a;
bayesCELLII g domain = nameBayesTypeString g domain "CELLTWO" --
bayesCELLIII g domain = nameBayesTypeString g domain "CELLTHREE"
bayesCELLIV g domain = nameBayesTypeString g domain "CELLFOUR"
{-                                intern   extern
                                  -----  --------
                 ___ CELLONE  | intern  : bayesCELLI |extern__
                /___ ..       ' each    : bayesCELLII|        \
    bayesCELLs /              | 4 new   : bayesCELIII|         \ bayesAstigmatic
               \____..        ' branches: bayesCELLIV|         / 
                \___ CELLFOUR | each    :            |      __/ 
                              ============
                                4 X 4    branches                             -}
---------------------------------------------------------
-- NEXT WHICH VALUE DOES THE TYPE CARRY
-- CHANGE THE VALUE
--
-- listStringBTS : [String] ; give name to cells e.g*> ["CELLONE, "CELLTWO"..."CELLIV"]
-- bayesAs: astigmatic and NotAstig: , bayes A's 
--
-- bayesAs => Int to pipeine goal    this "cell1" "cell" = 1  
-- filter Int "cellCC22" -> "22"
-- *> rulerA 57 
rulerA focell =  filter (<90) (map ord focell) 
rulerB focell = filter (>90) (map ord focell) 

bayesCELLs bayesAs liststringBTS = catchA bayesAs feed 
  where
   feed = liststringBTS;
  -- fomapB g domain stringBT  = nameBayesTypeString g domain stringBT;
  -- mapB = map fomapB;
   nameToInt = replaceC feed;
  -- transformType = map nameToInt ;
    -- filter "cell" out of stringBTS
    -- from "cellCC22" -> "CC22" -> "22"
     -- if a has an Int token list it with the selected Baysian String/Int 
   catchA a b = if (map chr (rulerB a)) == ("cell") then nameToInt ++ (map chr (rulerA a)) --nameToInt
                else b;

replaceC = map (\c -> if c== 'O' then '1'
                      else if c=='N' then '1'
                      else if c=='E' then '1'
                      else if c=='T' then '2'
                      else if c=='W' then '2' 
                      else if c=='o' then '2'
                      else if c=='T' then '3'
                      else if c=='H' then '3'
                      else if c=='R'  then '3'
                      else c)

bayesCell1 g domain = nameBayesTypeString g domain "cellONE"

-- function above names the cells this function
-- filters an Int as type recognition initiation of baysian cell types I..IV
nameBayesTypeInt g domain stringBT = triggerWordRAW g domain stringBT


-- foAli: String the name of this Punkt (baas)
-- underlaying metric of this bayes type
nameBayesDomain foA foAli  =  (unwords(checkBayes [] [(Punkt foAli foA foA foA foA foA)] ))




-- baysian data
-- based on Punkt type 
--addGh:Int ; 1 == add new line to a bonelist: ghCheck and write111111111
-- e.g> let li = ["AAABB","AABAB","AAA","BBBAA"]
--       let pi = Punkt "M" Nothing Nothing Nothing Nothing Nothing
-- e.g> baysianRAW 2 ["AAABB","AAABBAAABAB","AAABAB","AAA","AAABBBAA","BBBAA"] li 1 pi 1 1 [] "DD"
baysianRAW addGh liT bonelist mofaList connectWrist dit dit2 mCommand crit fourierMQ6NOPAN123 = do
     let allAcc foPun =  (checkBayes [] [(foPun)])
 
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
     let formTestRAW io r e e2 normalFunction =  let punktOrPg = checkBayes  io [(Punkt  (head(checkBayes[] [(basis4 e2 r)])) (Just (basis2 e 1 )) (Just (basis2 e 3 )) (Just (basis2 e 4 )) (Just (basis2 e 5 )) (Just (basis2 e 6))) ]
                 in let choosBy = length(head (group(punktOrPg)))
                 in if choosBy ==2 then normalFunction
                                   else punktOrPg 
 

     let formTest io r e normalFunction = formTestRAW io r e liT normalFunction

     let fnACCRAW cou = if unPoint == ("\"notM\"") then fst cou
                        else snd cou
            where 
             unPoint = (show(head(words(unwords(checkBayes [] [connectWrist]))))) ;
 
 -- import 'roaming' data into kArmTrack5     |  
 --  all data in one clunky data-type :
     let checkFo g = if (length g) == 0 then ""
                     else "MOTHER MODE: on"  

     let basis mm foA = Punkt (fnACCRAW(nACCRAW (unwords(allAcc (connectWrist))) ["When set M will work:"++" now sleeping", checkFo mm ] ) ) foA foA foA foA foA
    -- EXAMPLE Punkt new format connect two [Maybe Punkt] list1 and list2
     let formationRAW io1 io2 rd = (head(head(map words(checkBayes io1 [(basis (checkBayes io1 [(basis4 liT (preX rd))]) (Just (maybePu(unwords(formTest io2 (preX rd) [show(fourierMQ6NOPAN123 rd )] (words(show(sin rd))))))  ))]))))
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
     let gaussRate eins zuI = COL.similaritYvalue eins zuI
     -- ---------------------------------------------------------------------------------
     let beRepKEYRAW pick1 pick2 onelist twoList punktList = (crunchtoSimi pick2) 
                      where
                  --      commands =   [bone1,bone2,bone3,bone4,bone5]; 
                        compar = head (ausw (read pick2) twoList);
                        paletteKEY fpic2 twoList astrList = makePalette pick1 fpic2 astrList onelist twoList;
                        cleanPaletteRaw fpic2 twoList astrList = checkBayes[] [(paletteKEY fpic2 twoList astrList )];  -- go to pick2 
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

              let mapMo o r = unwords (checkBayes [mother] (motherTYPE o [1..prepRyth] r))
              let mapMo2 o r = unwords (checkBayes[mother] (motherTYPE2 o [1..prepRyth] r))
              let foexpoMo r w =  unlines (map (edR1 w ) r)
              --let innerLineMother = 
              let exportMother r = unlines(map (foexpoMo r ) r)
              let exportMother2 o = (words (concat(map (mapMo o) [2,3,4])))
              --let tryformation io1 io2 r = formationRAW io1 io2 r --bonelist [(exportMother [1..r])] io r 
              putStrLn "\n test mother functions"
             -- putStrLn (show (tryformation [mother] 1))
          --    putStrLn (show (map muster [1..4]))
          --    putStrLn (show (map muster2 [1..4]))
              putStrLn (unlines(checkBayes[mother] (ausw 1 (motherType (justGene 3) (3)))))
   -- Punkt function:
   -- insert your desired mother type to any Punkt
   -- e.g *> let mymotherT03 t = (ausw 1 (motherType (justGene t) (t)))
   --     *> let accesMT03 t= unlines$checkflow [mother] mymotherT3$ t
   --   use function 'tester' below to accomplish that
              let tester t = (unlines(checkBayes [mother] (ausw 1 (motherType (justGene t) (t)))))
              let motherType3 foas r = map (mayer3 (head(ausw r (map show justIO)))) ([foas])
              putStrLn (unlines(checkBayes [] (ausw 1 (motherType3 (justGene 3) (3)))))
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
                            COL.iframe_c 1 2 "<p>" "wd" toFrame "3"
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
                     foFuncPair fox aDD =  (checkBayes [] [mayer2"jack" fox] >>= (\x -> checkBayes [] [(mayer2 "jack2" [(show x ++ aDD)])]));  --Just 3 >>= (\x -> Just (show x ++ "!"))
                     foFuncPair2  o r  =  (  [mayer2 "otto" [(mapMo o r)] ] >>= (\x ->  [(mayer2 "otto" [(show x ++"   " ++(mapMo2 o r))])])); 
                     punktUpFuncPair fox1 fox aDD io = [( mayer2 (unlines(foFuncPair fox1 "")) (checkBayes io [(maybePu (concat(foFuncPair fox aDD)))]))];
                    
                  --   strives io io2 r aDD ifo = let preIO3 = (checkflow io2  [(maybePu2 "dot" (Just(head(ausw ifo (punktUpFuncPair [(alABOVE fst)] [(alABOVE snd)] aDD io)))))] ) --((head (checkflow [] (io3)))) 
                                   --         in preIO3 --(checkflow io3 [(want ((alABOVE kiez),(alABOVE kiez)))]) -- get name or functionality 
                     fmrTEST io r lis1 lis2 normFu  = formTestRAW io r lis1 lis2 normFu  --[(foFuncPair fox aDD)]
                     fmrTEST2 io e e2 forLine =  checkBayes  io [(Punkt  (head(checkBayes [] [(basis4 e2 forLine)])) (Just (basis2 e 1 )) (Just (basis2 e 3 )) (Just (basis2 e 4 )) (Just (basis2 e 5 )) (Just (basis2 e 6))) ]
                     mapfoFunc2 o = checkBayes [] (concat(map (foFuncPair2 o) (fst finDR)))
                     fmrTEST3RAW io o r e2 forLine =  checkBayes  io [(Punkt  (head(checkBayes[] [(basis4 e2 forLine)])) (Just (head(foFuncPair2 o r ))) Nothing Nothing Nothing Nothing )]
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
-- EXPORT INTERACTIVE HTML to HoofdDEV.hs
-------------------------------------------------------------
-- data entry accessTo iframe_c
-- t: Int; chooses textarea , set to have 2 textareas
-- toWrite: String ; search word in Text area and possible access points
-- chAr : Char ; 'r' or 'w' or 'd'  ; read write or delete lines in filesystem
-- e.g> iframe_c 1 1 "<p>" "wd" [("ptc3\n")++(show (tsRAW ptc3))++"\n"] "4"
iframe_c t railW toWrite chAr ko token= COL.iframe_cRAW t railW toWrite chAr ko token













---------------------------------------------------------
-- BELOW ALL BASIC DEEP BACK-END
-- basic needls
ausw l t = (drop(l-1) (take l t))
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
-- import your functions into kArmTest5 via formation
preX x = head(map scanChar (frmDoubletoInt (show x)))

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
basis4 foAL r = maybePu (show(checkBayes [] [((basis2 foAL r))]))

--baysian pure code
----------------------------------------------------------------------------------
--see the different states of flowState above
-- e.g checkBayes [mother] (flowState fotrack3 head) -> the optimized OUTPUT row ala [0.52,0.52,0.59,0.52,0.59,0.59,0.59]
checkBayes lis f = let ste1 lis f= publishPunkt f lis
                   in let ste2 = map (ste1 lis) f
                   in ste2

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

--------------------------------------------------
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


-------------------- 15-1-21
--develop a cell and give order to it
-- or Bayes board 
---------------------------------------------------------------
--access intern or extern via Punkt ..."intern" ...
--
startDchange g t iOE = let piToBinary = nameBayesDomain Nothing t  
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
                       else ((unlines(checkBayes [mother] [(maybePu "CELL")])) , 0 ) ;
        stC2 g numP = startDchange g  (fst(condI numP)) (show(snd(condI numP)),(fst(condI numP)));


--- KETTENVERTEILUNG aka CHAINDISTRIBUTION
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





