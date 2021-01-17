{- this module is a layer to                         color-sceme: morning
 turn all data of this programm into workable
 functions that will be exported into main -}
--
-- with mayor modules:
-- ------------------
-- Colored_2_3_5_counter.hs:  plot 2d ,3d, 2d/3d,sort network,  
--                            mq - ptc - fourierFS functions,
--                            busschop-bradley, display trivial machine
--
-- Experiment3:                randomize input, guesses , find solutions,
--                            2 work flows: random number=> baysian type 
--                                 from li and addGH    => baysian type
--                            4 streams in and out of cells:
--                               based on the 2 work fows
--                             => cell I,cell II cell III, cell IV
--                            other destinct work flow based on ptc9
--                            => cell1 , cell2, cell3, cell4
--                            1 formation role-model : runEXP3 && runEXP3Char
--
-- TheAtoBplotter   library to plot wxmaxima graphs . functions to compare
--
-- FourierFieldCLASSIFIER    provides basic magic quadrants or mq-functions
--                            to be used in file "src/Col..20.hs" 

-- UsefulFunctions19 library of useful functions used by all mayor modules
-- Path2Val2        introduce path svg data type not used in this module yet
--
module HoofdDev() where
--prelude
import Data.List
import Data.Char
import Control.Monad
import System.Environment
import System.IO
-- own modules 
import qualified Colored_2_3_5_Counter20 as C
import qualified Experiment3 as E
import qualified DataTypeBayes as BT
import UsefulFunctions19
import DataTypePunkt
   --, Experiment3
   --, TheA
   --
--some functions
ausw = C.ausw
checkflow = E.checkflow
triggerWord = E.triggerWord
inActie = E.inActie
baysianTypeInt=E.baysianTypeInt
baysianTypeString=E.baysianTypeString
--
--
--           ############### whole function is ################################## SAME AS   cellStream3EXT- 13-1-21        
                                                         --  ########################## BOTH are an INDEX matrices (or simply values)
       -- domain2 syntax                                                                                   ##############   I: EXPORT TO MAIN
       --                                                                                                                  II:  PUNKT JUNCTION POINT 
       --                                                   III: JOIN with a. 2dplot, 3dplot, 2/3dplot, ptcChart.html
       --                                                                                                    in Main
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
------------------------------------------------------
-- SIMIVAL RUNS for  'Quirky example'
-- manifest the given li function with and without 'appropiate mathematical syntax'
-- give a simiVal list to compare both states
-- liSolution: a real Prior/Bias?!? 
-- e.g -
li1 = ["0*x + y + 0*z = 3","0*x + y + 0*z = 31*x + 0 + 0*z = 6","1*x + 0 + 0*z = 6","0*x + 0*y + z = 2","0*x + 0*y + z = 2x + y + z = 11","x + y + z = 11"]
li2 = ["0*x + y + 0*z = 3","1*x + 0 + 0*z = 6","0*x + 0*y + z = 2","x + y + z = 11"]

-- so far liSolution is merely a symbolic type it has no use other to be simivalued to other liSolutions 
-- 
-- example for Punkt as selector will try filter : PROTRACTOR at how many degrees is the blind shut
--           test define a function that computes  
--           with Punkt "intern" there is NO ghAdd influence ONLY li 
--           with "cell"  YES ghAdd influence AND li plays an influence

runEXP3 li liSolution pi ghAdd = do                                                         
       -- domain2 syntax                                                                                 
      --                                                                                                             
       let iDF = li --["AaEe0","AaEeI","i0000","OoUu0","OoU0Y","y0000"]                                               
       -- domain1 solution                                                                                             
       --                          
       --
       let lP = liSolution --["0*x + y + 0*z = 3","0*x + y + 0*z = 33*x + 0 + 0*z = 6","3*x + 0 + 0*z = 6","0*x + 0*y + z = 2","0*x + 0*y + z = 2x + y + z = 11","x + y + z = 11"]
       let choosDomain ff =  if ff == 1 then iDF 
                             else lP 
       let chD ff  = choosDomain ff 
       allforIV <- forM [1] (\four4 -> do
            -- first loop is 'IDF' second one is 'LP'
            let de e r = head $ ausw e r                        --------- get every list of li or liSolution
            let foaLi e =  de e (chD four4)
            let aLi = [(foaLi 1),(foaLi 3),(foaLi 4),(foaLi 6)] --------- set to work with li lists of length 6
            --let prepComp g = startDchange g (1,2)
                -- compare internaly
            --let plugChoose = triggerWord pi  
            let fg = let allFor = (E.kArmWORK 1 (chD 2) aLi 1 pi 1 1 [] ghAdd) --(runKAXIOM offOn target plot addGh ghAdd n (theDs four4 d) (get1) (get2) (get3) (get4) 3 (theDs four4 d) (four4) [(read(show four4))] ((tk four4 subroutineList)))
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


-- stringBT:[String]  string bayesian type carries name
-- TWO streams 1. depend on LAST ATOM of list
--             2. depend on ghADD             => both get simivalued
formHoofdEX1 foli4 stringBT pi ghADD =  do
                                        let foliOrder = [1,2,536,537,538,1073,1074,1075,1076,1610,1611,1613]
                                        let runRandomLin foli4 = nub(map (E.cellStream3 foli4) foliOrder)
                                        let rndLin = runRandomLin foli4
                                        let usage h = (head$ausw h rndLin)
                                        let kickThisOF kick this = (map ord kick) \\ (map ord this) -- this list without that  
                                -- take n many solutions in this order
                                        let tHisOrder = if (even(length foliOrder))==False then map usage [1] --map mapKick [1,2,3,4,5,6]
                                                        
                                                        else map usage [2,4,6,8,10,12]
                                       
                                        let takeRndChars n = zufallsBasic1 1 (length(usage 1)) n -- finally select an rand Char with n + 1 
                                        let aTree n = if (length$rulerBT (head foli4) ghADD) == (length ghADD) then do takeRndChars n --map takeRndChars [1..50] --runRandomLin foli4
                                                      else takeRndChars n  --stream2 > streamI
                           -- just depends on li2 
                                        let ping importFunc = checkflow [mother] (E.inActieRAW ["0xy0z=3"] ["x0y0z=6"] ["0x0yz=2"] ["xyz=11"] li2 ["xyz=11"] importFunc)
                          -- pvs:[prog variables] e.g see above  ****************************************************************************************** CHANGE TYPE ORDER
                                        let pingWORK io fofoli guesses importFunc = checkflow io (E.inActieRAW ["0xy0z=3"] ["x0y0z=6"] ["0x0yz=2"] ["xyz=11"] fofoli guesses importFunc) -- pvs can be mixed up to change order
                                        let workLi = BT.bayesAstigmatic "wer" "wer"                           --   read   '*'   OR '**'  
                                  -- thisLi = 
                                      
                                        let slot1 t = ausw t li2
                                        let workSlotRAW streamI streamII  io1 = BT.checkCELLs io1 streamI streamII  -- simi rate this    -- Tuned bayesCELLI..IV
                                        let workSlot io1 =  workSlotRAW "cell" "0" io1
                                        let allSlot = map workSlot (map (:[]) [mother,father,mother2,loopNumber,minMaxTrueOrFalse])
                                        let allSlotWORK streamI streamII = map (workSlotRAW streamI streamII) (map (:[]) [mother,father,mother2,loopNumber,minMaxTrueOrFalse])
                                        let plugExp k = runEXP3 foli4 ( ausw k (concat allSlot)) pi ghADD -- simival depends 1. or 2.
                             -- if lenght fourBreadCells > 4 ,output not availabe via mother etc.
                                        let breedCELLs io a b fourBreadCells = checkflow io $ BT.joinBayes a b (["cell"] ++ fourBreadCells ++ ["CELLONE","CELLTWO","CELLTHREE","CELLFOUR"])
 
                                      --  let baysNothing = pingWORK    -- or conntect ot rndLin                                            /
                        -- connect foli4 ->              BAYES TYPES      ->                                                             /
                        --                     allSlotRAW -> simi rate   intern  -> connected foli4 with simi rate  -> write with plugExpWORK   -> tweak 1 new layer likethis    
                        --                                   via runEXP3                                             
                        --                  map workSlot    
                        --                                               extern  -> connected foli4 with bayes types
                        --                                                          AND connected to ghADD
                                        let plugExpWORK fofoli4 k guess a b = runEXP3 fofoli4 (ausw k (concat (allSlotWORK a b))) pi guess  
                                        --putStrLn (unlines(map show(tHisOrder)))
                                        putStrLn (show(plugExp 1))
                                        let loop = do
                                             miIO <- getLine
                                             foB <- getLine
                                             let ion = head (plugExp 1)
                                             kk <- getLine
                                             out <- getLine
                                             let guesseS n = E.poolBandNotB E.li4 n -- as four bread cells plug into breedCells
                                             let lab io a b guesseS = breedCELLs io a b guesseS
                                             let bios = plugExpWORK E.li4 (read kk) "ww" miIO foB  -- simi rate 
                                             print $show bios  
                                           --  if show ion > ("50") then loop                      -- write Bayes
                                             if out> "2" then loop
                                             else return "done"
                                        loop
                                        putStrLn (show(workSlotRAW "CCCC" "dddd" [mother]))
                                        putStrLn (show(workSlotRAW "CCCC" "A1" [mother]))
                                        putStrLn (show(workSlotRAW "cell" "cell1" [mother]))
                                        putStrLn (show(workSlotRAW "CELLONE1" "CELLONE" [father]))
                                        putStrLn (show(workSlotRAW "CELLONE1" "CELLONE1" [father]))
                                        putStrLn (show(workSlotRAW "CELLONE" "cCELLONE11" [father]))

                                        --putStrLn (show(plugExp 3))


   where
      palette = runEXP3 foli4 stringBT pi ghADD; 
      choosDomain ff =  if ff == 1 then foli4 
                                   else stringBT; 
      chD ff  = choosDomain ff;
-- slotDomain:String, map a li; testMap:String, map various ghADD -- write to stringBT ? 
      rulerBT slotDomain testMap = baysianTypeString [""] slotDomain testMap;  -- test with right length the order of ghADD 
      protractorBT slotDomain strIntBT testMap = baysianTypeInt ["48","","","","","57"] slotDomain strIntBT testMap;
 -- map with filterB 
      actie pv1 pv3 pv4 pv6 foli ghAdd = inActie (pv1 pv3 pv4 pv6 foli [ghAdd]);
      filterB = map (filter (/='A')) E.li4;
      solong b = [1..(length b)];
 --which:Int length output; m: source list; fromA: Int choose from m
      soSame  which m fromA = let stap1 = head(ausw fromA m)
                 in take which [stap1,stap1..];
      bayesMonad b thisAction = do 
          let solang = solong b
          bayWorld0 <- forM solang (\bay -> do
              let outBayesMonad = thisAction
              return (outBayesMonad))
          bayWorld0; 
      typeCheckTriggerword = if (triggerWord (filter (/=' ') (head(checkflow [] [pi]))) "intern" ) == "1" then palette 
                              else if (triggerWord (filter (/=' ') (head(checkflow [] [pi]))) "cell" )  == "1" then (map read [(triggerWord "cell" "cell3")]) -- snd (ghAdd,(snd(charFilterInt ghAdd)))

                         else palette --(head (snd allFor))


                   


