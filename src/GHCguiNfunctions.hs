-- this module provides on main
-- IO window that is a string :(
--
-- important is the ordering and hopefully 'information-design'
-- which is just to give a list of different data sources
-- of the main computations 'kArmTrack5'. A selector exports all selectd
-- functions :
-- selecTOR01 :: :: (Eq a, Num a) => a -> [Char] -> [Char] -> [Char] -> [Char]
           --let willi = ("") 
module GHCguiNfunctions

     (  xEqualsNotify
      , selecTOR1
      , developersBoard1)
       where
import Control.Monad
import Data.List
import Data.Char


-- " to my mind what thats needs 
xEqualsNotify = "xEquasNotify"
--selectFUNC4 :: [IO()] -> IO()

selecTOR1 func = do 
      let solongs = let ste1 = length func
                    in [1..ste1]
      tmnnad <- forM (solongs) (\a -> do
         let keuze = head ( drop (a-1 ) (take a func))
         do (keuze)
         return (keuze))
      putStrLn (show "") --(concat tmnnad))
  
--a = string name backbonefile
--c = string name allwallpath
--d = name fertigeHandle
developersBoard1 a b c d  = do
       writeFile a   ("ENTER a NAME:    \""++a++"\"   ____________________\n"++
                      "|BACKBONE , \"Content"++b++"SVG.txt\n"++
                      "|   AllWallPath File:\""++c++",\" \" \n"++
                      "|   fertigesHandel:   \""++d++"\"\n"++
                      "|         |_SVG________|_SVG____|_____TimeS_____|___int___|\n"++
                      "|  Line     ________________________________________00001 |\n"++
                      "|  Line                                             00002\n"++
                      "|         |Whatdo LIST |________|FERTIGES HANDLE| Wolfram |___ \n"++
                      "|         |            | SHEET  |---------------|         |\n"++
                      "|         | numlay1    |        |accessfunctions|         |\n"++
                      "|         | only here  |________|_______________|_________|\n"++
                      "|         |            |       CONTENT TYPE               |      \n"++
                      "|         |            |     \n"++
                      "                                                          |\n"++
                      "*Backbone the first 'whatdo' list of a Sheet type. 2nd Fertiges Handle \n"++
                      "that is the 2nd svg type of the Line DATA type that uses strictly access\n"++
                      "functions and no numlay type as defined in the frst string of the mayor Backbone Whatdo List\n"++
                      "Wolfram function gives further information about the MAIN  INPUT String and is ver useful." )

