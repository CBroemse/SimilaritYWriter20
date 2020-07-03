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
      , developersBoard1
      , beginJs
      , canvasJs
      , drawJs
      , sphereJs
      , writedataJs
      , writeFileJs
      , writeFileJs2
      , scaleJs
      , doscale1
      , doscale2
      , doscale3
      , argu
      , fovertex
      , vertexJs
      , scaledJs
      , verJs
      , sphereJs2
      , spherJs)
   --   , setIn)
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


beginJs = ("/*\n"++
           "* @name Geometries\n"++
           "* @description There are six 3D primitives in p5 now.\n"++
           "*/\n")

canvasJs x y = ("function setup() {\n"++
                "  let cnv = createCanvas("++(show x)++", "++(show y)++", WEBGL);\n"++
                "   cnv;\n"++
                "   cnv.position(0,0);\n"++
                "}\n\n")

drawJs backgr = ("function draw() {\n"++
                 "  background("++show backgr++");\n\n")
----------------------------------------------
scaleJs r v = zipWith (*) r v 
listd foptc t = length (foptc t) 
argu foptc t v = take (listd foptc t) (repeat v)
anA foscale foptc t v = scaleJs (argu foptc t v) (foscale foptc t) 

doscale1 foptc t = map head (foptc t)
doscale2 foptc t = map last (foptc t)
doscale3 foptc t = concat((map(ausw 2) (foptc t)))
-- scale: Int; spheres further apart11111
scaledJs v1 foscale foptc t  = let step1 foscale v1 = anA foscale foptc t v1
                          --   in let step2 v1 v2 v3 = [(step1 (doscale1 foptc t) v1),(step1(doscale2 foptc t)v2),(step1(doscale3 foptc t) v3)]
                             in (step1 foscale v1)--step2 v1 v2 v3

sphereJs a = unwords[("  translate("++(show (head (head a)))++", "++(show(head(ausw 2 (head a))))++", "++(show (last (head a)))++");\n"++
                  "  push();\n"++
                  "  rotateX(frameCount * 0.01);\n"++
                  "  rotateY(frameCount * 0.01);\n"++
                 -- "  box(7, 1, 70);\n"++
                  "  sphere(1);\n"++
                  "  pop();\n\n")]

sphereJs2 v1 v2 v3 foptc t = unwords[("  translate("++(show(head(ausw t (scaledJs v1 doscale1 foptc t))))++", "++(show(head(ausw t (scaledJs v2 doscale3 foptc t))))++", "++(show(head(ausw t (scaledJs v3 doscale2 foptc t))))++");\n"++
                  "  push();\n"++
                  "  rotateX(frameCount * 0.01);\n"++
                  "  rotateY(frameCount * 0.01);\n"++
                  "  box(7, 1, 1470);\n"++
                  "  sphere(3);\n"++
                  "  pop();\n\n")]

--x:String ;e.g "X"
sphereJs3 x v1 v2 v3 foptc t = unwords[("  translate("++(show(head(ausw t (scaledJs v1 doscale3 foptc t))))++", "++(show(head(ausw t (scaledJs v3 doscale2 foptc t))))++", "++(show(head(ausw t (scaledJs v2 doscale1 foptc t))))++");\n"++
                  "  push();\n"++
                  "  rotate"++x++"(frameCount * 0.01);\n"++
                  "  rotateY(frameCount * 0.01);\n"++
                  "  box(7, 1, 3470);\n"++
                  "  sphere(3);\n"++
                  "  pop();\n\n")]


fovertex = unlines [("translate(width/2, height/2, 0);\n"++
            "stroke(255);\n"++
            "rotateX(PI/2);\n"++
            "rotateZ(-PI/6);\n"++
            "noFill();\n\n")]
vertexJs v1 v2 v3 foptc t = unwords[("vertex("++show(head(ausw t (scaledJs v1 doscale1 foptc t)))++","++show(head(ausw t (scaledJs v2 doscale2 foptc t)))++","++show(head(ausw t (scaledJs v3 doscale3 foptc t )))++");\n")]

verJs v1 v2 v3 foptc t = unwords(map (vertexJs v1 v2 v3 foptc) [1..t])
spherJs v1 v2 v3 foptc t = unwords(map (sphereJs2 (v1-1) (v2-1) v3 foptc) [1..t])
spherJs2 v1 v2 v3 foptc t = unwords(map (sphereJs3 "Z" (v1-1) (v2-1) v3 foptc) [1..t])
--setIn v1 v2 v3 foptc t=  (vertexJs (scaledJs v1 v2 v3 foptc t))

-- two case a. the ptc function yields only one [line] (only ptc2)
--          b. mostly we deal with [line] longer 1
--           
--lisOlis: [[Double]] ; the input data
writedataJs withFuncJs t = do 
    openProc <- forM [1..t] (\i -> do
          let setIn = withFuncJs (i)
          return (setIn))
    (unlines openProc)  

--writePTCsJs0 foptc t = (beginJs++(canvasJs 1200 800)++(drawJs 250)++(unwords(map(sphereJs foptc) ["1"] ))++"}")
 --e.g>G.writeFileJs 10.0 10.0 10.0 ptc0 ((preX 12))


writePTCsJs v1 v2 v3 foptc t = (beginJs++(canvasJs 1200 800)++(drawJs 250)++(writedataJs (spherJs v1 v2 v3 foptc) t)++(writedataJs (spherJs2 v1 v2 v3 foptc) t)++"}")

writePTCsJs2 v1 v2 v3 foptc t = (beginJs++(canvasJs 1200 800)++(drawJs 0)++fovertex ++"beginShape();\n"++((verJs v1 v2 v3 foptc t))++"endShape();\n"++"}")

writeFileJs v1 v2 v3 foptc t = writeFile "p5SimY/openProces.js" ((writePTCsJs v1 v2 v3 foptc t))
writeFileJs2 v1 v2 v3 foptc t = writeFile "p5SimY/openProces.js" ((writePTCsJs2 v1 v2 v3 foptc t))


ausw n m = drop (n-1) (take n m) 
         
