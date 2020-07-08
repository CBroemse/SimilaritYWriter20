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
      , spherJs
      -- write index.html
      , fobase
      , writeHtml)
      --, picks)
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
        

-----------------------------------------------------------------------------
--WRITE index html plot a runKBase run 
--
fobaseD = ("                      </div>\n\n"++
	   "         <div class=\"row\">\n"++
          "<div class=\"column side\" style=\"background-color:none; width: 10%;\">\n\n"++
	  "<a href=\"file:///c://stack/SimilaritYWriter20/src/source/ptcDATA.txt\" target=\"iframe_a\">library           </a>\n"++
          "<a href=\"file:///c:/stack/SimilaritYWriter20/src/p5SimY/action.html\" target=\"iframe_a\">Animate           </a>\n"++
	  "<a href=\"file:///c:/stack/SimilaritYWriter20/src/p5SimY/action.html\" style=\"width:5%;\" target=\"iframe_a\">plot   ____\"A\"_\"A\"_\"A\"_\"A\"_\"A\"</a>\n\n\n"++ 
          "</div>\n\n"++  
          "<div class=\"column middle\" style=\"background-color:none;\">\n"++
           "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/liblabel.png\" name=\"lib_b\"  alt=\"daZip A..A\n"++
           " daZip 1\n"++
           "[1.0,1.0,25.447993447993454]\n"++
           "daZip 2\n"++
           "[1.0,22.318740325077396,1.0]\n"++
           "daZip 3\n"++
           "[30.99532710280374]\" style=\"width:5%\" onclick=\"myFunction(this);\">\n"++
    	   "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/ptc9label.png\" style=\"width:5%\"alt=\"[2.587176602924634,9.333333333333334,1.70261066969353],\n"++
           "[7.469342251950947,1.324503311258278,1.70261066969353],\n"++
           "[7.469342251950947,4.942528735632185,2.1857923497267757],\n"++
           "[11.473565804274465,9.333333333333334,2.1857923497267757],[11.473565804274465,1.324503311258278,0.44493882091212456],\n"++
           "[7.469342251950947,9.333333333333334,1.70261066969353],\n"++
           "[2.587176602924634,4.942528735632185,0.44493882091212456]\n"++
           "=>(1.167*10^5*%i+2.772*10^5)^(1/3)+(4.489*10^3)/(1.167*10^5*%i+2.772*10^5)^(1/3)+69.18\"\n"++
           "onclick=\"myFunction(this);\">\n"++
           "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/ptc8label.png\" style=\"width:5%\" alt=\"[[93.72586872586874,94.02573529411764,93.92523364485982]]\" onclick=\"myFunction(this);\">\n"++  
           "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/ptc7label.png\" style=\"width:5%\" alt=\"[[93.72586872586874,94.02573529411764,93.92523364485982]]\" alt=\"[93.72586872586874,94.02573529411764,3.03030303030303],\n"++
                         "[93.72586872586874,4.212860310421286,93.92523364485982],\n"++
                         "[3.683035714285714,94.02573529411764,93.92523364485982]\" onclick=\"myFunction(this);\">\n"++
                         "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/ptc6label.png\"  style=\"width:5%\" onclick=\"myFunction(this);\">\n"++
                         "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/ptc5label.png\"  style=\"width:5%\" onclick=\"myFunction(this);\">\n"++
                         "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/ptc4label.png\"  style=\"width:5%\" onclick=\"myFunction(this);\">\n"++
                         "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/ptc3label.png\"  style=\"width:5%\" onclick=\"myFunction(this);\">\n"++
                         "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/ptc2label.png\" style=\"width:5%\" onclick=\"myFunction(this);\">\n"++
                         "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/ilabel.png\" style=\"width:5%\" onclick=\"myFunction(this);\">\n"++
                         "</div>\n")


--picks: [String] , [[which ptc],[alt Text]]
--picks pt0 pt2 pt3 pt4 pt5 pt6 pt7 pt8 pt9 = [pt0,pt2,pt3,pt4,pt5,pt6,pt7,pt8,pt9]
writeHtml target ht pv1 pv2 pv3 pv4 pv5 pv6 daZip1 daZip2 daZip3 textAA pt0 pt2 pt3 pt4 pt5 pt6 pt7 pt8 pt9  = if ht==1 then do
                     let foBase = (length target)
                  --   let prem2 n = length$nub$group$ausw n fotsRAW
                     let prem3 d n = map length$group$nub$ausw n d

                     foPeace <- forM [1..foBase](\j -> do
                          let textAA = "=>(1.167*10^5*%i+2.772*10^5)^(1/3)+(4.489*10^3)/(1.167*10^5*%i+2.772*10^5)^(1/3)+69.18\"\n"
                          
                          let stril = [pt0,pt2,pt3,pt4,pt5,pt6,pt7,pt8,pt9]
                          let foalt = map words ["info ptc0","info ptc2","info ptc3","info ptc4","i","i","i","i","i9"]
                          ptcButoons <- forM [1..9] (\btn -> do
                                let seleD = head $prem3 (stril) btn--map group(transpose(nub((ausw btn stril))))
                                let rightPtc = if btn <= 1  then btn - 1
                                               else btn +1
                                chooslabel <- forM [1..4] (\wbt -> do  
                                         let gh = if seleD ==1 then ["ptc"++show (rightPtc)++"reduced.png"]
                                                  else if seleD ==2 then ["ptc"++show (rightPtc)++"green.png"]
                                                  else if seleD == 51 then ["ptc"++show (rightPtc)++"red.png"]
                                                  else ["ptc"++show (rightPtc)++"blue.png"]
                                         let df =  (fobase pv1 pv2 pv3 pv4 pv5 pv6 daZip1 daZip2 daZip3 textAA ([gh]) foalt)

                                         return (df))
                                --let altText = do stril 
                              --  return(chooslabel))  
                                return(seleD))        
                                                    --let df = concat (ptcButoons) 
                          return (ptcButoons))
                     --return(fobase "A" "A" "A" "A" "A" "A" daZip1 daZip2 daZip3 textAA (unlines (map fst (concat$foBae))) foalt))
                     putStrLn "1" 
                   --  writeFile "HtmlS/yourRun.html" ((unlines$concat$ foBae))
                     header <- readFile "HtmlS/allRowsHeader.txt"
               --      let theListIV = header++(unlines foPeace)++"</body>\n"++"</html>\n"
                   --  writeFile "HtmlS/yourRun.html" (unlines$concat$concat$foPeace) --((theListIV))
                     writeFile  "HtmlS/yourRun.html" (show$foPeace) 
                 else 
                     
                     putStrLn (unwords(return("")))
 

fobase et1 et2 et3 et4 et5 et6 daZip1 daZip2 daZip3 textAA tabs foalt = let slc e r = unwords$head (ausw e r) 
                                in let slcT e = slc e tabs 
                                in let slalt e = slc e foalt
                                in  ("                      </div>\n\n"++
	                       "               <div class=\"row\">\n"++
                               "<div class=\"column side\" style=\"background-color:none; width: 10%;\">\n\n"++
	                       "<a href=\"file:///c://stack/SimilaritYWriter20/src/source/ptcDATA.txt\" target=\"iframe_a\">library           </a>\n"++
                               "<a href=\"file:///c:/stack/SimilaritYWriter20/src/p5SimY/action.html\" target=\"iframe_a\">Animate           </a>\n"++
	                       "<a href=\"file:///c:/stack/SimilaritYWriter20/src/p5SimY/action.html\" style=\"width:5%;\" target=\"iframe_a\">plot   ____"++et1++" "++et2++" "++et3++" "++et4++" "++et5++" "++et6++"</a>\n\n\n"++ 
                               "</div>\n\n"++  
                               "<div class=\"column middle\" style=\"background-color:none;\">\n"++
                               "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/liblabel.png\" name=\"lib_b\" alt= \""++ (daZip1 ) ++"\n"++ (daZip2 )++"\n"++(daZip3 )++"\n"++textAA++
                               " style=\"width:5%\" onclick=\"myFunction(this);\">\n"++
    	                       "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/"++slcT 1++"\" style=\"width:5%\" alt=\""++slalt 1++"\" onclick=\"myFunction(this);\">\n"++
                               
                               "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/"++slcT 2++"\" style=\"width:5%\" alt=\""++slalt 3++"\" onclick=\"myFunction(this);\">\n"++  
                               "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/"++slcT 3++"\"  style=\"width:5%\" alt=\""++slalt 4++"\" onclick=\"myFunction(this);\">\n"++ 
                               "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/"++slcT 4++"\"  style=\"width:5%\" alt=\""++slalt 5++"\" onclick=\"myFunction(this);\">\n"++  
                               "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/"++slcT 5++"\"  style=\"width:5%\" alt=\""++slalt 6++"\" onclick=\"myFunction(this);\">\n"++  
                               "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/"++slcT 6++"\"  style=\"width:5%\" alt=\""++slalt 7++"\" onclick=\"myFunction(this);\">\n"++  
                               "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/"++slcT 7++"\" style=\"width:5%\" alt=\""++slalt 8++"\" onclick=\"myFunction(this);\">\n"++  
                               "<img src=\"file:///c:/stack/SimilaritYWriter20/src/source/"++slcT 8++"\" style=\"width:5%\" alt=\""++slalt 9++"\" onclick=\"myFunction(this);\">\n"++  
                               "</div>\n")


