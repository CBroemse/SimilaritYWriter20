module WriteWXmaximaJuicy (
     writeWXCloud4
   , aCloudPointNODE -- in use: write ullu.wxms writes seven files than start at fist again
   , writeWXCloudNODE  -- write lala.wxms to wxms folder being used
   , aCompleteWX
   , aCompleteWX3) where

import Data.Char
import Data.List
import Control.Monad

import qualified UsefulFunctions19 as Ev
{-
let begin = "/* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/
             /* [ Created with wxMaxima version 15.08.2 ] */
             /* [wxMaxima: input   start ] */

-}

-- translates to :

foSlash = (ord '/')
foAster = (ord '*')

beginN =           ("/* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/\n"++
                    "/* [ Created with wxMaxima version 17.08.2 ] */\n\n")

lineStart = ("/* [wxMaxima: input   start ] */\n")
lineEnd =   ("/* [wxMaxima: input   end   ] */\n\n")


-- nimmt Liste der zu bearbeit Funkt. auf, die in DreiBandDATA.hs
-- in jahresInterval ausgegeben wrden
--
--
--
-- fuer dieDartelleung in WX werden weiter parameter benoetigt
-- Format Wievieldaten y -> String;
-- minx -> String;
-- maxX; -> String
middLle anfile y minx maxX = 
                 let input = anfile
                     returnWxLine = let laengen = length anfile
                                        foMap = [1..laengen]
                                        stringSkellett g =  ("[discrete,bur"++(show g)++" ],")
                                        nameWX g = (stringSkellett g)
                                                   
                                        getNameWX = map nameWX foMap 
                                        ridString = let aer = map ord (concat getNameWX)
                                                    in  map chr (filter (/= 34) aer)
                                        outthow =  take ((length (ridString))-1) ridString    
   
                                                     
                                       
                                        
                                    in  ("plot2d\n"++ "(["++(outthow)++"]")    
                     
                 in (lineStart++returnWxLine++", [x,0,"++(y)++"],[y,"++minx++","++maxX++"]);\n"++lineEnd++"")

--inputlist : "[[[1,23],[2,67],[3,45]...]]"
--String list von listen VON LISTEN mit (y,x) kordianten 
middleData inputlist = 
             let laengen = length inputlist
                 takefrom g = ( drop (g-1) (take g inputlist) )
                 foMap = [1..laengen]
                 stringSkellett g =  (lineStart ++"bur"++(show g)++": "++(head (takefrom g))++";\n"++lineEnd)
                 nameWX g = (stringSkellett g)
             in map nameWX foMap

middleData3d inputlist = 
             let laengen = length inputlist
                 takefrom g = ( drop (g-1) (take g inputlist) )
                 foMap = [1..laengen]
                 stringSkellett g =  (lineStart ++"bur"++(show g)++": "++(head (takefrom g))++";\n"++lineEnd)
                 nameWX g = (stringSkellett g)
             in map nameWX foMap




endFileOut = ("/* Maxima can't load/batch files which end with a comment! */\n"++
              "\"Created with wxMaxima\"$")





aCompleteWX inputlist anfile y minx maxX =
            (beginN ++ (concat (middleData inputlist)) ++ (middLle inputlist y minx maxX) ++ endFileOut)

---------------------------------------------------------------------------------------------------------------------------
--
-- fuer dieDartelleung in WX werden weiter parameter benoetigt
-- Format Wievieldaten y -> String;
-- minx -> String;
-- maxX; -> String
-- inhouse 
middCust anfile y minx maxX = 
                 let input = anfile
                     returnWxLine = let laengen = length anfile
                                        foMap = [1..laengen]
                                        stringSkellett g =  ("[discrete,bur"++(show g)++" ],")
                                        nameWX g = (stringSkellett g)
                                                   
                                        getNameWX = map nameWX foMap 
                                        ridString = let aer = map ord (concat getNameWX)
                                                    in  map chr (filter (/= 34) aer)
                                        outthow =  take ((length (ridString))-1) ridString    
   
                                                     
                                       
                                        
                                    in  ("plot2d\n"++ "(["++(outthow)++"]")    
                     
                 in (lineStart++returnWxLine++", [x,0,"++(maxX)++"],[y,-"++maxX++","++maxX++"]);\n"++lineEnd++"")


middCust3d anfile y minx maxX = 
                 let input = anfile
                     returnWxLine = let laengen = length anfile
                                        foMap = [1..laengen]
                                        stringSkellett g =  ("color=red,bur"++(show g)++",")
                                        nameWX g = (stringSkellett g)
                                                   
                                        getNameWX = map nameWX foMap 
                                        ridString = let aer = map ord (concat getNameWX)
                                                    in  map chr (filter (/= 34) aer)
                                        outthow =  take ((length (ridString))-1) ridString    
   
                                                     
                                       
                                        
                                    in  ("draw3d\n"++ "("++(outthow)++")")    
                     
                 in (lineStart++returnWxLine++";\n"++lineEnd++"")


aCompleteWX2 inputlist anfile y minx maxX =
              (beginN ++ (concat (middleData inputlist)) ++ (middCust inputlist y minx maxX) ++ endFileOut)

aCompleteWX3 inputlist anfile y minx maxX =
              (beginN ++ (concat (middleData inputlist)) ++ (middCust3d inputlist y minx maxX) ++ endFileOut)
                                                     
                                       
                                        
-----------------------------------------
nnm t k = (drop (t-1)  (take t k))

--t:Int pick one line;
--stream:Int how many lines to generate
pointCloudEasy  t stream =   
          let aLine = [ (nnm t collm1),(nnm t collm3),(nnm t collm3)] 
          in (concat aLine) -- (unwords(map show(concat aLine))) -- -> "[12.7388] [-25.564] [1.0256] [1.0] [0.0] [1.0] [0.0] [0.0]"
   where
     poiN = stream; 
     collm1  = take poiN [(0.5),(0.5)..];
     counter = take poiN [(1.0),(2.0)..];
     collm2  = (zipWith (*) (take (poiN) [(1.0000),(1.0000)..]) (take (poiN) [(0.5),(0.5)..]));
     collm3  = take poiN [(1.0256),(1.0364)..];
     nuls    = take poiN [0,0..];

pointCloudEasy2  t stream1 stream2 stream3 stream =   
          let aLine = [ (nnm t collm1),(nnm t collm3),(nnm t collm3)] 
          in (concat aLine) -- (unwords(map show(concat aLine))) -- -> "[12.7388] [-25.564] [1.0256] [1.0] [0.0] [1.0] [0.0] [0.0]"
   where
     poiN = stream; 
     collm1  = stream1;
     counter = take poiN [(1.0),(2.0)..];
     collm2  = stream2;
     collm3  = stream3;
     nuls    = take poiN [0,0..];

writeWXCloud2 stream1 stream = do
     writeFile "lala.wxm" (aCloudPoint2 stream1 stream)
aCloudPoint2 func stream = let ste1 t = (func t)
                     in let ste2 = concat(map ste1 [1..(stream)])
                     in let theCloud = "mycloud:points("++(show ste2)++")"
                     in let rndCloud = "rndcloud:points(makelist([sin(t*%pi/10),cos(t*%pi/10),(t*%pi/(random(10)))],t,1,20))$" 
                     in (beginN ++ (aCons theCloud) ++ (aCons aCloudVec) ++ (aCons vecLabelTxt) ++ (aCons drawAll) ++ (aCons ";") ++ lineEnd)  
       where 
    aCloudVec = "[v1,v2,v3,v4]:[vector([0,0,1],[0.7,0,0]),vector([0,0,1],[0,0.7,0]),vector([0,0,1],[-0.7,0,0]),vector([0,0,1],[0,-0.7,0])];";
    vecLabelTxt = "text:label([\"North\",0,1,1],[\"East\",1,0,1],[\"South\",0,-1,1],[\"West\",-1,0,1]);";
    drawAll = "draw3d(color=red,mycloud,color=orange,v1,v2,v3,v4,color=blue,text)$";
    aCons above = (lineStart ++ above ++"\n"++ lineEnd)

writeWXCloud3 stream1 stream2 stream3 = do
     writeFile "lala.wxm" (aCloudPoint3 stream1 stream2 stream3)
aCloudPoint3 stream1 stream2 stream3 = 
                     let ste2 = stream1
                     in let ste3 = stream2
                     in let ste4 = stream3

                     in let theCloud = "mycloud:points("++(show ste2)++")"
                     in let theCloud2 = "mycloud2:points("++(show ste3)++")"
                     in let theCloud3 = "mycloud3:points("++(show ste4)++")"

                     in let rndCloud = "rndcloud:points(makelist([sin(t*%pi/10),cos(t*%pi/10),(t*%pi/(random(10)))],t,1,20))$" 
                     in (beginN ++ (aCons theCloud) ++  (aCons theCloud2) ++  (aCons theCloud3) ++ (aCons aCloudVec) ++ (aCons vecLabelTxt) ++ (aCons drawAll) ++ (aCons ";") ++ lineEnd)  
       where 
    aCloudVec = "[v1,v2,v3,v4]:[vector([0,0,1],[0.7,0,0]),vector([0,0,1],[0,0.7,0]),vector([0,0,1],[-0.7,0,0]),vector([0,0,1],[0,-0.7,0])];";
    vecLabelTxt = "text:label([\"North\",0,1,1],[\"East\",1,0,1],[\"South\",0,-1,1],[\"West\",-1,0,1]);";
    drawAll = "draw3d(color=red,mycloud,color=green,mycloud2,color=black,mycloud3,color=orange,v1,v2,v3,v4,color=blue,text)$";
    aCons above = (lineStart ++ above ++"\n"++ lineEnd)
writeWXCloud4 functionList ste3 ste4 ste5 ste6 ste7 = do
     writeFile (Ev.evalToWrite "wxms/ullu.wxm") (aCloudPoint4 functionList ste3 ste4 ste5 ste6 ste7)

aCloudPoint4 functionList ste3 ste4 ste5 ste6 ste7 = 
                     let ausw = length functionList
            --         short <- forM [1..ausw](\gh -> do
              --              let sd = head(nnm gh (functionList))
                --            let theCloud fg rt = "mycloud:points("++(show (fg rt))++")"
                  --          let cons0 rt = (aCons (theCloud sd rt))
                    --        return(cons0 gh ));
                     in let ste2 =functionList
                 --    in let ste3 = "3"
       ---              in let ste4 = "4" --stream3

                     in let theCloud = "mycloud:points("++(show ste2)++")"
                     in let theCloud2 = "mycloud2:points("++(show ste3)++")"
                     in let theCloud3 = "mycloud3:points("++(show ste4)++")"
                     in let theCloud4 = "mycloud4:points("++(show ste5)++")"
                     in let theCloud5 = "mycloud5:points("++(show ste6)++")"
                     in let theCloud6 = "mycloud6:points("++(show ste7)++")"

               --      in let rndCloud = "rndcloud:points(makelist([sin(t*%pi/10),cos(t*%pi/10),(t*%pi/(random(10)))],t,1,20))$" 
                     in let bh = (beginN  ++ (aCons theCloud)++ (aCons theCloud2) ++ (aCons theCloud3) ++ (aCons theCloud4)++ (aCons theCloud5) ++ (aCons theCloud6)   ++(aCons aCloudVec) ++ (aCons vecLabelTxt) ++ (aCons drawAll) ++ (aCons ";") ++ lineEnd)  
                     in bh
       where 
    aCloudVec = "[v1,v2,v3,v4]:[vector([0,0,1],[0.7,0,0]),vector([0,0,1],[0,0.7,0]),vector([0,0,1],[-0.7,0,0]),vector([0,0,1],[0,-0.7,0])];";
    vecLabelTxt = "text:label([\"North\",0,1,1],[\"East\",1,0,1],[\"South\",0,-1,1],[\"West\",-1,0,1]);";
    drawAll = "draw3d(color=red,mycloud,color=green,mycloud2,color=black,mycloud3,color=brown,mycloud4,color=orange,mycloud5,color=orange,v1,v2,v3,v4,color=blue,mycloud6)$";
    aCons above = (lineStart ++ above ++"\n"++ lineEnd)

writeWXCloudNODE functionList ste3 ste4 ste5 ste6 ste7 = do
     writeFile (Ev.evalToWrite "wxms/lala.wxm") (aCloudPointNODE functionList ste3 ste4 ste5 ste6 ste7)

aCloudPointNODE functionList ste3 ste4 ste5 ste6 ste7 = 
                     let ausw = length functionList
            --         short <- forM [1..ausw](\gh -> do
              --              let sd = head(nnm gh (functionList))
                --            let theCloud fg rt = "mycloud:points("++(show (fg rt))++")"
                  --          let cons0 rt = (aCons (theCloud sd rt))
                    --        return(cons0 gh ));
                     in let ste2 =functionList
                 --    in let ste3 = "3"
       ---              in let ste4 = "4" --stream3

                     in let theCloud = "mycloud:("++(show ste2)++")"
                     in let theCloud2 = "mycloud2:("++(show ste3)++")"
                     in let theCloud3 = "mycloud3:("++(show ste4)++")"
                     in let theCloud4 = "mycloud4:("++(show ste5)++")"
                     in let theCloud5 = "mycloud5:("++(show ste6)++")"
                     in let theCloud6 = "mycloud6:("++(show ste7)++")"

               --      in let rndCloud = "rndcloud:points(makelist([sin(t*%pi/10),cos(t*%pi/10),(t*%pi/(random(10)))],t,1,20))$" 
                     in let bh = (beginN  ++ (aCons theCloud)++ (aCons theCloud2) ++ (aCons theCloud3) ++ (aCons theCloud4)++ (aCons theCloud5) ++ (aCons theCloud6) ++(aCons aCloudVec) ++ (aCons vecLabelTxt)  ++ (aCons (drawList 1)) ++ (aCons (drawList 2)) ++(aCons (drawList 3)) ++ (aCons (drawList 4)) ++ (aCons (drawList 5))++  (aCons (drawList 6))  ++ (aCons drawAll) ++ (aCons ";")++ lineEnd)  
                     in bh
       where 
    aCloudVec = "[v1,v2,v3,v4]:[vector([0,0,1],[0.7,0,0]),vector([0,0,1],[0,0.7,0]),vector([0,0,1],[-0.7,0,0]),vector([0,0,1],[0,-0.7,0])];";
    vecLabelTxt = "text:label([\"North\",0,1,1],[\"East\",1,0,1],[\"South\",0,-1,1],[\"West\",-1,0,1]);";
    drawAll = "draw3d(\n"++
              "      enhanced3d = [sin(j), j],\n"++
              "      point_size = 0.2,\n"++
              "      point_type = filled_circle,\n"++
              "      points_joined = true,\n"++
              "      (m1),m2,m3,m4,m5,m6) $;\n";
    fodraw u = head(nnm u ["mycloud","mycloud2","mycloud3","mycloud4","mycloud5","mycloud6"]);
    drawList u = "m"++show u++":makelist(points ("++ (fodraw u) ++"));";
    aCons above = (lineStart ++ above ++"\n"++ lineEnd)




writeWXCloud stream = do
     writeFile "lala.wxm" (aCloudPoint stream)
aCloudPoint stream = let ste1 t = (pointCloudEasy t stream)
                     in let ste2 = (map ste1 [1..(stream)])
                     in let theCloud = "mycloud:points("++(show ste2)++")"
                     in let rndCloud = "rndcloud:points(makelist([sin(t*%pi/10),cos(t*%pi/10),(t*%pi/(random(10)))],t,1,20))$" 
                     in (beginN ++ (aCons theCloud) ++ (aCons aCloudVec) ++ (aCons vecLabelTxt) ++ (aCons drawAll) ++ (aCons ";") ++ lineEnd)  
       where 
    aCloudVec = "[v1,v2,v3,v4]:[vector([0,0,1],[0.7,0,0]),vector([0,0,1],[0,0.7,0]),vector([0,0,1],[-0.7,0,0]),vector([0,0,1],[0,-0.7,0])];";
    vecLabelTxt = "text:label([\"North\",0,1,1],[\"East\",1,0,1],[\"South\",0,-1,1],[\"West\",-1,0,1]);";
    drawAll = "draw3d(color=red,rndcloud,color=orange,v1,v2,v3,v4,color=blue,text)$";
    aCons above = (lineStart ++ above ++"\n"++ lineEnd)
------------------------------------------------------------------------    
-- makeRawPoints:Int how many ranVars, randomly between 4..10
-- randomVariance:Int  how many points in pointcloud?
 
aCons above = (lineStart ++ above ++"\n"++ lineEnd);
---------------- ********************************************************************************************************* MAIN aMemory EXPORT I
writeMQ6ScreenI makeRawPoints randomVariance whichPoints = writeMQ6ScreenRAW drawCloudPoints makeRawPoints randomVariance 
           where
     drawCloudPoints = if whichPoints=="1" -- just two fields
                       then aCons ("draw3d(color=red,rndcloud,color=blue,bluestream,color=orange,v1,v2,v3,v4,color=blue,text)$")
                       else if whichPoints=="2" -- just three fields	
                       then aCons ("draw3d(color=green,greencloud,color=blue,bluestream,color=orange,v1,v2,v3,v4,color=blue,text)$")
                       else if whichPoints=="3" -- just three fields
                       then aCons ("draw3d(color=red,bluestream,color=magenta,torui,color=orange,v1,v2,v3,v4,color=blue,text)$")
                       else -- just three fields
                           aCons ("draw3d(color=red,rndcloud,color=green,greencloud,color=blue,bluestream,color=magenta,torui,color=orange,v1,v2,v3,v4,color=blue,text)$");

--------------- ********************************************************************************************************** MAIN aMemory EXPORT II
writeMQ6ScreenII makeRawPoints randomVariance = writeMQ6ScreenRAW drawMQ6Sphere makeRawPoints randomVariance 
           where
     drawMQ6Sphere = aCons ("draw3d(nticks=200,color=orange,torus,line_width=2,color=blue,spiral,color=purple,torui,color=blue,text)$");
    

writeMQ6ScreenRAW toExport makeRawPoints randomVariance =
    (beginN++isertStream++rndCloud++rndGen1++rndGen2++rndGlobe++bluestream++rndLine++vectorField++textLabel++fouri1o6++fouri2o6++fouri3o6++fouri4o6++fouri5o6++fouri6o6++fourier6++elemDisplay1++elemDisplay2++elemDisplay3++toExport)
  where
   mR = (show makeRawPoints);
   rV = (show randomVariance);
   lenStrem = [1..randomVariance];
   isertStream = let ste1 t = (pointCloudEasy t (randomVariance))
                 in let ste2 = (map ste1 lenStrem)
                 in aCons("mycloud:points("++show ste2++")$");   
   rndCloud = aCons ("rndcloud:points(makelist([sin(t*%pi/myRandom2(10)),cos(t*%pi/myRandom2 ("++ mR++")),abs(myRandom2("++mR++"))],t,1,"++rV++"))$");
   rndGen1 = aCons ("myRandom(rans):=(random (rans))+1;");
   rndGen2 = aCons ("myRandom2(rans):=(random (rans))+4;");
   rndGlobe = aCons ("rndGlob:points((makelist([sin(t*%pi/myRandom2(10)),cos(t*%pi/myRandom2 ("++mR++")),abs(myRandom2("++mR++"))],t,1,"++rV++")))$");
   rndLine = aCons ("greencloud:points(makelist([sin(t*myRandom2(10)),cos(t*myRandom2 ("++mR++")),abs(myRandom2("++mR++"))],t,1,"++rV++"))$"); -- Rnd Pointcloud -> rnd globe?
   bluestream = aCons ("bluestream:points(makelist([fourier6((t*%pi/myRandom2(10))),fourier6(t*%pi/(10)),fourier6(%pi/myRandom2(10))],t,1,80))$");
   vectorField = aCons ("[v1,v2,v3,v4]:[vector([0,0,2],[2.7,0,0]),vector([0,0,2],[0,2.7,0]),vector([0,0,2],[-2.7,0,0]),vector([0,0,2],[0,-2.7,0])];");
   textLabel = aCons ("text:label([\"North\",0,1,1],[\"East\",1,0,1],[\"South\",0,-1,1],[\"West\",-1,0,1]);");
       fouri1o6 = aCons("ourier1MQ6 (x):= ((sin ( 1*x))+(sin(4*x )) +  (sin(13*x)) + (sin(30*x) )  +  (sin(31*x) ) + ( sin (32*x))); ");
   fouri2o6 = aCons("ourier2MQ6 (x) := ((sin ( 35*x))+(sin(34*x )) +  (sin(8*x )) + (sin(23*x ))  +  (sin(9*x )) + ( sin (2*x)));");
   fouri3o6 = aCons("ourier3MQ6 (x) := ((sin ( 18*x))+(sin(15*x))  +  (sin(17*x)) + (sin(26*x))  +  (sin(16*x) )+ ( sin (19*x)));"); 
   fouri4o6 = aCons("ourier4MQ6 (x) := ((sin ( 27*x))+ (sin(14*x)) + ( sin(28*x)) + (sin(3*x))  +  (sin(29*x)) +  (sin (10*x)));");
   fouri5o6 = aCons("ourier5MQ6 (x) := ((sin ( 25*x))+(sin(11*x)) +  (sin(21*x)) + (sin(22*x))  +  (sin(20*x)) + ( sin (12*x)));");
   fouri6o6 = aCons("ourier6MQ6 (x) := ((sin ( 5*x))+(sin(33*x)) +  (sin(24*x)) + (sin(7*x))  +  (sin(6*x)) + ( sin (36*x)));");
   fourier6 = aCons ("fourier6 (x):= ((ourier1MQ6 (x)) + (ourier2MQ6 (x)) + (ourier3MQ6 (x)) + (ourier4MQ6 (x)) + (ourier5MQ6 (x)) + (ourier6MQ6 (x)));"); 
   elemDisplay1= aCons("spiral:parametric((2-0.5*cos(t))*sin(t/4),(2-0.5*cos(t))*cos(t/4),0.5*sin(t),t,0,8*%pi);"); -- spiral
  -- below  -- torus
   -- a torus manifeting MQ6 with input data a complex? parametric function
   elemDisplay2= aCons("torus:parametric_surface(fourier6((2-0.2*cos(phi)))*sin(theta),fourier6(2-0.2*cos(phi))*cos(theta),fourier6(0.2*cos(phi)),phi,0,2*%pi,theta,0,2*%pi)$");   --  torus
   -- below: a torus manifeting MQ6 with input data
   elemDisplay3 = aCons("torui:parametric_surface(fourier6((2-0.2*cos(phi)))*mycloud(sin(theta)),fourier6(2-0.2*cos(phi))*(cos(theta)),fourier6(0.04*cos(phi)),phi,0,2*%pi,theta,0,2*%pi)$");


