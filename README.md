# SimilaritYWriter20

#### Visiualize a network or your name - nomen est omen

1.0.0
A bayesian network optimization. Compare strings to render
complex programming concepts such as:
[[Int]] , [[String]] =>  bayesian data;   
or complex Numbers with the goal to visiualize higher programming concepts such as classes and types
in 2d or 3d file output 'src/wxms/lala1.wxm'

1.1.0
an iterative process that tries to 'backpropagate' to given program variables (pv1::String)
Finding a random solution. 

1.1.1
'git branch: experiments' Experiment partII
guess n many solutions ...
        
1.0.1
sort any bonelist ([[String]]) with simiaritYWriter a b = simiVals a b  with
    ghAdd::  String -> maximum (simiVals a b ) -> c
    => maximum filter called 'phiMax' 
  
1.0.2  
I. basic axioms --  pv-variables pg-functions
     
  visiualize a 'Char'-type based topology given a certain [[String]] of letters and/or numbers
  
           Pv-variables -- program variables; five or six strings-> ( [[Ord]]  ) get fed into
                           four destinct periodic functions called the
           Pg-functions -- program ground functions; right now set to 
                           pg1 x = F.fourierMQ6NOPAN123 x 
                           pg2 x = (F.fourierMQ5NOPAN123 x)
                           pg3 x = (F.fourierMQ4NOPAN123 x)
                           pg4 x = sin(x) or fourierMQ4TRACE (x)-- all 'src/Colored_2_3_5_Counter20.hs'
    
   all pg-functions can be changed, just note, the more linear the pg functions are the less divertivied is the output.
   pg-functions are a key hole the complexity of a  basic 'algebra', mostly matrix manipulation.
   Not everything is a computer but sufficiently differing concepts are: all pg| f (x) can be changed.
    Pv-variabes are Pg-functions 
                                         
  
  
  -- assume every pg function used has a certain (slope) in the function plotted at every
     Int (Natural number) there is a slope in all pg functions plotted together
     at every Int. If the solution of a matrix is an Int, is there another related order connecting between these plotted 
     functions , that is somehow unique to the solution ??....
     Update 14.09.2022
     'slopes' can be influenced by higher order functions. i.o.w
     think of sionoids like pv-functions
      
  ![alt tag](https://github.com/CBroemse/SimilaritYWriter20/blob/master/source/pgInts.png)  
  
  #####cell
  
        SIMIVAL RUNS for  'Quirky example'
        manifest the given li function with and without 'appropiate mathematical syntax'
        give a simiVal list to compare both states
        liSolution: a real Prior/Bias?!? 
    e.g 
["0*x + y + 0*z = 3","0*x + y + 0*z = 3x + 0 + 0*z = 6","1*x + 0 + 0*z = 6","0*x + 0*y + z = 2","0*x + 0*y + z = 2x + y + z = 11","x + y + z = 11"]

        e.g > liM2 = ["00000","0xy0z=3x0y0z=6","x0y0z=6","0x0yz=2","01111","xyz=11"]
        is the main DATA example of this branch it is 
        a 3x3 matrix + its solution in one [String]
  ![alt tag](https://github.com/CBroemse/SimilaritYWriter20/blob/master/source/explainCell.png)   
  
  ![alt tag](https://github.com/CBroemse/SimilaritYWriter20/blob/master/source/explainCell2.png)   
  
  ![alt tag](https://github.com/CBroemse/SimilaritYWriter20/blob/master/source/explainCell3.png)   
  
       
   example line 1, list 1 of the 'solution matrix' above
   
       cell  name   content -> 1 'filled cell'
          >"  B     Byzyzyz"
          
 'formHoofdDevEX1WORK':: IO ( )            
  the 'hoofd' function is one branch -- HoofdDev.hs   <-->  Main    <-> select in ghc GUI   
  
  The main junction point around a bayesian (until falsifite) data type, see file HoofdDev.hs for details.
  One goal is to write a routine that can run a number of iterations with lists of strings called cells.
  The idea of 'cell' combines a random guess with §1.1.0 and to the concept of 'simiVal' (bottom this README)
  
          poolBandNotB :: [[Char]] -> Int -> [Int]
          poolBandNotB -> 
          *Experiment3> map chr (poolBandNotB li4 1)
                       "zz6zz6zz6zz6zz6zz6"
  
  in that matter it runs an iteration of char guesses via 'poolBandNotB' within an iteration 'formHoofdEX1WORK' 
  both states are combined on iframe_cWORK (iframe_c.hs) and written to html.
          
  
  
                                     mode selFunction              comment1      whatwrite token
                                        |   |                          |                 |  |      
               *Iframe_c> iframe_cWORK "1" "1" "1" 1 1 "<r>" "wdc" "wrote one line\n<s>" 2 "5" 2
                                                |        |     |                     |         |
                                              transp  search  command            build         dataON 
 
      if mode =="1" then  see 'solution matrix' and write main randomly generated cell names 
                          to "textS/indat23720/filesystem"++ token +1 ++.html"
                     
      else  select 11 different [String] to write into the html files the list are either datatype e.g
                                     'B B'  vs '1.55654433'
             one being all chars the the other just number digits. (num String)
 
 How to select modi of 'filled-in cells'
  selFunction: 
  'simiVals' are function  : 2 , 6 , 8 , 10   
  'cell content'  are: 5 , 7 , 9          
  'cell names'    are:  1, 11   ;  3 is any char, 4 insert "cell"
  
  writeToken = (read token)+1 
  Everything gets written into file "textS/indat23720/filesystem"++ (show writeToken) ++".html" 
  case of writeToken+1> 9 then 0 Start overwriting existing files in the same folder, 'search' must occure in
  the actual file selected by 'token'otherwise error  with every call token-1 is read and written to token+1
  thus if iframe_c gets iterated it must be counted for step-width = 2 in other words just choose even or odd 'token'
  numbers in that case ('criteria' Iframe_c.hs) , 'transp' changes output [String] from sorted to unsorted,
  'command' 'dd' delete line 'wd' write downwards or 'wu' write over search token or 'new' empty file 'wdc' comments only ghc GUI,
          'whatwrite' if = 1 then search htm for "<textarea" else search for 'search' which will be automatically written to writeToken,
                                  put 'search' \<p\> and 'build' \<p\> with 'whatwrite' 1 write into fst textarea.
                      else   'search' = "\<p\>" and 'build'== "\<q\>" and 'commant' ="wd" 
                             in an iteration can search next char,than add 'selFunction' to bottom of selected 'token' file and write to writeToken.
                                           
  
  

 
 
 
  #####ptc
  So far the program deals with downstream of the the former functions. 
  the ptc functions are the main basic for this program, see concept plotter, 
  abstractly:
  they are based on 'SimiVal'* compare a pv variable to another -> compare the differences with each other
                                        
           ptc-functions ::
           
  sentence e.g:
  1.The world is everything that is the case.∗ -> sentence -> [String] -> [[String]]
  
    a)
    Colored_2_3_..*> let li = ["The","world","is","everything"]
                  *> let li2 = ["that","is","the","case."]
                  *> let li3 = [[li1,li2]]
  
                  *> let myTestA at = kArmTest5 at 1 pi 1 1 [] "AAA"
                  *> myTestA li  -- write "src/wxms/lala1.wxm -> plot string 

   There are 5 variable functions ( ptc5,ptc6,ptc7,ptc8,ptc9) which yield different 
   related graphs. (see 'How things are compared')
   
    b)  write an 'reduced' table to Htmls/yourRun.html into the ptc buttons in display
        => aim to automatically give an overview of a selected sentence 

    TheAtoBplotter*> let li = ["The","world","is","everything"]
                  *> let li2 = ["that","is","the","case."]
                  *> let li3 = [[li1,li2]]
  
                  *> let myTest at = runKBASE 2 [1,2] 1 2 at 2 (li3) 1 2 3 4 
                e.g> myTest "AAA" subroutinE
  ###### Just li
  ![alt tag](https://github.com/CBroemse/SimilaritYWriter20/blob/master/source/ptc4wittsatz1.png)
  
  #### Step I
   
  ###### a.
  
      this will be the main example being used throughout this program
     
     *> let li = ["AAABB","AABAB","AAA","BBBAA"]
     *> let bonelist = li
     *> let pi = Punkt "m" Nothing Nothing Nothing Nothing Nothing -- data type in kArmTrack5 could help error handling ??
     
     we compare all lines and atoms of the bonlist. To describe certain Types we wil use ' 

            ':=' -- which is equivalent to '::' but ':=' /= '::' thus
     
  ###### save computation
             write occourance list to Html via:
             kArmTest5 addGh liT bonelist mofaList connectWrist dit dit2 mCommand crit
             for variables see StepI below
             e.g 
    *TheAToBplotter>  kArmTest5 2 ["AHEy","AAABBAAABAB","AAABAB","BBB","AAABBBAA","BBBAA"] li 1 pi 1 1 [] "BBBB"
            
            -- imported to TheAToBPlotter from Colored_2_3_5_Counter20 as C.
     C.kArmTest5 addGh li 1 pi 1 1 [] "AAA"    -- the last item "AAA" will be the test run
  ###### main function 
     runKBASE offOn target plot addGh ghAdd n d get1 get2 get3 get4 
     
               offOn: Int, if==1 then run 'kArmTrack5'
               target: Int , if==1 then ....
               plot: Int , if==1 then write file.wxm
               addGH: Int , if==1 then run kArmTrack5 --display order without ghCheck
                            else if ==2 then  run kArmTrack5 -- dispay order WITH ghCheck
                            else dont run
               ghAdd: String , add a string to a bonelist e.g "AAA"
               n : Int ; io (n) in 'kArmTrack5'
               d: [[String]] , [bonelist]
               e.g main> let d = [["AAA","AAA","AAA","AAAB"],["AAABB","AAABAB","AAA","BBBAA"]]
               
               of a bonelist get an element
               e.g main> get 1 ["AAABB","AAABAB","AAA","BBBAA"]
                       > "AAABB"
               get1: Int ,compare get1 to get1++get2 
               get2: Int ,compare get2 to get1++get2 
               get3: Int ,compare get3 to get3++get4
               get4: Int ,compare get4 to get3++get4
     
 ###### experiment 2
  How does the data look like on screen? Our eyes are a broadband connection 
  with the environment. Can we literally gain more insigths by plotting
  and watching graphics of ptc functions on screen?
   A catalouge of ptc functions ptc4 ..ptc9 to see the differences between
   ptc functions is deviced, different outputs always depend on different [String] (li lists).
   The data is handed to the wxm file generator.
   How does the graph change when the input data is brought into square matrices.
   If the length of a given list that shall be brought into a square matrix
   (for symetric matrices eigenvectors and eigenspaces
     are orthogonal *source/makeStats19.wxm*) Compare with function daZip'
                  
                  in wxmaxima
                  (%13)    y: gramschmidt(Mabove);
                  (y)	[[17.08494208494209,5.427509293680298,3.727369542066028],
                  [-9704899/(3^3*127*641),(2^4*11*154789)/(3*3767609),(2^4*1967323)/1881619],
                  [-(2^5*3*23*127*641*1939312688974051*10709394715085445829388803209977114513)/
                      (97*149*151*157*883*21433*1231093*104756501*239289735677*4353051074923305463),
                    (2*1013*952597*3767609*14406767*13031972297*30309630400722546075373424171)/
                      (3*97*149*151*157*883*21433*1231093*104756501*239289735677*4353051074923305463),
                   -(2^2*7*1881619*85024124848484189*663589211299840294699319006606036441)/
                      (97*149*151*157*883*21433*1231093*104756501*239289735677*4353051074923305463)]]
                  => almost complete prime factorization - first line
                  
                  in ghc
                  *Colored_2_3_5_Counter20> daZip 1
                   [1.2319499833831837,3.877983984689766,5.646822478991597]
                   --with one of x y z already set to 0
                   -- e.g:
                  *Colored_2_3_5_Counter20>let u1 = abs$head$ausw 2 [0.0,-14.282466422086806,15.992256626158275]
                  --determine with daZip another list that will be kept
                  *C..> let stepp = zipWith (/) [u1,u1,u1] (daZip 1) 
                  --run test with Experiment3 partII to optimize to find the following list
                  -- [0.028,0.46,0.52] -> minimze the differences in (stepp)
                  *C..> zipWith (+) [0.028,0.46,0.52] stepp
                  [0.6535332549883125,0.6587181190357931,0.6564706763769379] 

                                  
   If a given matrix is not square additional '0' are added until this is achieved. The catalouge exploits this step by plotting
   various examples of ways how this adding of zeros could be done. This second
   experiment also explores how different A's that are subgroups of 
   the Haskell char set encoding appears on screen with regard to varieng ptc functions.
   At one point we select a subgroup that only contains numbers and letters 
   and try to establish an ?'arythmetic-operator'? '+' to observe on screen.
   We have established a group with one of the mentioned As that only operates 
   on letters and numbers.  
   
                       syntactical layer       conceptual layer  
                        plot ptc as wxm        select a ptc function
                        output [String]        choose a case of the catalouge
                                               
   If we choose another A that strives to add the known mathematical matrix algebra to the syntactical
   side with the operators: '0', '=' and the result of that matrix to the conceptual side one example is:
 Experiment 3
'quirky example': (ptc6 5) (ptc6 25) (ptc6 150)...
              ... (ptc6 100) (ptc6 125) (ptc6 50)
with doubled length of pv2 pv5

                       progVar1 = "0*x + y + 0*z = 3"
                       progVar2 = "0*x + y + 0*z = 33*x + 0 + 0*z = 6"
                       progVar3 = "3*x + 0 + 0*z = 6"
                       progVar4 = "0*x + 0*y + z = 2
                       progVar5 = "0*x + 0*y + z = 2x + y + z = 11"
                       progVar6 = "x + y + z = 11"
                       
note! a 3x3 matrix function
file: ptclibrary/Experiment3description1.png

How does the plot look like? There is a 'mistake' in the concept
of the quirky example. How does that change if we correct 'progVar2' and 'progVar3'?
                 
                        :
                        progVar2 = "0*x + y + 0*z = 3x + 0 + 0*z = 6"
                        progVar3 = "x + 0 + 0*z = 6"
                        :
 Does that lead to insights about the syntactical side of the 'quirky example'?
 
 ###### experiment 3
   PartII: find n many solutions to the question above with a random number generator.
           When selecting other progVars, how is the syntactical side of the 'quirky example'
           equivalent to other 3x3 matricres with an unknown result?
                
                         :
                         progVar5 = "0*x + 0*y + z = 2x + y + z = ??"
                         progVar6 = "x + y + z = ??"
                         :
   
   PartI:
   transform data into hexagon and write to 'zooSvg.svg'
   
   a ptc function is nubbed
   
                           *Colored_2_3_5_Counter20> nub (ptc6 10)
[[22.77992277992278,14.338235294117647,6.296691568836714],[10.03861003861004,0.0,25.233644859813086],[10.03861003861004,26.470588235294116,6.296691568836714]]
                           
                           *Colored_2_3_5_Counter20> fofina2 foBrad 1 3
   A geometric transformation of ptc data in a 2d hexagon using 'Busschop-Bradley's method.
   The result is a Svg plot with ptc data on screen. Three different functions help to 
   retrieve, observe and to manipulate the coordinate points of a ptc function.
   
               -- find input "2.81" in list 
               *Col..>  minkowskiAdd2  10.0 "1" ["2.9","2.8","0.01"] "2.81" (((ptc6 ))) 1 1
                ["2278","2277","2200","54"] 
                -- calc value 6.2 at line one take atom 1
               *Col..> (lengthXY (ptc4 )  [foBrad] 1 6.2 fst  "26666.470588235294116" "2.647"1 2)
                [22.2]
                *Col..> mapExp3 foBrad 2 3   -- calc x y of selected ptc (anchor0)
                [[131.47058823529412],[111.2966915688367]]
                
                *Col..> fofina2 foBrad 1 3  -- => overview how data is transformed -> write 'zooSvg.svg'
                
                
  
   ##### state of development

                                       |    the List for ListIV    
                                       | visiualize the reduction in
                                       | the ptc functions matrices
 
                                       | whished visualization of
                                       | the 2_3_5_Counter with data
                                       | from catlouge and experiment3.

                                       | - keep track of the wxm files

                                       | - integrate the MQ functions in the display
                                       |   the 'Dom' ??
                                       
                                       | - how can Gnuplot scripts or Wxmaxima
                                       |   be embedded lazily
                                       | - fill up a library , an overview of everything 
                                       |   also in Git-pages
                                       
                                       | - experiment with calling Haskell from Html
                                       |   in index.html top button row

                                       | complete Experiment2 'catalouge'
                                       | embed Experiment3 into the main program
                                         partI. 
                                         a geometric tansformation of ptc
                                         data in a 2d hexagon using 'Busschop-Bradley's
                                         method. The result is a Svg plot with ptc data on screen.
                                         partII.
                                         guessing B in A  with the 'quirky example of the 'catalouge'
                                         by highlighting finds in A with a
                                         random number generator. 
                                         
     
##### sort and filter any [String] ~ [[Char]] with similaritYValue in function 'kArmTrack5' = gh1
    thought experiment
      gh: ["A","B","A","A"] to ["A","A","A","B"]
      OR
      gh1:= ["AAABB","AAABAB","AAA","BBBAA"] => 'kArmTrack5' -> phiMax -> ["AAABB","AAABAB","BBBAA","AAA"] 
##### add a new element to the list above: 
      addGh:Int ; if Int == 1 then run kArmTrack5 show occurance fst atom ("A") throughout the whole bonelist
                                   without ghCheck in display
                  else if Int==2 then like above but add new line (ghAdd) to a bonelist: ghCheck in display 
                  else not
      ghAdd: String , e.g "BBB"            

##### plot graphs writing to wxmaxima
      kArmTrack5 and M.writeWXCloudNODE

##### connect to openProcessing in index.html
##### How to write a filter for index.html 
    four colors : reduced,green,red,blue
    are used to indicate the dimension of a matrix: tsRAW (ptc)
    There are 9 different ptc functions each is a given matrix.
    criteria:
        tsRAW := 100 points of a point cloud (ptc) -> (ptc 100)->
                 -> filter all occourcances > 1 with nub ->
                ( -> group so that digits next to each other
                    are [String] )
                  -> due to three zipper functions the reduced 
                      state is set to :3  -> as reduced
                      the green to:  6  -> as 1
                          red   to: 93  ->    2
                          blue  >   93  ->    3
                    
     tsRAW pt9c:= [1 (ptc9) 1)..100 (ptc9 100)]
        *>length(nub(concat(tsRAW ptc9)))
        *> 3
                    
 ###### There is a syntactic level and a conceptual level

        =======================================================================================================================
                                  SYNTAX                  |             CONCEPT
        =======================================================================================================================
        runKBASE  the main function that handes all       |   data selection printing and mostly used in main IO
                  kArmTrack5 and M.writeWXCloudNODE       |  
                  via  'defSearch'                        |   defSearch <- defSearchRAW (kaRmTrack5,
                                                                                         M.writeWXCloudNODE  )  
                    e.g > let li2 = [li,["AAA","AAA","CCC","FFFFF"]]                                                               
                    main> let mybase addGH ghAdd n = runKBASE 1 [1..(length li)] 1 addGh ghAdd n li2 1 2 3 4 
 ------------------------------------------------------------------------------------------------------------------------
    
 ######  Examine
      run 
      mybase 2 ghAdd n : change bonelist, make it crash and handle error
        >let myTest1 ghAdd = mybase 2 ghAdd 1  -- set to print screen with ghCheck 
    e.g >myTest1 "ZZZ" 
  ###### b.
     
      in function 'kArmTest5'  sort a variable called 'ghCheck' in the bonelist 
      ghCheck :: String,   
      via operation 'sortEmInput'
      example 1
       before:  ["AAABB","AABAB","AAA","BBBAA"]   with ghCheck = "BBB"
       after: ["AAABB","AABAB","AAA","right","BBBAA"] => yields new order
      example 2
      
       before:  ["AAABB","AABAB","AAA","BBBAA"]   with ghCheck = "ZZZ"
       after: ["AAABB","AABAB","left","AAA","BBBAA"] => yields new order
       
       
   ##### Step II
   ###### data type:'Punkt'
      - define a structure that is called 'formation'
      - which uploads functions into the main pipe and applies them to the data
      - when buiding the final 'Punkt' structure these are different
        computations that can be used with '[father,mother...' 
       
         -- to be used with pg-functions
        formation :: [String] -> Punkt
     *> formation e = Punkt "formation" (Just (basis2 1 e)) (Just (basis2 2 e)) (Just (basis2 3 e)) (Just (basis2 4 e)) (Just (basis2 5 e)) 
      
         -- inserted into 'kArmTest5'
         formTest :: [Punkt -> Maybe Punkt] -> Int -> [String] -> [[Char]] -> [[Char]]


#####   ABSTRACT

######  Function:'kArmTest'

STEP 2: An abstact overview of the content of 'kArmTest5' in 'Colored_2_3_5_Counter20.hs'
with I . CHAIN DISTRIBUTION
     II. SIMILARITYVALUE 
         rate with simiYvalue to compare  all atoms of all lines to the same metric
         
the matrix alike structures below represent lists not matrices 
where   a a a a = line1 ; b b b b = line2 ... x x x x = line n 

    -------------------------------------------
          read  |   complement |  rate with                   
       bonelist |   fst ..last |  simiYvalue              
    ------------------------------------------------
      a a a a   |   a1 a2 a3   |  a a a       a01 a01 a01           |a0 b0 c0|           b0 a0 c0     
      b b b b  --\  b0 b2 b3 --\  b b b --\   a01 a01 a01 --\ MONAD |a1 b1 c2| --\ MAYBE a1 c1 1b --\
      c c c c  --/  c0 c1 c3 --/  c c c --/   a3  a3  a3  --/       |a2 b1 c3| --/  e.g  b2 c2 a2 --/
      d d d d   |   d0 d1 d2   |  d d d       a2  a2  a2            |a4 b4 c4|           c3 a3 b2 
    ======================================================================================================
              |a0 b0 c0|           b0 a0 c0       
    --\ MONAD |a1 b1 c2| --\ MAYBE a1 c1 1b --\
    --/       |a2 b1 c3| --/  e.g  b2 c2 a2 --/ 
    phiMax:
    [[0],[0],[],[]]
    [[0],[0],[],[]]
    [[],[],[],[0]]
    [[],[],[0],[]]   _____________________________________________________________________
    [[0],[0],[],[]]
    [[0],[0],[],[]]
    [[],[],[],[0]] 
    [[],[],[0],[]]   _____________________________________________________________________
    [[],[],[],[0]]    ->  [0.3048780487804878,0.3048780487804878,40.54878048780488,0.0]   
    [[],[],[],[0]]    -> [2,2,3,1]  => phiMax 
    [[0],[0],[],[]]
    [[],[],[],[]]     
    [[],[],[0],[]]   _____________________________________________________________________
    [[],[],[0],[]] 
    [[],[],[],[]]
    [[0],[0],[],[]]                       
    
     All boils down to this depending on bonelist. Every atom 
     appears. kArmTest5 := max (map (similaritYvalue) bonelist) ->
                       -> athing = (map similaritYvalue bonelist)
                       -> phiMax: start with (map max athing)->
                       => (pick1 phiMax)`elemIndices` (phiMax) -> 
                       -> phiMax-1
    ========================================================

##### Step III
###### Concept plotter

    get 4 lines of bonelist as variables  progVar1
                                         ,progVar1++progVar2
                                         ,progVar2
                                         ,progVar3
                                         ,porgVar3++progVar4
                                         ,progVar4
![alt tag](https://github.com/CBroemse/SimilaritYWriter20/blob/master/source/conceptPlotter.png)

###### How things are compared

       pointCloud01
 
             -- wohlGeor1    <-->    wohlGeor2    <->    wohGeor1
             --   I                   I+II               III
              progVar1         progVar!++progVar2       progVar2
       
        amatrix n m = concat [(atrix0 n m),(atrix1 n m),(atrix2 n m)]

   
                                                       amatrix -> ptc0 => ?progVar? group vector
        --------------------------------------------------------------+
                            
        wohlGeor3 ->atrixCo -> amatrix2 -> changs ->  chgLine  -> ptc4
                            -> amatrix2 ->                        ptc2
        ---------------------------------------------------------------                    
               -> amatrix2 -> the Trix -> changs2 -> chgLine2 -> ptc5
               find .... in matrix2                                     => vector
                
                                        -> changs2 -> chgLine2 -> ptc6 =>  interesting

                                        -> changs2 -> chgLine2 -> ptc7 => half 'crown'
                                                                  ptc8 => half 'crown' similar to ptc7
                                                                  ptc9 => half 'crown' similar to ptc8
        ---------------------------------------------------------------                                 
                      theTrix 2 -> ptc3                               =>  progVar group vector (5 steps 1 vector ?)
                 
                      theTrix 4 -> ptc3a                                 
                      --    IV   <->      IV++VI  <->  VI
             atrix4a t n = atrix0R theGeors 1 t theVarias 4 n
             atrix5a t n = atrix0R theGeors 1 t theVarias 5 n
             atrix6a t n = atrix0R theGeors 1 t theVarias 6 n

                                              amatrixDifa  -> ptc3a    => sin of progVars ?     
                                              theTrix 6    -> ptc3b    => 
         

##### Step IV

Now we try to define rules concerning the metric' above. One guidline is
to use bone list as a mode but keep 'realy big' input like below at bay:

      e.g*> listBackofHead = map show [1..1978419655660313589123979]
         *> lineTerror= map show [[1..1978419655660313589123979]]
      
 above is a relation between 19 and the floating numbers ?!. The number above is 19^19.
 If we would define prime number groups until 19 ( 19 is special with prime numbers:)  
 , the beginnig three digits of one fictive prime number group sorting system until
 19, would be 2 3 5 making 1 step on the sieve below..
         
  Is there a way to 'leapfrog' the shortest path whithout reading every atom of bonelist? 
  
  #### concept to build rules for the case
  
![alt tag](https://github.com/CBroemse/SimilaritYWriter20/blob/master/source/2_3_5_1.png)

      aSieve :: Num b => Int -> [(String, b)]
      
      aSieve2 :: (Num a, Num b) => Int -> [(a, b)]

#### So what is it

    To me the thing above is a metric a 'trivial machiene' sort of speak yet it looks funky.
    The intend was to symbolize the computation that all numbers on the Haskell side became 
    their own computation. It is tempting to assemble whole programs that way, maybe its 
    like an old assembler language but that I need to ask more experienced peers. 
    However it 'aint a walk in the park' and the number of posssibities is staggering.
    Now the Haskell type declaration system with its syntax seems much more appealing. I believe the solution if I ought to   
    understand it must be in the Haskell types with simYvalues first and secondarily to apply this metric
    to compare the different domains found and visiualize their effects. So back to 'kArmTrack5'
    map ancestory:
    
    This last step is more for visiualization 
    in wxmaxima
                  (%13)    y: gramschmidt(Mabove);
                  (y)	[[17.08494208494209,5.427509293680298,3.727369542066028],
                  [-9704899/(3^3*127*641),(2^4*11*154789)/(3*3767609),(2^4*1967323)/1881619],
                  [-(2^5*3*23*127*641*1939312688974051*10709394715085445829388803209977114513)/
                      (97*149*151*157*883*21433*1231093*104756501*239289735677*4353051074923305463),
                    (2*1013*952597*3767609*14406767*13031972297*30309630400722546075373424171)/
                      (3*97*149*151*157*883*21433*1231093*104756501*239289735677*4353051074923305463),
                   -(2^2*7*1881619*85024124848484189*663589211299840294699319006606036441)/
                      (97*149*151*157*883*21433*1231093*104756501*239289735677*4353051074923305463)]]
                  => almost complete prime factorization - first line
                  
                  in ghc
                  *Colored_2_3_5_Counter20> let gramS = [[17.08494208494209,5.427509293680298,3.727369542066028],
                  [-9704899/(3^3*127*641),(2^4*11*154789)/(3*3767609),(2^4*1967323)/1881619],
                  [-(2^5*3*23*127*641*1939312688974051*10709394715085445829388803209977114513)/
                    (97*149*151*157*883*21433*1231093*104756501*239289735677*4353051074923305463),
                    (2*1013*952597*3767609*14406767*13031972297*30309630400722546075373424171)/
                    (3*97*149*151*157*883*21433*1231093*104756501*239289735677*4353051074923305463),
                    -(2^2*7*1881619*85024124848484189*663589211299840294699319006606036441)/
                     (97*149*151*157*883*21433*1231093*104756501*239289735677*4353051074923305463)]]
                     
                   *C..> let ofgramS atom line = (head $ausw atom(head$ausw line gramS))
                   *C..> let foGb gb =  map realToFrac (map ord gb ) 
                   *C..> let simiVals gb1 gb2  = similaritYvalue (foGb gb1) (foGb gb2)
                   *C..> let aptc = maximum (concat(nub(ptc6 25)))  -- set to maximum can place other ptc here
                   *C..> let trivMac s r = simiVals (show aptc) (show(ofgramS s r)) 
                   *C..> let pTriv s = map (trivMac s) [1..3]  
                   *C..> let allGramschmidt = map pTriv [1..3] 
                   *C..> allGramschmidt
                   [[5.347593582887701,4.979674796747967,5.0761421319796955], 
                    [4.491978609625669,4.812834224598931,0.6376195536663124],
                    [5.454545454545455,1.371308016877637,3.409090909090909]]  -- now can be turned to step IV
                    
                    -- Step IV : turn 'allGramschmidt with 'minkowskiAdd to SVG/Html 
                    *C..> let sieveGramMink k1 k2 = aSieve (minkLike (allGramschmidt) k1 k2 )
                    *C..> let foSieveGM k1 = map (sieveGramMink k1) [1..3]
                    *C..> let sieveGM = map foSieveGM [1..3]
                    *C..> sieveGM
                    [[[("\"red\"",1),("blue",3),("\"green\"",3)],[("blue",2),("\"red\"",3),("\"green\"",3)],[("blue",1),("\"red\"",2),("\"green\"",5)]],
                     [[("\"red\"",2),("\"green\"",1),("\"green\"",4)],[("blue",2),("\"green\"",3),("\"red\"",4)],[("\"green\"",2),("\"red\"",2),("\"red\"",3)]],
                     [[("\"red\"",1),("blue",3),("\"green\"",3)],[("blue",1),("\"red\"",2),("blue",4)],[("\"red\"",2),("blue",1),("\"red\"",5)]]]
                   
                     
                  

------------------------------------------------------------------------

--------------------------------------------------------------------------
A function that can compare any two values:

Gaussian influenced - Aehnlichkeitswahrscheinlichkeit
this function rates the similarity of two
lists of the same list compares them und gives back a percentage of the "Einerstelle"

  ##### 'Colored_2_3_5_Countrer20':= C*
  
     C*> cond1 a b = (a-b)
      *> cond2 a b = (b-a)
      
 ##### 'similaritYvalue' ('src/Colered_2_3_5_Counter.hs')

*turn into  % with a < b and b < a  

    *>simiValF a b = let a1 = if a > b then ((cond1 a b)/ (a/100) )
                              else if a<b then  ((cond2 a b)/ (b/100) )
                              else 0
                     in let b1 g h = ((g) / ((h)/100)) 
                     in a1 

 
    *> similaritYvalue li la = let a = la -- e.g [11,12,34,44]
                          in let b = li 
                          in let c1 w = sum w
                          in let c2 = c1 a   -- length
                          in let c3 = c1 b--length
                          in simiValF c2 c3 
###### install  
    prepare the path to use 2 main modules
    a) src/Colored_2_3_5_Counter20.hs
    b) src/TheAtoBplotter.hs
    *Main>root
    *Main>/c:/stack/SimilaritYWriter20/src/
    *Main>:t root
    *Main>:i root
    Change the function 'root' to your system 
    
###### Programmers nodes
Status Q
Shurely the 'Punkt' data type can do error handling but is it useful this way or is another way to handle errors better?
