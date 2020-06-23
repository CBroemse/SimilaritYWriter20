# SimilaritYWriter20
## plot graphs writing to wxmaxima
                  
A function that can compare any two lists, The aim is to find the most general rules that can 'maybe' 
help my understanding.

four lines of thought

     I. 
     a. compare all atoms of a [String] called bonelist and order them due to similaritYvalue
     b. insert a new line (ghCheck) into a. do the same as above.
     
     II.
     an inherent data type 'Punkt' :: String -> Maybe String
     that is customized with other functions to fit I,III and IV. The data type , so far
     is only connected to the plotting functions of below.
     
     III.
      library to plot wxmaxima graphs . functions to compare
      a. two lines of bonelist with each other.
      b. compare a to periodic functions called pg functions
         e.g  pg1 x = sin x 
         
 ![alt tag](https://github.com/CBroemse/SimilaritYWriter20/blob/master/source/sinCosSinCos.png)
         
     IV. 
      a 2_3_5_Counter to compare I, II and III with each other

#### Step I
   
  ###### a.
  
      this will be the main example being used throughout this program
     
     *> let li = ["AAABB","AABAB","AAA","BBBAA"]
     *> let bonelist = li
     *> let pi = Punkt "M" Nothing Nothing Nothing Nothing Nothing  -- 
     
     we compare all lines and atoms of the bonlist. To describe certain Types we wil use ' 

            ':=' -- which is equivalent to '::' but ':=' /= '::' thus
     
     kArmTest5 (bonelist) := [type [ord (String)] ] -> [[Int]] -> [[Double]] => [([String],[Double])]
     -- simiariYvalue network
     *> kArmTest5 li 1 pi 1 1 [] "AAA"    -- the last item "AAA" will be the test run
     
 ######  Examine

There is an syntactic level and a conceptual level

    =======================================================================================================================
                                  SYNTAX                  |             CONCEPT
    =======================================================================================================================
       hints:       all Functions in 'e.g-sections' have  |  The 'e.g-sections' should be able to be called via ghc
                    to be takenout of the do functions    |  without a doupt that would help understanding.
                      in 'chainDistribute'    AND         |  see if atom is already in list , dile:
                         'kArmTest5'                      |
    ------------------------------------------------------------------------------------------------------------------------
    given:  row-width (length atom bonelist, 'zeilenlaenge ) 
            pick1:String; a of (a,b) -> compare a to b    | one feasible way is to 'weigh' Strings is with simiaritYvalue
            pick2:String; b of (a,b) -> compare a to b    | The aim is to retrieve a reliable function that will destinct 
                                                          | between any two strings. 
          via ( beRepKEY pick1 pick2 punktList )          |           punktist :: Punkt -> [Punkt] -> not used
                                                                         ausw pick1 bonelist ->  atombonelist 
                                                                           

    e.g Colored*>  ( beRepKEY "1" "2" [] )           
  
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
      a a a a   |   a1 a2 a3   |    a a a     a01 a01 a01           |a0 b0 c0|           b0 a0 c0     
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

how things are compared

        wohlGeor3 ->atrixCo -> progVar3
                ->   atrix3 t m = (F.chooseMQ t (wohlGeor3 (progVar1 ) m))
                ->  atrix3a t m = (F.chooseMQ t (wohlGeor3 (progVar1 ) m))
                ->  atrix3b t m = (F.chooseMQ t (wohlGeor3 (progVar1 ) m))



##### Step IV

Now we try to define rules concerning the metric' above. One guidline is
to use bone list as a mode but keep 'realy big' input like below at bay:

      e.g*> listBackofHead = map show [1..1978419655660313589123979]
         *> lineTerror= map show [[1..1978419655660313589123979]]
      
 above is a relation between 19 and the floating numbers ?!. The number above is 19^19.
 If we would define prime number groups until 19 ( 19 is special with prime numbers:)  
 , the beginnig three digits of one fictive prime number group sorting system until
 19, would be 2 3 5 making 1 step on the sieve below..
         
  Is there a way to 'leapfrog' the shortest path whithout reading every atom of boneist? 
  
  #### concept to build rules for the case
  
![alt tag](https://github.com/CBroemse/SimilaritYWriter20/blob/master/source/2_3_5_1.png)

      aSieve :: Num b => Int -> [(String, b)]
      
      aSieve2 :: (Num a, Num b) => Int -> [(a, b)]

#### So what is it

    To me the thing above is a metric a 'trivial machiene' sort of speak yet it looks funky.
    The intend was to symbolize the computation that all numbers on the Haskell side became 
    their own computation. It is tempting to assemble whole programs that way, maybe its 
    like an old assembler language but that I need to ask more experienced peers. 
    However it aint a 'walk in the park' and the number of posssibities is staggering.
    Now the Haskell type declaration system with its syntax seems much more appealing. I believe the solution if I ought to   
    understand it must be in the Haskell types with simYvalues first and secondarily to apply this metric
    to compare the different domains found and vsiualize their effects. So back to 'kArmTrack5'
    map ancestory
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

###### Programmers nodes
Status Q
There is a certain relation between the Haskell type system and the usage of the Punkt type.
The former being so deep  I am barely scratching the surface.The latter being over and overly repeated by 
me throughout his endevour, to finally being put to a hold.
The thing that mainly stands out in this program attempt with the Punkt type that it helped understanding
but became not used within the main function , more resembeling a 'braket' with less meaning inside
kArmTrack5 but more with its initiatisation and the output that can export some functions of kArmTest5
as do IO'. 
