function cleanEquation(string) {
	if(string == ""){return 0}
	else{
		//xPart = (string.split('='))[1]
		
		restart = true;
		
		yPart = string.substr(0,string.indexOf('='));
		yPart = applyRegex(yPart)
		
		xPart = string.substr(string.indexOf('=')+1)
		xPart = applyRegex(xPart)
		//print(xPart)
		x = 5
		z = 5
		y = 5
		correct = true;	
		try {
  	  eval(xPart);
			eval(yPart);
		} 
		catch (e) {
			correct = false;
  	  print("Not a valid equation!");
		}	
		if(correct){
			if(yPart.split(' ').join('') == "y"){
				return [xPart]
			}
			else{
				return [xPart, yPart]	
			}
		}
		else {
			return 0
		}
	}
}

function plotEllipse(string, x, z) {
	string1 = string[0]
	string2 = string[1]
	if(Math.abs(x) <= 25 && Math.abs(z) <= 25){
		for(var y = -10; y < 10; y += 0.2){
			if(Math.abs(eval(string1)-eval(string2)) <= 2){
				plotX = (x*w)
				plotY = (y*yw*-1)
				plotZ = (z*w)
				//cArray.push([x,y,z])
				col = map(dist(0,0,0,plotX,plotY,plotZ), 0, 250, 0, 360)
				stroke(col, 100, 100)
			 
				push()
				translate(plotX, plotY, plotZ)
				sphere(1)
				pop()
				
			}
		}
	}
}


function plotEquation(string, x, z) {
	string1 = string[0]
	if(string){
		var y = eval(string1)*-1		
		plotX = (x*w)
		plotY = (y*yw)
		plotZ = (z*w)
		col = map(dist(0,0,0,plotX,plotY,plotZ), 0, 250, 0, 360)
		stroke(col, 100, 100)
		if(!cubes){vertex(plotX, plotY, plotZ)}
		else{
			push()
			translate(plotX, plotY, plotZ)
			box(2)
			pop()
		}
	}
}

function applyRegex(string) {
	var temp = string
	//temp = temp.replace("y = ", "")
	temp = temp.replace(/(\w*\([^\s()]+\)|[^\s+()]+)\^(\w*\([^\s()]+\)|[^\s,+()]+)/g, 'pow($1, $2)');			
	temp = temp.replace(/\be\b/g, "Math.E")
	temp = temp.replace(/\bfc\b/g, "frameCount")
	temp = temp.replace("pi", "Math.PI")
	//temp = temp.replace(/y/g, "z")
	return temp
}
function plotPunktNu(string, x, z) {
string1 = string[0]
string2 = string[1]
if(Math.abs(x) <= 25 && Math.abs(z) <= 25){
   for(var y = -10; y < 10; y += 0.2){
         if(Math.abs(eval(string1)-eval(string2)) <= 2){
                   plotX = (x*w)
                   plotY = (y*yw*-1)
                   plotZ = (z*w)
                   //cArray.push([x,y,z])
                   col = map(dist(0,0,0,plotX,plotY,plotZ), 0, 0, 0, 60)
                   stroke(col, 5, 5)
push()
translate(plotX +6.7114093959731544, plotY +10.506208213944603, plotZ +-52.60617760617761)
sphere(1)
pop()

push()
translate(plotX +4.595588235294118, plotY +25.089605734767026, plotZ +-52.84926470588235)
sphere(1)
pop()

push()
translate(plotX +0.0, plotY +58.927000879507474, plotZ +-35.04672897196262)
sphere(1)
pop()

push()
translate(plotX +34.83432455395072, plotY +91.7587085811385, plotZ +-85.38657604078166)
sphere(1)
pop()

push()
translate(plotX +16.21160409556314, plotY +17.064846416382252, plotZ +-81.48464163822526)
sphere(1)
pop()

push()
translate(plotX +7.1301247771836005, plotY +19.366197183098592, plotZ +-105.47576301615798)
sphere(1)
pop()

push()
translate(plotX +3.524229074889868, plotY +59.06821963394343, plotZ +-92.39610963748895)
sphere(1)
pop()

push()
translate(plotX +27.23404255319149, plotY +127.65957446808511, plotZ +-69.7872340425532)
sphere(1)
pop()

push()
translate(plotX +0.9216589861751152, plotY +86.70033670033669, plotZ +-48.8479262672811)
sphere(1)
pop()

push()
translate(plotX +1.855287569573284, plotY +46.01769911504425, plotZ +-45.91836734693878)
sphere(1)
pop()

push()
translate(plotX +6.5055762081784385, plotY +20.036429872495447, plotZ +-42.12765957446808)
sphere(1)
pop()

push()
translate(plotX +11.054421768707483, plotY +27.51504729148753, plotZ +-94.58297506448838)
sphere(1)
pop()

push()
translate(plotX +37.30017761989343, plotY +75.87382779198636, plotZ +-68.7269372693727)
sphere(1)
pop()

push()
translate(plotX +3.5650623885918002, plotY +9.70873786407767, plotZ +-86.00713012477718)
sphere(1)
pop()

push()
translate(plotX +61.344537815126046, plotY +75.63025210084034, plotZ +-81.5126050420168)
sphere(1)
pop()

push()
translate(plotX +43.975373790677224, plotY +53.64995602462621, plotZ +-64.64379947229551)
sphere(1)
pop()

push()
translate(plotX +36.639857015192135, plotY +54.898648648648646, plotZ +-39.32082216264522)
sphere(1)
pop()

push()
translate(plotX +10.859728506787329, plotY +75.31380753138076, plotZ +-84.16289592760181)
sphere(1)
pop()

push()
translate(plotX +25.77777777777778, plotY +119.11111111111111, plotZ +-77.33333333333333)
sphere(1)
pop()

push()
translate(plotX +10.46207497820401, plotY +72.36268526591107, plotZ +-73.23452484742808)
sphere(1)
pop()

push()
translate(plotX +61.61971830985916, plotY +83.48968105065666, plotZ +-47.53820033955858)
sphere(1)
pop()

push()
translate(plotX +4.464285714285714, plotY +10.647737355811891, plotZ +-52.46636771300448)
sphere(1)
pop()

push()
translate(plotX +7.352941176470588, plotY +43.401240035429595, plotZ +-52.77777777777778)
sphere(1)
pop()

push()
translate(plotX +25.662959794696324, plotY +28.985507246376812, plotZ +-111.50131694468831)
sphere(1)
pop()

push()
translate(plotX +42.58943781942079, plotY +86.29893238434164, plotZ +-85.40925266903913)
sphere(1)
pop()

push()
translate(plotX +3.508771929824561, plotY +50.87719298245614, plotZ +-91.66666666666666)
sphere(1)
pop()

push()
translate(plotX +42.03323558162268, plotY +63.18681318681318, plotZ +-44.523597506678534)
sphere(1)
pop()

push()
translate(plotX +42.06836108676599, plotY +69.7872340425532, plotZ +-49.40530649588289)
sphere(1)
pop()

push()
translate(plotX +7.9646017699115035, plotY +14.15929203539823, plotZ +-66.3716814159292)
sphere(1)
pop()

push()
translate(plotX +15.775635407537248, plotY +99.03593339176162, plotZ +-66.17002629272568)
sphere(1)
pop()

push()
translate(plotX +17.5, plotY +98.33333333333334, plotZ +-113.33333333333334)
sphere(1)
pop()

push()
translate(plotX +23.430178069353328, plotY +60.73943661971831, plotZ +-72.63355201499532)
sphere(1)
pop()

push()
translate(plotX +19.301470588235293, plotY +40.56437389770723, plotZ +-39.373412362404736)
sphere(1)
pop()

push()
translate(plotX +17.070979335130275, plotY +42.16867469879518, plotZ +-57.95148247978436)
sphere(1)
pop()

push()
translate(plotX +39.79678238780694, plotY +51.83946488294314, plotZ +-71.8694885361552)
sphere(1)
pop()

push()
translate(plotX +25.662959794696324, plotY +84.28446005267779, plotZ +-52.67778753292362)
sphere(1)
pop()

push()
translate(plotX +17.69165964616681, plotY +60.89193825042882, plotZ +-102.48713550600343)
sphere(1)
pop()

push()
translate(plotX +82.3327615780446, plotY +85.47008547008546, plotZ +-60.74766355140187)
sphere(1)
pop()

push()
translate(plotX +35.587188612099645, plotY +47.45166959578207, plotZ +-41.974169741697416)
sphere(1)
pop()

push()
translate(plotX +49.68944099378882, plotY +55.55555555555556, plotZ +-41.083099906629315)
sphere(1)
pop()

push()
translate(plotX +57.88590604026845, plotY +87.24832214765101, plotZ +-83.89261744966444)
sphere(1)
pop()

push()
translate(plotX +2.544529262086514, plotY +39.016115351993214, plotZ +-55.13146734520781)
sphere(1)
pop()

push()
translate(plotX +1.6849199663016008, plotY +36.28691983122363, plotZ +-114.76793248945148)
sphere(1)
pop()

push()
translate(plotX +39.43377148634985, plotY +43.520309477756285, plotZ +-66.9877408056042)
sphere(1)
pop()

push()
translate(plotX +43.24801412180052, plotY +47.89915966386555, plotZ +-82.5242718446602)
sphere(1)
pop()

push()
translate(plotX +3.7037037037037033, plotY +46.09929078014184, plotZ +-38.56877323420075)
sphere(1)
pop()

push()
translate(plotX +42.40282685512368, plotY +48.28797190517997, plotZ +-43.81918819188192)
sphere(1)
pop()

push()
translate(plotX +22.104332449160037, plotY +24.691358024691358, plotZ +-54.249547920434)
sphere(1)
pop()

push()
translate(plotX +37.41496598639456, plotY +37.41496598639456, plotZ +-107.14285714285715)
sphere(1)
pop()

push()
translate(plotX +0.0, plotY +52.27655986509276, plotZ +-84.07473309608541)
sphere(1)
pop()

push()
translate(plotX +0.8795074758135445, plotY +52.77044854881267, plotZ +-88.39050131926122)
sphere(1)
pop()

push()
translate(plotX +5.089058524173028, plotY +111.67945439045182, plotZ +-70.33248081841433)
sphere(1)
pop()

push()
translate(plotX +35.55555555555556, plotY +40.92071611253197, plotZ +-63.55555555555555)
sphere(1)
pop()

push()
translate(plotX +16.55933762649494, plotY +86.99719363891488, plotZ +-55.32445923460899)
sphere(1)
pop()

push()
translate(plotX +28.293545534924842, plotY +30.008826125330977, plotZ +-16.710642040457344)
sphere(1)
pop()

push()
translate(plotX +16.778523489932887, plotY +57.04697986577181, plotZ +-114.09395973154362)
sphere(1)
pop()

push()
translate(plotX +63.829787234042556, plotY +85.76480990274094, plotZ +-48.471615720524014)
sphere(1)
pop()

push()
translate(plotX +9.540329575021683, plotY +45.09973980919341, plotZ +-73.72072853425846)
sphere(1)
pop()

push()
translate(plotX +39.180765805877115, plotY +56.53382761816498, plotZ +-47.729379054680265)
sphere(1)
pop()

push()
translate(plotX +14.1718334809566, plotY +14.1718334809566, plotZ +-63.34231805929918)
sphere(1)
pop()

push()
translate(plotX +11.226252158894646, plotY +41.04803493449782, plotZ +-49.78165938864629)
sphere(1)
pop()

push()
translate(plotX +16.695957820738137, plotY +49.209138840070295, plotZ +-92.2671353251318)
sphere(1)
pop()

push()
translate(plotX +6.39853747714808, plotY +111.51736745886654, plotZ +-60.786106032906766)
sphere(1)
pop()

push()
translate(plotX +37.50000000000001, plotY +55.21472392638037, plotZ +-115.49165120593692)
sphere(1)
pop()

push()
translate(plotX +54.3970988213962, plotY +73.71225577264654, plotZ +-58.485139022051776)
sphere(1)
pop()

push()
translate(plotX +28.31858407079646, plotY +37.686240140227866, plotZ +-91.07468123861565)
sphere(1)
pop()

push()
translate(plotX +8.81057268722467, plotY +34.863945578231295, plotZ +-58.59030837004405)
sphere(1)
pop()

push()
translate(plotX +5.942275042444822, plotY +7.582139848357203, plotZ +-112.90322580645162)
sphere(1)
pop()

push()
translate(plotX +88.17829457364341, plotY +133.50125944584383, plotZ +-79.94186046511628)
sphere(1)
pop()

push()
translate(plotX +19.498607242339833, plotY +49.210770659238634, plotZ +-65.9238625812442)
sphere(1)
pop()

push()
translate(plotX +40.38630377524144, plotY +59.70149253731343, plotZ +-75.50482879719051)
sphere(1)
pop()

push()
translate(plotX +0.916590284142988, plotY +43.02019315188762, plotZ +-45.412844036697244)
sphere(1)
pop()

push()
translate(plotX +34.72840605520926, plotY +51.61854768153981, plotZ +-47.97047970479704)
sphere(1)
pop()

push()
translate(plotX +41.73764906303237, plotY +54.5144804088586, plotZ +-80.9199318568995)
sphere(1)
pop()

push()
translate(plotX +32.50641573994868, plotY +75.27801539777587, plotZ +-97.94696321642431)
sphere(1)
pop()



                 }
              }
            }
         }


