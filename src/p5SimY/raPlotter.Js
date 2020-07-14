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
translate(plotX +0.6711409395973155, plotY +1.0506208213944603, plotZ +10.521235521235521)
sphere(1)
pop()

push()
translate(plotX +0.45955882352941174, plotY +2.5089605734767026, plotZ +10.56985294117647)
sphere(1)
pop()

push()
translate(plotX +0.0, plotY +5.892700087950748, plotZ +7.009345794392524)
sphere(1)
pop()

push()
translate(plotX +3.4834324553950724, plotY +9.17587085811385, plotZ +17.07731520815633)
sphere(1)
pop()

push()
translate(plotX +1.621160409556314, plotY +1.706484641638225, plotZ +16.296928327645052)
sphere(1)
pop()

push()
translate(plotX +0.7130124777183601, plotY +1.9366197183098592, plotZ +21.095152603231597)
sphere(1)
pop()

push()
translate(plotX +0.3524229074889868, plotY +5.906821963394343, plotZ +18.47922192749779)
sphere(1)
pop()

push()
translate(plotX +2.723404255319149, plotY +12.76595744680851, plotZ +13.957446808510639)
sphere(1)
pop()

push()
translate(plotX +9.216589861751152e-2, plotY +8.67003367003367, plotZ +9.76958525345622)
sphere(1)
pop()

push()
translate(plotX +0.1855287569573284, plotY +4.601769911504425, plotZ +9.183673469387756)
sphere(1)
pop()

push()
translate(plotX +0.6505576208178439, plotY +2.0036429872495445, plotZ +8.425531914893616)
sphere(1)
pop()

push()
translate(plotX +1.1054421768707483, plotY +2.751504729148753, plotZ +18.916595012897677)
sphere(1)
pop()

push()
translate(plotX +3.730017761989343, plotY +7.587382779198636, plotZ +13.745387453874539)
sphere(1)
pop()

push()
translate(plotX +0.35650623885918004, plotY +0.970873786407767, plotZ +17.201426024955435)
sphere(1)
pop()

push()
translate(plotX +6.134453781512605, plotY +7.563025210084033, plotZ +16.30252100840336)
sphere(1)
pop()

push()
translate(plotX +4.397537379067723, plotY +5.3649956024626215, plotZ +12.928759894459104)
sphere(1)
pop()

push()
translate(plotX +3.6639857015192137, plotY +5.489864864864865, plotZ +7.864164432529044)
sphere(1)
pop()

push()
translate(plotX +1.085972850678733, plotY +7.531380753138076, plotZ +16.832579185520363)
sphere(1)
pop()

push()
translate(plotX +2.577777777777778, plotY +11.911111111111111, plotZ +15.466666666666667)
sphere(1)
pop()

push()
translate(plotX +1.046207497820401, plotY +7.236268526591107, plotZ +14.646904969485615)
sphere(1)
pop()

push()
translate(plotX +6.161971830985916, plotY +8.348968105065666, plotZ +9.507640067911716)
sphere(1)
pop()

push()
translate(plotX +0.44642857142857145, plotY +1.064773735581189, plotZ +10.493273542600896)
sphere(1)
pop()

push()
translate(plotX +0.7352941176470588, plotY +4.340124003542959, plotZ +10.555555555555555)
sphere(1)
pop()

push()
translate(plotX +2.5662959794696323, plotY +2.898550724637681, plotZ +22.300263388937662)
sphere(1)
pop()

push()
translate(plotX +4.258943781942079, plotY +8.629893238434164, plotZ +17.081850533807827)
sphere(1)
pop()

push()
translate(plotX +0.3508771929824561, plotY +5.087719298245614, plotZ +18.333333333333332)
sphere(1)
pop()

push()
translate(plotX +4.203323558162268, plotY +6.318681318681318, plotZ +8.904719501335707)
sphere(1)
pop()

push()
translate(plotX +4.206836108676599, plotY +6.9787234042553195, plotZ +9.881061299176578)
sphere(1)
pop()

push()
translate(plotX +0.7964601769911503, plotY +1.415929203539823, plotZ +13.27433628318584)
sphere(1)
pop()

push()
translate(plotX +1.5775635407537247, plotY +9.903593339176162, plotZ +13.234005258545135)
sphere(1)
pop()

push()
translate(plotX +1.75, plotY +9.833333333333334, plotZ +22.666666666666668)
sphere(1)
pop()

push()
translate(plotX +2.343017806935333, plotY +6.073943661971831, plotZ +14.526710402999063)
sphere(1)
pop()

push()
translate(plotX +1.9301470588235292, plotY +4.056437389770723, plotZ +7.874682472480948)
sphere(1)
pop()

push()
translate(plotX +1.7070979335130276, plotY +4.216867469879518, plotZ +11.590296495956872)
sphere(1)
pop()

push()
translate(plotX +3.979678238780694, plotY +5.183946488294314, plotZ +14.373897707231041)
sphere(1)
pop()

push()
translate(plotX +2.5662959794696323, plotY +8.428446005267778, plotZ +10.535557506584723)
sphere(1)
pop()

push()
translate(plotX +1.7691659646166809, plotY +6.089193825042882, plotZ +20.497427101200685)
sphere(1)
pop()

push()
translate(plotX +8.23327615780446, plotY +8.547008547008547, plotZ +12.149532710280374)
sphere(1)
pop()

push()
translate(plotX +3.5587188612099645, plotY +4.745166959578207, plotZ +8.394833948339484)
sphere(1)
pop()

push()
translate(plotX +4.9689440993788825, plotY +5.555555555555555, plotZ +8.216619981325863)
sphere(1)
pop()

push()
translate(plotX +5.7885906040268456, plotY +8.724832214765101, plotZ +16.778523489932887)
sphere(1)
pop()

push()
translate(plotX +0.2544529262086514, plotY +3.9016115351993217, plotZ +11.026293469041562)
sphere(1)
pop()

push()
translate(plotX +0.16849199663016007, plotY +3.628691983122363, plotZ +22.953586497890296)
sphere(1)
pop()

push()
translate(plotX +3.9433771486349847, plotY +4.352030947775629, plotZ +13.397548161120842)
sphere(1)
pop()

push()
translate(plotX +4.324801412180053, plotY +4.7899159663865545, plotZ +16.50485436893204)
sphere(1)
pop()

push()
translate(plotX +0.37037037037037035, plotY +4.609929078014185, plotZ +7.713754646840149)
sphere(1)
pop()

push()
translate(plotX +4.240282685512367, plotY +4.828797190517998, plotZ +8.763837638376383)
sphere(1)
pop()

push()
translate(plotX +2.2104332449160036, plotY +2.4691358024691357, plotZ +10.849909584086799)
sphere(1)
pop()

push()
translate(plotX +3.741496598639456, plotY +3.741496598639456, plotZ +21.42857142857143)
sphere(1)
pop()

push()
translate(plotX +0.0, plotY +5.2276559865092755, plotZ +16.814946619217082)
sphere(1)
pop()

push()
translate(plotX +8.795074758135445e-2, plotY +5.277044854881267, plotZ +17.678100263852244)
sphere(1)
pop()

push()
translate(plotX +0.5089058524173028, plotY +11.167945439045182, plotZ +14.066496163682864)
sphere(1)
pop()

push()
translate(plotX +3.5555555555555554, plotY +4.092071611253197, plotZ +12.71111111111111)
sphere(1)
pop()

push()
translate(plotX +1.655933762649494, plotY +8.699719363891488, plotZ +11.064891846921798)
sphere(1)
pop()

push()
translate(plotX +2.829354553492484, plotY +3.000882612533098, plotZ +3.342128408091469)
sphere(1)
pop()

push()
translate(plotX +1.6778523489932886, plotY +5.704697986577181, plotZ +22.818791946308725)
sphere(1)
pop()

push()
translate(plotX +6.382978723404255, plotY +8.576480990274094, plotZ +9.694323144104803)
sphere(1)
pop()

push()
translate(plotX +0.9540329575021683, plotY +4.509973980919341, plotZ +14.744145706851691)
sphere(1)
pop()

push()
translate(plotX +3.9180765805877114, plotY +5.653382761816498, plotZ +9.545875810936053)
sphere(1)
pop()

push()
translate(plotX +1.41718334809566, plotY +1.41718334809566, plotZ +12.668463611859837)
sphere(1)
pop()

push()
translate(plotX +1.1226252158894645, plotY +4.104803493449782, plotZ +9.956331877729259)
sphere(1)
pop()

push()
translate(plotX +1.6695957820738137, plotY +4.92091388400703, plotZ +18.45342706502636)
sphere(1)
pop()

push()
translate(plotX +0.6398537477148081, plotY +11.151736745886655, plotZ +12.157221206581353)
sphere(1)
pop()

push()
translate(plotX +3.7500000000000004, plotY +5.521472392638037, plotZ +23.098330241187384)
sphere(1)
pop()

push()
translate(plotX +5.43970988213962, plotY +7.371225577264654, plotZ +11.697027804410356)
sphere(1)
pop()

push()
translate(plotX +2.831858407079646, plotY +3.768624014022787, plotZ +18.21493624772313)
sphere(1)
pop()

push()
translate(plotX +0.881057268722467, plotY +3.4863945578231292, plotZ +11.71806167400881)
sphere(1)
pop()

push()
translate(plotX +0.5942275042444822, plotY +0.7582139848357203, plotZ +22.580645161290324)
sphere(1)
pop()

push()
translate(plotX +8.817829457364342, plotY +13.350125944584383, plotZ +15.988372093023255)
sphere(1)
pop()

push()
translate(plotX +1.9498607242339834, plotY +4.921077065923863, plotZ +13.18477251624884)
sphere(1)
pop()

push()
translate(plotX +4.038630377524144, plotY +5.970149253731343, plotZ +15.100965759438102)
sphere(1)
pop()

push()
translate(plotX +9.165902841429881e-2, plotY +4.302019315188762, plotZ +9.082568807339449)
sphere(1)
pop()

push()
translate(plotX +3.472840605520926, plotY +5.161854768153981, plotZ +9.59409594095941)
sphere(1)
pop()

push()
translate(plotX +4.173764906303237, plotY +5.45144804088586, plotZ +16.1839863713799)
sphere(1)
pop()

push()
translate(plotX +3.2506415739948675, plotY +7.527801539777588, plotZ +19.58939264328486)
sphere(1)
pop()



                 }
              }
            }
         }


