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
                   col = map(dist(0,0,0,plotX,plotY,plotZ), 0, 250, 0,360)
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



                 }
              }
            }
         }


