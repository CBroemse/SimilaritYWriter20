/*
* @name Geometries
* @description There are six 3D primitives in p5 now.
*/

let a = 10.0
let s = 0.0
function setup() {
  let cnv = createCanvas(2000, 900, WEBGL);
  cnv;
  cnv.position(-1000,-400);
}

function draw() {
 
background(0);
a = a + 0.04;
s = (a) * 2;
translate(width/2, height/2, 0);
scale(s);
  fill(51);
stroke(255);
rotateX(PI/2);
rotateZ(-PI/6);
noFill();

beginShape();
vertex(-68.33976833976834,-82.07720588235294,-51.12149532710281);
vertex( 100, -100, -100);
vertex(   0,    0,  100);

vertex( 100, -100, -100);
vertex( 100,  100, -100);
vertex(   0,    0,  100);

vertex( 100, 100, -100);
vertex(-68.33976833976834,-82.07720588235294,-51.12149532710281);
vertex(   0,   0,  100);

vertex(-100,  100, -100);
vertex(-100, -100, -100);
vertex(   0,    0,  100);
endShape();

}