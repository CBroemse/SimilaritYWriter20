/*
* @name Geometries
* @description There are six 3D primitives in p5 now.
*/
function setup() {
  let cnv = createCanvas(1200, 800, WEBGL);
   cnv;
   cnv.position(0,0);
}

function draw() {
  background(250);

  translate(843.5328185328186, 33.14732142857142, 937.2586872586874);
  push();
  rotateX(frameCount * 0.01);
  rotateY(frameCount * 0.01);
  box(7, 1, 1470);
  sphere(3);
  pop();


  translate(33.14732142857142, 937.2586872586874, 843.5328185328186);
  push();
  rotateZ(frameCount * 0.01);
  rotateY(frameCount * 0.01);
  box(7, 1, 3470);
  sphere(3);
  pop();


}