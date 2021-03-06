/*
* @name Geometries
* @description There are six 3D primitives in p5 now.
*/

let ae = 111
let be = 10

// this class describes the properties of a single particle.
class Particle {
// setting the co-ordinates, radius and the
// speed of a particle in both the co-ordinates axes.
  constructor(){
    this.x = random(0,width);
    this.y = random(0,height);
    this.r = random(4,8);
    this.xSpeed = random(-0.5,0.5);
    this.ySpeed = random(-0.5,0.75);
    this.color = random(79,248);
    this.all = (this.color,144,118,0.5)
  }

// creation of a particle.
  createParticle() {
    noStroke();
    fill('rgba(12,144,118,5)');
    circle(this.x,this.y,this.r);
  }

// setting the particle in motion.
  moveParticle() {
    if(this.x < 0 || this.x > width)
      this.xSpeed*=-1;
    if(this.y < 0 || this.y > height)
      this.ySpeed*=-1;
    this.x+=this.xSpeed;
    this.y+=this.ySpeed;
  }

// this function creates the connections(lines)
// between particles which are less than a certain distance apart
  joinParticles(paraticles) {
    particles.forEach(element =>{
      let dis = dist(this.x,this.y,element.x,element.y);
      if(dis<85) {
        stroke('rgba(76,144,118,0.4)');
        line(this.x,this.y,element.x,element.y);
      }
    });
  }
}

// an array to add multiple particles
let particles = [];

function setup() {
  createCanvas(20, 150);
  for(let i = 0;i<7;i++){
    particles.push(new Particle());
  }
}

function draw() {
  background(255);
  for(let i = 0;i<particles.length;i++) {
    particles[i].createParticle();
    particles[i].moveParticle();
    particles[i].joinParticles(particles.slice(i));
  }
}