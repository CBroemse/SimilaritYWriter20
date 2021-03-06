// Stack Blur v1.0
//
// Author: Mario Klingemann <mario@quasimondo.com>
// http://incubator.quasimondo.com
// created Feburary 29, 2004


// This is a compromise between Gaussian Blur and Box blur
// It creates much better looking blurs than Box Blur, but is
// 7x faster than my Gaussian Blur implementation.
//
// I called it Stack Blur because this describes best how this
// filter works internally: it creates a kind of moving stack
// of colors whilst scanning through the image. Thereby it
// just has to add one new block of color to the right side
// of the stack and remove the leftmost color. The remaining
// colors on the topmost layer of the stack are either added on
// or reduced by one, depending on if they are on the right or
// on the left side of the stack. 
//
// If you are using this algorithm in your code please add
// the following line:
// 
// Stack Blur Algorithm by Mario Klingemann <mario@quasimondo.com>

PImage a;
PImage b;

void setup()
{
  a=loadImage("dog.jpg");
  size(a.width, a.height);
  b=new PImage(a.width, a.height);
  fill(255);
  noStroke();
  frameRate(25);
}

void draw()
{
  System.arraycopy(a.pixels,0,b.pixels,0,a.pixels.length);
  fastblur(b,mouseY/4);
  image(b, 0, 0);
}

void fastblur(PImage img,int radius){

  if (radius<1){
    return;
  }
  int[] pix=img.pixels;
  int w=img.width;
  int h=img.height;
  int wm=w-1;
  int hm=h-1;
  int wh=w*h;
  int div=radius+radius+1;

  int r[]=new int[wh];
  int g[]=new int[wh];
  int b[]=new int[wh];
  int rsum,gsum,bsum,x,y,i,p,yp,yi,yw;
  int vmin[] = new int[max(w,h)];

  int divsum=(div+1)>>1;
  divsum*=divsum;
  int dv[]=new int[256*divsum];
  for (i=0;i<256*divsum;i++){
    dv[i]=(i/divsum);
  }

  yw=yi=0;

  int[][] stack=new int[div][3];
  int stackpointer;
  int stackstart;
  int[] sir;
  int rbs;
  int r1=radius+1;
  int routsum,goutsum,boutsum;
  int rinsum,ginsum,binsum;

  for (y=0;y<h;y++){
    rinsum=ginsum=binsum=routsum=goutsum=boutsum=rsum=gsum=bsum=0;
    for(i=-radius;i<=radius;i++){
      p=pix[yi+min(wm,max(i,0))];
      sir=stack[i+radius];
      sir[0]=(p & 0xff0000)>>16;
      sir[1]=(p & 0x00ff00)>>8;
      sir[2]=(p & 0x0000ff);
      rbs=r1-abs(i);
      rsum+=sir[0]*rbs;
      gsum+=sir[1]*rbs;
      bsum+=sir[2]*rbs;
      if (i>0){
        rinsum+=sir[0];
        ginsum+=sir[1];
        binsum+=sir[2];
      } else {
        routsum+=sir[0];
        goutsum+=sir[1];
        boutsum+=sir[2];
      }
    }
    stackpointer=radius;

    for (x=0;x<w;x++){

      r[yi]=dv[rsum];
      g[yi]=dv[gsum];
      b[yi]=dv[bsum];
      
      rsum-=routsum;
      gsum-=goutsum;
      bsum-=boutsum;

      stackstart=stackpointer-radius+div;
      sir=stack[stackstart%div];
      
      routsum-=sir[0];
      goutsum-=sir[1];
      boutsum-=sir[2];
      
      if(y==0){
        vmin[x]=min(x+radius+1,wm);
      }
      p=pix[yw+vmin[x]];
      
      sir[0]=(p & 0xff0000)>>16;
      sir[1]=(p & 0x00ff00)>>8;
      sir[2]=(p & 0x0000ff);

      rinsum+=sir[0];
      ginsum+=sir[1];
      binsum+=sir[2];

      rsum+=rinsum;
      gsum+=ginsum;
      bsum+=binsum;
      
      stackpointer=(stackpointer+1)%div;
      sir=stack[(stackpointer)%div];
     
      routsum+=sir[0];
      goutsum+=sir[1];
      boutsum+=sir[2];
     
       rinsum-=sir[0];
      ginsum-=sir[1];
      binsum-=sir[2];
     
       yi++;
    }
    yw+=w;
  }
  for (x=0;x<w;x++){
    rinsum=ginsum=binsum=routsum=goutsum=boutsum=rsum=gsum=bsum=0;
    yp=-radius*w;
    for(i=-radius;i<=radius;i++){
      yi=max(0,yp)+x;
     
       sir=stack[i+radius];
      
      sir[0]=r[yi];
      sir[1]=g[yi];
      sir[2]=b[yi];
     
      rbs=r1-abs(i);
      
      rsum+=r[yi]*rbs;
      gsum+=g[yi]*rbs;
      bsum+=b[yi]*rbs;
     
      if (i>0){
        rinsum+=sir[0];
        ginsum+=sir[1];
        binsum+=sir[2];
      } else {
        routsum+=sir[0];
        goutsum+=sir[1];
        boutsum+=sir[2];
      }
      
      if(i<hm){
        yp+=w;
      }
    }
    yi=x;
    stackpointer=radius;
    for (y=0;y<h;y++){
      pix[yi]=0xff000000 | (dv[rsum]<<16) | (dv[gsum]<<8) | dv[bsum];

      rsum-=routsum;
      gsum-=goutsum;
      bsum-=boutsum;

      stackstart=stackpointer-radius+div;
      sir=stack[stackstart%div];
     
      routsum-=sir[0];
      goutsum-=sir[1];
      boutsum-=sir[2];
     
       if(x==0){
        vmin[y]=min(y+r1,hm)*w;
      }
      p=x+vmin[y];
      
      sir[0]=r[p];
      sir[1]=g[p];
      sir[2]=b[p];
      
      rinsum+=sir[0];
      ginsum+=sir[1];
      binsum+=sir[2];

      rsum+=rinsum;
      gsum+=ginsum;
      bsum+=binsum;

      stackpointer=(stackpointer+1)%div;
      sir=stack[stackpointer];
     
      routsum+=sir[0];
      goutsum+=sir[1];
      boutsum+=sir[2];
      
      rinsum-=sir[0];
      ginsum-=sir[1];
      binsum-=sir[2];

      yi+=w;
    }
  }
  
  img.updatePixels();
}