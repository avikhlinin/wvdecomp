int XMIN, XMAX, YMIN, YMAX;
int NX, NY;
int *MAP;
int CONNECT;

int (*getPixel)(int, int);
void (*setPixel)(int, int, int);

/**********************************************************************/
int getPixelF (int x, int y)
{
  return MAP[NX*y+x];
}

/**********************************************************************/
int getPixelC (int x, int y)
{
  return MAP[NY*x+y];
}

/**********************************************************************/
void setPixelF (int x, int y, int color)
{
  MAP[NX*y+x]=color;
}

/**********************************************************************/
void setPixelC (int x, int y, int color)
{
  MAP[NY*x+y]=color;
}

/**********************************************************************/
void iniFloodFill (int *img, int nx, int ny, int fortran, int connect)
{
  XMIN = 0; XMAX = nx-1;
  YMIN = 0; YMAX = ny-1;
  NX = nx; NY = ny;
  MAP = img;
  CONNECT = connect;
  if (fortran) {
    getPixel = (int (*)(int,int))getPixelF;
    setPixel = (void (*)(int,int,int))setPixelF;
  } else {
    getPixel = (int (*)(int,int))getPixelC;
    setPixel = (void (*)(int,int,int))setPixelC;
  }    
}

/**********************************************************************/
void inifloodfill_ (int *img, int *nx, int *ny, int *fortran, int *connect)
{
  iniFloodFill (img, *nx, *ny, *fortran, *connect);
}

void ini_flood_fill_ (int *img, int *nx, int *ny, int *fortran, int *connect)
{
  iniFloodFill (img, *nx, *ny, *fortran, *connect);
}


/**********************************************************************/


/* Idea: fill the lines with a loop instead of recursive calls.  */
void FloodFill (int x0, int y0, int fill_color, int original_color) 
{ 
  int color, x, y; 
  if (x0<XMIN || x0>XMAX || y0<YMIN || y0>YMAX ) return;
  x = x0; y = y0; 
  color = (*getPixel)(x, y);
  if (color == original_color) { 
    while (color == original_color) { 
      (*setPixel)(x, y, fill_color); 
      FloodFill(x, y+1, fill_color, original_color); 
      FloodFill(x, y-1, fill_color, original_color);
      if (CONNECT==8) {
	FloodFill(x+1, y+1, fill_color, original_color); 
	FloodFill(x+1, y-1, fill_color, original_color);	
	FloodFill(x-1, y+1, fill_color, original_color); 
	FloodFill(x-1, y-1, fill_color, original_color);	
      }
      x = x - 1; 
      if (x<XMIN) return;
      color = (*getPixel)(x, y); 
    } 
    x = x0 + 1; y = y0; 
    if (x>XMAX) return;
    color = (*getPixel)(x, y); 
    while (color == original_color) { 
      (*setPixel)(x, y, fill_color); 
      FloodFill(x, y+1, fill_color, original_color); 
      FloodFill(x, y-1, fill_color, original_color); 
      if (CONNECT==8) {
	FloodFill(x+1, y+1, fill_color, original_color); 
	FloodFill(x+1, y-1, fill_color, original_color);	
	FloodFill(x-1, y+1, fill_color, original_color); 
	FloodFill(x-1, y-1, fill_color, original_color);	
      }
      x = x + 1; 
      if (x>XMAX) return;
      color=(*getPixel)(x, y); 
    } 
  } 
} 

void floodfill_ (int *x0, int *y0, int *fill_color, int *original_color)
{
  FloodFill (*x0-1, *y0-1, *fill_color, *original_color);
}
