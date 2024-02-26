/*************************************************************************
 * This program computes and averages the RDC for given vectors over
 * different states.
 *************************************************************************/

#include <iostream>
#include <math.h>
#include <fstream>
#include <stdio.h>
#include <stdlib.h>
using namespace std;

#define strSize 50
#define pi      3.141592
#define space "   "
#define tab "\t"

class Nuclei {
private:
  double *x1, *y1, *z1;
  double *X1, *Y1, *Z1;
  double *x2, *y2, *z2;
  double *X2, *Y2, *Z2;
  double *max, *d, *avg_dip, *e;
  int   Dim, MDim;

public: 
  Nuclei(int d=500);
  ~Nuclei();
  void Euler_Rotate(double ,double ,double );
  void Arbit_Rotate (double, double, double, double); 
  void Calculate_Dipole( double, double, double, double);
  void Read_Coord( char *);
  void Write_Coord(char *);
};

Nuclei::Nuclei(int dim) {
MDim = dim;

x1 = new double[MDim];
X1 = new double[MDim];
y1 = new double[MDim];
Y1 = new double[MDim];
z1 = new double[MDim];
Z1 = new double[MDim];

x2 = new double[MDim];
X2 = new double[MDim];
y2 = new double[MDim];
Y2 = new double[MDim];
z2 = new double[MDim];
Z2 = new double[MDim];

max = new double[MDim];
d   = new double[MDim];
e   = new double[MDim];
avg_dip = new double[MDim];

for (int i=0; i < MDim; i++) avg_dip[i] = 0.0;
}

Nuclei::~Nuclei() {
	delete [] x1;
	delete [] X1;
	delete [] y1;
	delete [] Y1;
	delete [] z1;
	delete [] Z1;

	delete [] x2;
	delete [] X2;
	delete [] y2;
	delete [] Y2;
	delete [] z2;
	delete [] Z2;

	delete [] max;
	delete [] d;
	delete [] e;
	delete [] avg_dip;
}

/********************************************************
Perform Euler transformation on the data
*********************************************************/

void Nuclei::Euler_Rotate(double a, double b, double c){

double d[9];
double x, y, z;
int i;

  a *= pi / 180.0;
  b *= pi / 180.0;
  c *= pi / 180.0;

  /* Create direction cosine matrix (this is directly out of Arfken) */
  /* This does z, y, z rotation */

  d[0] = cosf (c) * cosf (b) * cosf (a) - sinf (c) * sinf (a);
  d[1] = cosf (c) * cosf (b) * sinf (a) + sinf (c) * cosf (a);
  d[2] = -cosf (c) * sinf (b);
  d[3] = -sinf (c) * cosf (b) * cosf (a) - cosf (c) * sinf (a);
  d[4] = -sinf (c) * cosf (b) * sinf (a) + cosf (c) * cosf (a);
  d[5] = sinf (c) * sinf (b);
  d[6] = cosf (a) * sinf (b);
  d[7] = sinf (a) * sinf (b);
  d[8] = cosf (b);

  for (i = 0; i < Dim; i++)
    {
      x = x1[i] * d[0] + y1[i] * d[1] + z1[i] * d[2];
      y = x1[i] * d[3] + y1[i] * d[4] + z1[i] * d[5];
      z = x1[i] * d[6] + y1[i] * d[7] + z1[i] * d[8];
      X1[i] = x; Y1[i] = y; Z1[i] =  z;

      x = x2[i] * d[0] + y2[i] * d[1] + z2[i] * d[2];
      y = x2[i] * d[3] + y2[i] * d[4] + z2[i] * d[5];
      z = x2[i] * d[6] + y2[i] * d[7] + z2[i] * d[8];
      X2[i] = x; Y2[i] = y; Z2[i] =  z;
    }

}

void Nuclei::Calculate_Dipole( double Sxx, double Syy, double Szz, double fraction) {
double x, y, z, r;

	for(int i = 0; i < Dim; i++){
  		x = X2[i] - X1[i];
  		y = Y2[i] - Y1[i];
  		z = Z2[i] - Z1[i];
  		r = sqrt(x*x + y*y + z*z);
  		avg_dip[i] += fraction*(x*x*Sxx + y*y*Syy + z*z*Szz) * max[i] / 
			      (r*r*r*r*r); 
	}
}

/********************************************************
Perform rotation of data about an arbitrary axis
*********************************************************/
 
void Nuclei::Arbit_Rotate (double a, double b, double c, double m_theta)
{
  double d[9], norm, ct, st;
  int i;
 
  norm = sqrt (a * a + b * b + c * c);
  a /= norm;
  b /= norm;
  c /= norm;
 
  ct = cosf (m_theta * pi/180);
  st = sinf (m_theta * pi/180);
  d[0] = a * a + ct * (b * b + c * c);
  d[1] = a * b * (1.0 - ct) - c * st;
  d[2] = a * c * (1.0 - ct) + b * st;
  d[3] = a * b * (1.0 - ct) + c * st;
  d[4] = b * b + ct * (a * a + c * c);
  d[5] = c * b * (1.0 - ct) - a * st;
  d[6] = a * c * (1.0 - ct) - b * st;
  d[7] = c * b * (1.0 - ct) + a * st;
  d[8] = c * c + ct * (b * b + a * a);

  for (i = 0; i < Dim; i++)
    {
      X1[i] = x1[i] * d[0] + y1[i] * d[1] + z1[i] * d[2];
      Y1[i] = x1[i] * d[3] + y1[i] * d[4] + z1[i] * d[5];
      Z1[i] = x1[i] * d[6] + y1[i] * d[7] + z1[i] * d[8];

      X2[i] = x2[i] * d[0] + y2[i] * d[1] + z2[i] * d[2];
      Y2[i] = x2[i] * d[3] + y2[i] * d[4] + z2[i] * d[5];
      Z2[i] = x2[i] * d[6] + y2[i] * d[7] + z2[i] * d[8];
    }
}  

void Nuclei::Read_Coord( char fname[strSize] ) {
ifstream infile;
char inLine[300];

    infile.open(fname);
    if(infile.fail()) {
	  cout << "Error openning the file " << fname << " \n";
	  exit(0);
    }

     Dim = 0;
     while(infile.getline(inLine, 300)) {
	sscanf(inLine, "%lf %lf %lf %lf %lf %lf %lf %lf %lf", 
                       &x1[Dim], &y1[Dim], &z1[Dim], 
                       &x2[Dim], &y2[Dim], &z2[Dim], 
                       &d[Dim], &max[Dim], &e[Dim]);
        X1[Dim] = x1[Dim];
        Y1[Dim] = y1[Dim];
        Z1[Dim] = z1[Dim];
        X2[Dim] = x2[Dim];
        Y2[Dim] = y2[Dim];
        Z2[Dim] = z2[Dim];
        Dim++;
    }
}

void Nuclei::Write_Coord( char fname[strSize])
{
ofstream outfile;

  outfile.open(fname);
  if(outfile.fail()) {
    cout << "Error openning the file " << fname << " \n";
    exit(0);
  }

  for(int i = 0; i < Dim; i++) {
     outfile << x1[i] << " " << y1[i] << " " << z1[i] << " ";
     outfile << x2[i] << " " << y2[i] << " " << z2[i] << " ";
     outfile << avg_dip[i] << " " << max[i] << " " << e[i] << endl;
  }
}

int main(int argc, char **argv) {
char inFname[strSize], outFname[strSize], statesFname[strSize];
int Rotation_type, num_states;
double Sxx, Syy, Szz;
double alpha, beta, gamma;
double x, y, z, theta;
double fraction;
ifstream sfile;


if(argc != 8){
   cout << "Usage: " << argv[0] << " <Infile> <outfile> <states file> ";
   cout << "<num states> Sxx Syy Szz" << endl; 
   exit(0);
}

sprintf(inFname, "%s\0", argv[1]);
sprintf(outFname, "%s\0", argv[2]);
sprintf(statesFname, "%s\0", argv[3]);

num_states = atoi(argv[4]);
Sxx = atof(argv[5]);
Syy = atof(argv[6]);
Szz = atof(argv[7]);

if(fabs(Sxx+Syy+Szz) > 1e-6) {
  cout << "Invalid order parameters.\n";
  exit(0);
}

Nuclei n(1000);
n.Read_Coord(inFname);

sfile.open(statesFname);
if(sfile.fail()) {
  cout << "Error openning the file " << statesFname << " \n";
  exit(0);
}

for(int i=0; i < num_states; i++) {
	sfile >> Rotation_type;
	if(Rotation_type == 1) {  // Euler Rotation
		sfile >> alpha >> beta >> gamma >> fraction;
                n.Euler_Rotate(alpha, beta, gamma);
	}
	if(Rotation_type == 0) {  // Rotation about an arbitrary axis
		sfile >> x >> y >> z >> theta >> fraction;
		n.Arbit_Rotate(x, y, z, theta);
	}
        n.Calculate_Dipole(Sxx, Syy, Szz, fraction);
}

n.Write_Coord(outFname);

return(0);
}
