/***************************************************************************
 *   Copyright (C) 2005 by Dr. Homayoun Valafar                            *
 *   homayoun@cse.sc.edu                                                   *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 *                                                                         *
 *    Edited by Chris Schmidt on June 2010                                 *
 ***************************************************************************/

#include "Redcat.h"
#include <algorithm>
#include <cstdlib>
#include <string.h>
#include <cmath>
#include <fstream>
#include <sstream>
#include <iostream>
#include <iterator>
#include <vector>
#include <limits>
#include <random>
#include <unordered_map>
#include <tuple>
namespace redcat {

Redcat::Redcat(int size): Count(size){
  Data  = new double *[Count];
  Comments = new string [Count];

  MatchCount = Count;

  for(int i = 0; i < Count; i++)
    Data[i] = new double [9];
}

Redcat::Redcat(istream & infile) {
    string s;

    //set Count
    Count = 0;
    while (getline(infile, s))
        Count++;

    MatchCount = Count;

    //Reset pointer
    infile.clear();
    infile.seekg(0);

    //set up arrays
    Data = new double *[Count];
    Comments = new string [Count];
    string *stringRDC = new string[Count];
    Matrix tempCoord(Count, 5), tempRDC(Count, 1);
    int avgCount = 0;
    double deltaX, deltaY, deltaZ, r, x, y, xy, xz, yz;
    deltaX = deltaY = deltaZ = r = x = y = xy = xz = yz = 0;
    int j = 0;

    for(int i = 0; i < Count; i++){
        Data[i] = new double [9];
        /*<debug>cout << "printing Data" << endl;<debug>*/
        infile >> skipws >> Data[i][0] >> Data[i][1]
               >> Data[i][2] >> Data[i][3] >> Data[i][4]
               >> Data[i][5] >> stringRDC[i] >> Data[i][7]
               >> Data[i][8];
        getline(infile, s);
        Comments[i] = s;
        /*<debug>*
        cout << Data[i][0] << " " << Data[i][1] << " "
             << Data[i][2] << " " << Data[i][3] << " " << Data[i][4]
             << Data[i][5] << " " << stringRDC[i] << " " << Data[i][7]
             << Data[i][8] << " " << s << endl;
        *</debug>*/
        if(Data[i][0] == 999 || Data[i][1] == 999 || Data[i][2] == 999
               || Data[i][3] == 999 || Data[i][4] == 999 || Data[i][5] == 999){
            /*<debug>*
            cout << "Data " << (i+1) << " " << Data[i][0] << " " << Data[i][1]
                 << " " << Data[i][2] << " " << Data[i][3] << " "
                 << Data[i][4] << " " << Data[i][5] << " "
                 << atof(stringRDC[i].c_str()) << endl;
            *</debug>*/
            if(stringRDC[i]=="AVG"){
                //skip this one
                continue;

            }//end if
            for(int k = 0; k < tempCoord.GetCol(); k++){
                tempCoord.Set_Mij(j, k, 999);

            }//end for
            tempRDC.Set_Mij(j, 0, 999);
            j++;
            continue;

        }//end if
        Data[i][6] = 999;

        deltaX = Data[i][3] - Data[i][0];
        deltaY = Data[i][4] - Data[i][1];
        deltaZ = Data[i][5] - Data[i][2];

        r = sqrt(SQR(deltaX) + SQR(deltaY) + SQR(deltaZ));
        r = r * r * r * r * r;

        x += (SQR(deltaX) - SQR(deltaZ)) * (Data[i][7] / r);
        y += (SQR(deltaY) - SQR(deltaZ)) * (Data[i][7] / r);
        xy += (2 * deltaX * deltaY) * (Data[i][7] / r);
        xz += (2 * deltaX * deltaZ) * (Data[i][7] / r);
        yz += (2 * deltaY * deltaZ) * (Data[i][7] / r);
        avgCount++;

        if (stringRDC[i] != "AVG") {
            //assign
            Data[i][6] = atof(stringRDC[i].c_str());
            tempRDC.Set_Mij(j, 0, Data[i][6]);
            tempCoord.Set_Mij(j, 0, x / avgCount);
            tempCoord.Set_Mij(j, 1, y / avgCount);
            tempCoord.Set_Mij(j, 2, xy / avgCount);
            tempCoord.Set_Mij(j, 3, xz / avgCount);
            tempCoord.Set_Mij(j, 4, yz / avgCount);

            //reset avg
            x = y = xy = xz = yz = 0.0;
            avgCount = 0;
            j++;

        }//end if
        else {
            Data[i][6]=-999;

        }

    }//end for i

    //cout << endl;
    Coordinates = (tempCoord.submatrix(j));
    RDC = (tempRDC.submatrix(j));
    /*<debug>*
    cout << "printing Coordinates\n" << *Coordinates << endl;
    cout << "size: " << Coordinates->GetRow() << ", " << Coordinates->GetCol()
         << endl;
    cout << "printing RDC\n" << *RDC << endl;
    cout << "size: " << RDC->GetRow() << ", " << RDC->GetCol()
         << endl;
    cout << "total size " << Count << " new size " << j << endl;
    cout << "printing CleanCoord\n" << CleanMat(Coordinates) << endl;
    cout << "printing CleanRDC\n" << CleanMat(RDC) << endl;
    cout << "printing Data[i][1-6]" << endl;
    for(int i = 0; i<Count; i++){
        for(int j = 0; j<7; j++){
            cout << Data[i][j] << " ";
        }//end for j
        cout << endl;

    }//end for i
   *</debug>*/

    delete [] stringRDC;

}//end Redcat(istream)

Redcat::Redcat(const Redcat &rhs):Count(rhs.Count){
  Data  = new double *[Count];
  Comments = new string [Count];

  for(int i = 0; i < Count; i++)
    Data[i] = new double [9];

  for(int i = 0; i < Count; i++)
  {
    for(int j = 0; j < 9; j++)
      Data[i][j] = rhs.Data[i][j];
    Comments[i] = rhs.Comments[i];
  }
  Coordinates = rhs.Coordinates;
  RDC = rhs.RDC;

  Max = rhs.Max;
  Min = rhs.Min;
  Mean = rhs.Mean;
  Std = rhs.Std;
}

Redcat::~Redcat(){
  for(int i = 0; i < Count; i++)
    delete [] Data[i];
  delete [] Data;
  delete [] Comments;
  delete RDC;
  delete Coordinates;
}

Redcat & Redcat::operator= (const Redcat & rhs){

  if (Count != rhs.Count)
  {
    cout << "Miss-matched dimensions in = operator.\n";
    exit(1);
  }

  for(int i = 0; i < Count; i++)
  {
    for(int j = 0; j < 9; j++)
      Data[i][j] = rhs.Data[i][j];
    Comments[i] = rhs.Comments[i];
  }

  Max = rhs.Max;
  Min = rhs.Min;
  Mean = rhs.Mean;
  Std = rhs.Std;

  return *this;
}

ostream & operator<< (ostream & s, const Redcat & rhs){

  //s.setf(ios::scientific);
  //s.precision(10);
  for (int i = 0; i < rhs.Count; i++)
  {
    for (int j = 0; j < 9; j++)
      s << rhs.Data[i][j] << " ";
    s << rhs.Comments[i] << endl;
  }
  return s;
}

istream & operator>> (istream & s, Redcat & rhs){

  for (int i = 0; i < rhs.Count; i++)
  {
    s >> rhs.Data[i][0] >> rhs.Data[i][1] >> rhs.Data[i][2];
    s >> rhs.Data[i][3] >> rhs.Data[i][4] >> rhs.Data[i][5];
    s >> rhs.Data[i][6] >> rhs.Data[i][7] >> rhs.Data[i][8];
    getline(s, rhs.Comments[i]);
  }
  return s;
}

void Redcat::Prune(Redcat &R){

  for(int i = 0; i < MatchCount; i++)
  {
    if(!(IsValid(i)) || !(R.IsValid(i)))
    {

      Shift(i);
      R.Shift(i);
      MatchCount--;
      i--;
    }
  }
  cout << "Final size: " << MatchCount << endl;
}

void Redcat::Shift(int row){

  int i, j;
  double temp[9];
  string tempcomment;

  for(j = 0; j < 9; j++)
    temp[j] = Data[row][j];
  tempcomment = Comments[row];

  for(i = row; i < (MatchCount-1); i++)
  {
    for(j = 0; j < 9; j++)
      Data[i][j] = Data[i+1][j];
    Comments[i] = Comments[i+1];
  }

  for(j = 0; j < 9; j++)
    Data[MatchCount-1][j] = temp[j];
  Comments[MatchCount-1] = tempcomment;
}

bool Redcat::IsValid(int row){
  for(int i = 0; i < 6; i++)
    if(Data[row][i] == 999)
    {
      return false;
    }
  return true;
}

void Redcat::CalcStat(){
  double sum, sum_sqr;

  Min = 1e6;
  Max = 0;
  sum =  sum_sqr = 0;
  for(int i = 0; i < Count; i++)
  {
    sum += Data[i][6];
    sum_sqr+= Data[i][6] * Data[i][6];
    if(Data[i][6] > Max)
      Max = Data[i][6];
    if(Data[i][6] < Min)
      Min = Data[i][6];
  }
  Mean = sum / Count;
  Std = sqrt((sum_sqr/(Count-1.0) - sum*sum/(1.0*Count*Count - Count))/Count);
}

void Redcat::PrintStat(){
  cout << "Min = " << Min << endl;
  cout << "Max = " << Max << endl;
  cout << "Mean = " << Mean << endl;
  cout << "StdDev = " << Std << endl;
}

void Redcat::PrintVect(){
  for(int i = 0; i < Count; i++)
  {
    cout << "0 0 0 ";
    cout << Data[i][3] - Data[i][0] << " ";
    cout << Data[i][4] - Data[i][1] << " ";
    cout << Data[i][5] - Data[i][2] << endl;
  }
}

/*
Matrix & Redcat::CalcPDP(double sigma, double start, double stop, int intervals)
{
  Matrix *M;
  double x, y;

  M = new Matrix(intervals, 2, 0);
  for(int i = 0; i < intervals; i++)
  {
    x = i * (stop - start) / intervals + start;
    y = 0.0;
    for(int j = 0; j < Count; j++)
    {
      if(Data[j][6] != 999)
      {
        y += exp(-0.5*(x-Data[j][6])*(x-Data[j][6])/(sigma*sigma)) / (sigma * denom);
      }
    }
    M->Set_Mij(i, 0, x);
    M->Set_Mij(i, 1, y/Count);
  }
  return *M;
}

Matrix & Redcat::CalcPDP(Redcat &D2, double sigma1, double start1, double stop1, int intervals1,
                          double sigma2, double start2, double stop2, int intervals2)
{
  Matrix *M;
  double x, y, z;
  double delta1, delta2;
  int index=0;

  delta1 = (stop1-start1)/intervals1;
  delta2 = (stop2-start2)/intervals2;

  M = new Matrix(intervals1 * intervals2, 3, 0);

  for(int i = 0; i < intervals1; i++)
  {
    x = i * delta1 + start1;
    for(int j = 0; j < intervals2; j++)
    {
      y = j * delta2 + start2;
      z = 0.0;
      for(int k = 0; k < MatchCount; k++)
        if((fabs(x-Data[k][6]) <= sigma1) && (fabs(y-D2.Data[k][6]) <= sigma2))
          z++;

      M->Set_Mij(index, 0, x);
      M->Set_Mij(index, 1, y);
      M->Set_Mij(index, 2, z/(2 * sigma1 * 2 * sigma2 * MatchCount)); //Each kernell is 2*sigma1 by 2*simga2.
      index++;
    }
  }
  return *M;
}

double Redcat::CalcDiffPDP(Matrix &MPDP, double sigma, double start, double stop, int intervals)
{
  double x, y;
  double C, M, chi, tchi;
  double delta;
  int index=0;

  Matrix CPDP(intervals, 2, 0);
  CPDP = CalcPDP(sigma, start, stop, intervals);
  delta = (stop-start)/intervals;
  chi = 0.0;

  for(int i = 0; i < intervals; i++)
  {
      M = MPDP.Get_Mij(i, 1);
      C = CPDP.Get_Mij(i, 1);

      tchi= (M - C) * (M - C);
      if(C <= 1e-5)
        tchi *= 100;
      else
        tchi /= C;
      chi += tchi/2;

      tchi= (M - C) * (M - C);
      if(M <= 1e-5)
        tchi *= 100;
      else
        tchi /= M;
      chi += tchi/2;

      index++;
  }
  return (chi);
}

double Redcat::CalcDiffPDP(Redcat &D2, Matrix &MPDP,
                            double sigma1, double start1, double stop1, int intervals1,
                            double sigma2, double start2, double stop2, int intervals2)
{
  double x, y, z;
  double C, M, chi, tchi;
  double delta1, delta2;
  int index=0;

  delta1 = (stop1-start1)/intervals1;
  delta2 = (stop2-start2)/intervals2;
  chi = 0.0;

  for(int i = 0; i < intervals1; i++)
  {
    x = i * delta1 + start1;[-t fstype] something somewhere'.
Details found in /etc/fstab may b
    for(int j = 0; j < intervals2; j++)
    {
      y = j * delta2 + start2;
      z = 0.0;
      for(int k = 0; k < MatchCount; k++)
        if((fabs(x-Data[k][6]) <= sigma1) && (fabs(y-D2.Data[k][6]) <= sigma2))
          z++;

      if(x != MPDP.Get_Mij(index, 0))
      {
        cout << "Error: x indecies do not match\n";
        exit(0)
        ;
      }
      if(y != MPDP.Get_Mij(index, 1))
      {
        cout << "Error: y indecies do not match\n";
        exit(0)
        ;
      }


      M = MPDP.Get_Mij(index, 2);
      C = z/(2*sigma1*2*sigma2*MatchCount);

      tchi= (M - C) * (M - C);
      if(C <= 1e-5)
      {
        tchi *= 100;
      }
      else
      {
        tchi /= C;
      }
      chi += tchi;
      index++;
    }
  }
  return (chi*Count);
}
*/

void Redcat::Rotate(double Sxx, double Syy, double Szz, double a, double b,
                    double c, bool stat){

  double d[9];
  double x1, y1, z1;
  double x2, y2, z2;
  double x, y, z, r;
  int i;

  a *= PI / 180.0;
  b *= PI / 180.0;
  c *= PI / 180.0;

  /* Create direction cosine matrix (this is directly out of Arfken) */
  /* This does z, y, z rotation */
  /* Modified by Chris Schmidt, cosf() and sinf() changed to cos() and sin() */

  d[0] = cos (c) * cos (b) * cos (a) - sin (c) * sin (a);
  d[1] = cos (c) * cos (b) * sin (a) + sin (c) * cos (a);
  d[2] = -cos (c) * sin (b);
  d[3] = -sin (c) * cos (b) * cos (a) - cos (c) * sin (a);
  d[4] = -sin (c) * cos (b) * sin (a) + cos (c) * cos (a);
  d[5] = sin (c) * sin (b);
  d[6] = cos (a) * sin (b);
  d[7] = sin (a) * sin (b);
  d[8] = cos (b);

  for (i = 0; i < Count; i++)
  {
    x1 = Data[i][0] * d[0] + Data[i][1] * d[1] + Data[i][2] * d[2];
    y1 = Data[i][0] * d[3] + Data[i][1] * d[4] + Data[i][2] * d[5];
    z1 = Data[i][0] * d[6] + Data[i][1] * d[7] + Data[i][2] * d[8];
    if(stat)
    {
      Data[i][0] = x1;
      Data[i][1] = y1;
      Data[i][2] = z1;
    }

    x2 = Data[i][3] * d[0] + Data[i][4] * d[1] + Data[i][5] * d[2];
    y2 = Data[i][3] * d[3] + Data[i][4] * d[4] + Data[i][5] * d[5];
    z2 = Data[i][3] * d[6] + Data[i][4] * d[7] + Data[i][5] * d[8];
    if(stat)
    {
      Data[i][3] = x2;
      Data[i][4] = y2;
      Data[i][5] = z2;
    }

    x = x2 - x1;
    y = y2 - y1;
    z = z2 - z1;
    r = sqrt(x*x + y*y + z*z);
    Data[i][6] = (x*x*Sxx + y*y*Syy + z*z*Szz) * Data[i][7] / (r*r*r*r*r) ;
  }
}

void Redcat::Rotate(double a, double b, double c){

  double d[9];
  double x, y, z;
  int i;

  a *= PI / 180.0;
  b *= PI / 180.0;
  c *= PI / 180.0;

  /* Create direction cosine matrix (this is directly out of Arfken) */
  /* This does z, y, z rotation */
  /* Modified by Chris Schmidt, cosf() and sinf() changed to cos() and sin() */

  d[0] = cos (c) * cos (b) * cos (a) - sin (c) * sin (a);
  d[1] = cos (c) * cos (b) * sin (a) + sin (c) * cos (a);
  d[2] = -cos (c) * sin (b);
  d[3] = -sin (c) * cos (b) * cos (a) - cos (c) * sin (a);
  d[4] = -sin (c) * cos (b) * sin (a) + cos (c) * cos (a);
  d[5] = sin (c) * sin (b);
  d[6] = cos (a) * sin (b);
  d[7] = sin (a) * sin (b);
  d[8] = cos (b);

  for (i = 0; i < Count; i++)
  {
    x = Data[i][0] * d[0] + Data[i][1] * d[1] + Data[i][2] * d[2];
    y = Data[i][0] * d[3] + Data[i][1] * d[4] + Data[i][2] * d[5];
    z = Data[i][0] * d[6] + Data[i][1] * d[7] + Data[i][2] * d[8];
    Data[i][0] = x;
    Data[i][1] = y;
    Data[i][2] = z;

    x = Data[i][3] * d[0] + Data[i][4] * d[1] + Data[i][5] * d[2];
    y = Data[i][3] * d[3] + Data[i][4] * d[4] + Data[i][5] * d[5];
    z = Data[i][3] * d[6] + Data[i][4] * d[7] + Data[i][5] * d[8];
    Data[i][3] = x;
    Data[i][4] = y;
    Data[i][5] = z;
  }
}

void Redcat::Rotation(double a, double b, double c){
    a= a*PI/180;
    b= b*PI/180;
    c= c*PI/180;
    Matrix answer = Matrix::findRotation(a, b, c);
    cout << "Rotation matrix: " << endl;
    cout << answer << endl;

}//end Rotation(double, double, double);

void Redcat::Rotation(double x1, double y1, double z1, double x2, double y2, double z2,
                         double x3 ,double y3 ,double z3){
    double a, b, c;
    Matrix R(3,3);
    R.Set_Mij(0, 0, x1);
    R.Set_Mij(0, 1, y1);
    R.Set_Mij(0, 2, z1);
    R.Set_Mij(1, 0, x2);
    R.Set_Mij(1, 1, y2);
    R.Set_Mij(1, 2, z2);
    R.Set_Mij(2, 0, x3);
    R.Set_Mij(2, 1, y3);
    R.Set_Mij(2, 2, z3);

    R.findAngles(a, b, c);
    a= a*180/PI;
    b= b*180/PI;
    c= c*180/PI;

    cout << "A: " << a << " B: " << b << " C: " << c << endl;

}//end Rotation(double, double, double, double, double, double, double ,
//              double ,double);

void Redcat::SolveBest(){
    Matrix *cleanMatArray = CleanMat();
    /*tempCoord(CleanCoord()), tempRDC(CleanRDC())*/
    Matrix decomp, rotation, equationNum = cleanMatArray[0],
           cleanCoord = cleanMatArray[1], cleanRDC = cleanMatArray[2],
           cleanError = cleanMatArray[3];

    /*<debug>*
    cout << "(" << cleanCoord.GetRow() << ", " << cleanCoord.GetCol() << ")" << endl;
    cout << "printing cleanCoord\n" << cleanCoord << endl;
    cout << "printing cleanRDC\n" << cleanRDC << endl;
    *</debug>*/
    
    double a,b,c;

    //compose
    Matrix *lInv = cleanCoord.pseudoInverse();
    Matrix tens = (*lInv * cleanRDC);
    Tensor t(tens);

    //decompose
    t.getMatrix().Diag(decomp, rotation);
    cout << "compose: " << endl << t << endl;
    cout << "decompose: " << endl << decomp << endl;
    cout << "rotation: " << endl << rotation << endl;

    cout << "OTM: ";

    rotation.Transpose().findAngles(a,b,c);
    for(int i = 0; i < decomp.GetRow(); i++)
        cout << decomp.Get_Mij(i,i) << " ";

    cout << a * 180 / PI << " " << b * 180 / PI << " " << c * 180 / PI << endl;

    delete lInv;

    Matrix RDCCalc = (cleanCoord * tens);
    if(RDCCalc.GetRow() != cleanRDC.GetRow()){
        cout << "Error in back calculation of RDC during SolveBest()" << endl;
        exit(0);

    }//end if

    double s;
    for(int i = 0; i < RDCCalc.GetRow(); i++){
        s = abs(RDCCalc.Get_Mij(i,0) - cleanRDC.Get_Mij(i,0));
        /*<debug>*
        cout << "debug error is: abs("  << RDCCalc.Get_Mij(i,0)
             << " - " << cleanRDC.Get_Mij(i,0) << ") =  " << s << endl;
        *</debug>*/
        if(s > cleanError.Get_Mij(i,0)){
            cout << "Error: " << equationNum.Get_Mij(i,0) << " : " << s << " > "
                 << cleanError.Get_Mij(i,0) << endl;

        }//end if

    }//end for
    delete [] cleanMatArray;

}//end SolveBest()

void Redcat::Solve(int repeats){
    std::random_device rd;
    std::default_random_engine re(rd());
    std::uniform_real_distribution<double> randDoub(0.0, 1.0);
    Matrix *cleanMatArray = CleanMat();
    Matrix backCalcRDC, backCalcTensor, decomp(3,3), rotation(3,3), plot(3,3),
           equationNum(cleanMatArray[0]), cleanCoord(cleanMatArray[1]),
           cleanRDC(cleanMatArray[2]), cleanError(cleanMatArray[3]);
    Matrix *RDCError= new Matrix(cleanRDC.GetRow(),1);
    Matrix *lInv = cleanCoord.pseudoInverse();
    Matrix tensor = (*lInv * cleanRDC);
    Tensor t(tensor);
    vector<Tensor> tensors;
    double countViol[cleanRDC.GetRow()];
    double err[cleanRDC.GetRow()];
    double a, b, c, s;
    int count, p;
    count = p = 0;
    bool pass = true;


    /*<debug>*
    cout << "printing cleanRDC: "<< endl;
    for(int i = 0; i < cleanRDC.GetRow(); i++)
        cout << cleanRDC.Get_Mij(i,0) << endl;
    cout << endl;
    cout << "printing RDC: "<< endl;
    for(int i = 0; i < RDC->GetRow(); i++)
        cout << RDC->Get_Mij(i,0) << endl;
    *</debug>*/

    tensors.push_back(t);
    backCalcRDC = (cleanCoord * tensor);

    for(int i = 0; i < cleanRDC.GetRow(); i++){
        countViol[i] = 0;
        err[i] = abs(backCalcRDC.Get_Mij(i,0) - cleanRDC.Get_Mij(i,0));
        if (err[i] < SMALL_NUMBER)
            err[i] = 0;

    }//end for
    
    //for repeat
        //for count
            //A*S = R +-E
                //s*[i] = A^-L * R^~
            //solve s*[j]
            //if(abs(A * s*[i] - R) <= E)
                //continue
            //else
                //inc counter at [i]
    for(int i = 0; i < repeats; i++) {

        //set all errors for this run
        for (int j = 0; j < cleanRDC.GetRow(); j++)
            RDCError->Set_Mij(j, 0, (cleanRDC.Get_Mij(j, 0) + 2.0 * randDoub(re)
                              * cleanError.Get_Mij(j, 0))
                              - cleanError.Get_Mij(j, 0));

        //calc tensor for this run
        backCalcTensor= *lInv * *RDCError;

        //back calc rdc for this run
        backCalcRDC = (cleanCoord * backCalcTensor);

        //check back calc rdc's
        for(int j = 0; j < cleanRDC.GetRow(); j++){
                s = abs(backCalcRDC.Get_Mij(j,0) - cleanRDC.Get_Mij(j,0));
                if(s > cleanError.Get_Mij(j,0)){
                    countViol[j]++;
                    pass = false;
            
                }//end if

        }//end for
        if(pass){
            Tensor tensor(backCalcTensor);
            tensors.push_back(tensor);
            
        }//end if
        else
            count++;

        //reset pass for next run
        pass = true;

    }//end for i

    //print data
    //print rejected
    cout << "=REJECTIONS=" << endl;
    cout << "Total rejected: " << count << endl ;
    cout << endl << "Printing Rejections:     \t" << "Error Analysis:\t"
         << endl;

    for (int i = 1; i < (RDC->GetRow()+1); i++){
        if(p < equationNum.GetRow() && i==equationNum.Get_Mij(p,0)){
            cout << "Rejections by Equation " << equationNum.Get_Mij(p,0)
                 << ": " << countViol[p] << " \t" << err[p] << endl;
            p++;

        }//end if
        else {
            cout << "Equation " << i << " excluded ..." << endl;

        }//end else

    }//end for i
    cout << endl << "=TENSORS=" << endl;

    if (tensors.size() < 2){
        cout << "There are no monte carlo solutions within error" << endl
             << endl;
        cout << "Printing Tensors: " << endl;
        cout << "\t \txx" << "\t \tyy" << "\t \tzz" << "\t \talpha"
             << " \tbeta" << " \tgamma" << endl;
        tensors[0].getMatrix().Diag(decomp, rotation);
        cout << "Tensor 0" << ": \t";
        for (int i = 0; i < decomp.GetRow(); i++)
            cout << decomp.Get_Mij(i, i) << "\t";

        rotation.Transpose().findAngles(a, b, c);
        cout << a * 180 / PI << " \t" << b * 180 / PI << " \t"
             << c * 180 / PI << endl;
        plot = Matrix::getSphereCoords(rotation);
        cout << "\t \t";
        for (int j = 0; j < plot.GetRow(); j++) {
            for (int k = 0; k < plot.GetCol() - 1; k++) {
                cout << plot.Get_Mij(j, k) << " \t";

            }//end for k

        }//end for j
        cout << endl << "\t \t";

        plot = Matrix::getSphereCoords(-1 * rotation);
        for (int j = 0; j < plot.GetRow(); j++) {
            for (int k = 0; k < plot.GetCol() - 1; k++) {
                cout << plot.Get_Mij(j, k) << " \t";

            }//end for k

        }//end for j
        cout << endl;

    }//end if
    else {
        //print tensors
        cout << "Tensors within Error: "  << tensors.size()-1 << endl << endl;
        cout << "Printing Tensors: " << endl;
        cout << " \t \txx" << " \t \tyy" << " \t \tzz" << " \t \talpha"
             << " \tbeta" << " \tgamma" << endl;
        for (int i = 0; i < tensors.size(); i++) {
            tensors[i].getMatrix().Diag(decomp, rotation);
            cout << "Tensor " << i << ":\t ";
            for (int i = 0; i < decomp.GetRow(); i++)
                cout << decomp.Get_Mij(i, i) << "\t ";

            rotation.Transpose().findAngles(a, b, c);
            cout << a * 180 / PI << " \t " << b * 180 / PI << " \t "
                 << c * 180 / PI << endl;
            plot = Matrix::getSphereCoords(rotation);
            cout << "\t \t";
            for (int j = 0; j < plot.GetRow(); j++) {
                for (int k = 0; k < plot.GetCol() - 1; k++) {
                    cout << plot.Get_Mij(j, k) << " \t";
                    
                }//end for k

            }//end for j
            cout << endl << "\t \t";

            plot = Matrix::getSphereCoords(-1 * rotation);
            for (int j = 0; j < plot.GetRow(); j++) {
                for (int k = 0; k < plot.GetCol() - 1; k++) {
                    cout << plot.Get_Mij(j, k) << " \t";

                }//end for k

            }//end for j
            cout << endl;

        }//end for i

    }//end else
    delete RDCError;
    delete lInv;
    delete [] cleanMatArray;

}//end Solve()

void Redcat::CalculateRDC(double **rmsd, double **qfactor, double **sfactor, 
                          double Sxx, double Syy, double Szz, double a,
                          double b, double c, bool errFromFile,
                          double error, bool sub){
    if(fabs(Sxx + Syy + Szz) > SMALL_NUMBER){
        cout << "Order parameters do not sum to zero: " << endl;
        exit(EXIT_FAILURE);

    }//end if
    double q, s, RMSD, QFactor, SFactor, nAn;
    q = s = RMSD = QFactor = SFactor = 0;
    a *= PI / 180;
    b *= PI / 180;
    c *= PI / 180;
    Matrix SPrime(3,3,0);
    Matrix *temp(CleanMat());
    SPrime.Set_Mij(0,0,Sxx);
    SPrime.Set_Mij(1,1,Syy);
    SPrime.Set_Mij(2,2,Szz);
    std::random_device rd;
    std::default_random_engine re(rd());
    std::uniform_real_distribution<double> randDoub(0.0, 1.0);
    bool hasNines= false;
    nAn = 999;
    /*<debug>*cout << "printing tempCoord\n" << tempCoord << endl;*</debug>*/
    /*<debug>*cout << "printing tempRDC\n" << tempRDC << endl;*</debug>*/

    Matrix R(Matrix::findRotation(a,b,c));
    Matrix S = R.Transpose() * SPrime * R;

    //translate S to 5x1
    Matrix trans(5,1);

    //sxx syy sxy sxz syz
    trans.Set_Mij(0,0,S.Get_Mij(0,0));
    trans.Set_Mij(1,0,S.Get_Mij(1,1));
    trans.Set_Mij(2,0,S.Get_Mij(0,1));
    trans.Set_Mij(3,0,S.Get_Mij(0,2));
    trans.Set_Mij(4,0,S.Get_Mij(1,2));

    Matrix backRDC(*Coordinates * trans);
    if (errFromFile) {
        for (int i = 0; i < backRDC.GetRow(); i++) {
            for (int j = 0; j < Coordinates->GetCol(); j++) {
                if (Coordinates->Get_Mij(i, j) == 999) {
                    hasNines = true;
                    break;

                }//end if

            }//end for j
            if(!hasNines)
                backRDC.Set_Mij(i, 0, backRDC.Get_Mij(i, 0) + (Data[i][8] *
                                (2 * randDoub(re) - 1)));
            
            else
                backRDC.Set_Mij(i, 0, nAn);

            hasNines=false;

        }//end for

    }//end if
    else {
        for (int i = 0; i < backRDC.GetRow(); i++) {
            for (int j = 0; j < Coordinates->GetCol(); j++) {
                if (Coordinates->Get_Mij(i, j) == 999) {
                    hasNines = true;
                    break;

                }//end if

            }//end for j
            if (!hasNines)
                backRDC.Set_Mij(i, 0, backRDC.Get_Mij(i, 0) + (error *
                                (2 * randDoub(re) - 1)));

            else
                backRDC.Set_Mij(i, 0, nAn);

            hasNines=false;

        }//end for

    }//end else

    /*</debug>*
    cout << Sxx << " " << Syy << " " << Szz << " " << a << " " << b << " "
         << c << " " << endl;
    cout << "S" << endl;
    S.Print();
    cout << "R" << endl;
    R.Print();
    cout << "R.compare() = " << dif << endl;
    cout << "SPrime" << endl;
    SPrime.Print();
    cout << "backRDC" << endl;
    backRDC.Print();
    *</debug>*/
    
    for(int i = 0; i < RDC->GetRow(); i++){
        if(RDC->Get_Mij(i,0)!=999){
            RMSD += (RDC->Get_Mij(i,0) - backRDC.Get_Mij(i,0))
                    * (RDC->Get_Mij(i,0) - backRDC.Get_Mij(i,0));
            q += (RDC->Get_Mij(i,0) * RDC->Get_Mij(i,0));
            s += (backRDC.Get_Mij(i,0) * backRDC.Get_Mij(i,0));
            
        }//end if
        
    }//end for
    QFactor = sqrt(RMSD/q);
    SFactor = sqrt(RMSD/s);
    RMSD = sqrt(RMSD/(temp[1].GetRow()));
    *rmsd = &RMSD;
    *qfactor = &QFactor;
    *sfactor = &SFactor;

    if(sub){
        *RDC = backRDC;

    }//end if

    cout << "Back Calculated RDC: " << "\t" << endl;
    cout << backRDC  << "\t" << endl;
    delete [] temp;

}//void CalculateRDC(double **, double **, double **, double Sxx, double Syy,
//                   double, double, double, double, bool, double, bool)

void Redcat::Debug(){
    Matrix *cleanMatArray = CleanMat();
    Matrix equationNum = cleanMatArray[0],
           cleanCoord = cleanMatArray[1], cleanRDC = cleanMatArray[2],
           cleanError = cleanMatArray[3];
    cout << "File size: " << Count << endl;
    cout << endl;
    /*cout << "Redcate Data: " << endl;
    for(int i = 0; i < Count; i++){
        for(int j = 0; j < 9; j++){
            if(j==6&&Data[i][j]==-999){
                cout << "AVG ";

            }//end if
            else {
            cout << Data[i][j];
            if(j<8)
                cout << " ";
            
            }//end else

        }//end for j
        cout << " " << Comments[i]<< endl;

    }//end for i*/
    cout << endl;
    cout << "Analyzed Equations:" << endl << equationNum << endl;
    cout << "Coordinates: " << endl << *Coordinates << endl;
    cout << "Clean Coordinates: " << endl << cleanCoord << endl;
    cout << endl << "printing RDC: " << endl << *RDC << endl;
    cout << endl << "printing clean RDC: " << endl << cleanRDC << endl;

}//end Debug()

void Redcat::Save(ofstream& out){

    for(int i = 0; i < Count; i++){
        for(int j = 0; j < 6; j++){
            if(Data[i][j]==-999){
                out << "AVG ";

            }//end if
            else {
                out << Data[i][j] << " ";
                
            }//end else

        }//end for j

        out << RDC->Get_Mij(i,0) << " ";
        out << Data[i][7] << " ";
        out << Data[i][8];
        out << Comments[i] << endl;

    }//end for i

}//end Save(ofstream)

Redcat* Redcat::Parse(const char* pdb, int start, int end, const char* schema){
    std::ifstream schema_file;
    schema_file.open(schema);
    std::vector<std::string> a1s;
    std::vector<std::string> a2s;
    std::vector<int> gaps;
    std::vector<int> dmaxs;
    if (schema_file.is_open()) {
        
        std::string line;
        while (std::getline(schema_file, line))
        {
            // read schema file
            std::istringstream iss(line);
            std::string a1, a2;
            int gap;
            int dmax;
            if (!(iss >> a1 >> a2 >> gap >> dmax)) { 
              std::cerr << "Unable to parse schema" << std::endl;
              exit(EXIT_FAILURE);
              break; } // error
            a1s.push_back(a1);
            a2s.push_back(a2);
            gaps.push_back(gap);
            dmaxs.push_back(dmax);
        }
    } else {
      std::cerr << "Unable to open schema file: " << schema << std::endl;
    }
    schema_file.close();

    // now that we have the important information from the schema file
    // open the pdb and extract information as well
    std::ifstream pdb_file;
    pdb_file.open(pdb);
    // use dictionary to store atom data
    // (atom, residue number) -> "x y z"
    std::unordered_map<std::string, std::string> atom_coords;
    if (pdb_file.is_open()) {
      // RELEVANT PDB FORMAT
      // ATOM trash atom trash trash resNum x y z trash trash trash
      
      std::string line;
      while (std::getline(pdb_file, line)) {
        std::istringstream iss(line);
        std::string first_word;
        iss >> first_word;
        if (first_word != "ATOM") continue;
        std::string trash;
        std::string atom;
        int resNum;
        double x, y, z;
        if (!(iss >> trash >> atom >> trash >> trash >>
          resNum >> x >> y >> z)) { 
          std::cerr << "Unable to parse pdb" << std::endl;
          exit(EXIT_FAILURE);
          break; 
        }
        // add to dictionary only if atom is part of schema
        if (std::find(a1s.begin(), a1s.end(), atom) != a1s.end() || std::find(a2s.begin(), a2s.end(), atom) != a2s.end()) {
          std::string atom_res = atom + " " + to_string(resNum);
          std::ostringstream ss;
          ss << x << " " << y << " " << z;
          std::string coords = ss.str();
          atom_coords[atom_res] = coords;
        }
      }
      pdb_file.close();


      // write results to Redcat.in
      std::ofstream output_file;
      output_file.open("Redcat.in");
      if (output_file.is_open()) {
        for (int i = start; i <= end; i++) {
          for (int j = 0; j < a1s.size(); j++) {
            int next = i + gaps[j];
            std::string atom1 = "999 999 999";
            std::string atom_res = a1s[j] + " " + to_string(j);
            if (atom_coords.find(atom_res) != atom_coords.end()) {
              atom1 = atom_coords[atom_res];
            } 
            std::string atom2 = "999 999 999";
            std::string atom_res2 = a2s[j] + " " + to_string(next);
            if (atom_coords.find(atom_res2) != atom_coords.end()) {
              atom2 = atom_coords[atom_res2];
            } 
            
            output_file << atom1 << " " << atom2 << " 999 " << dmaxs[j]
              << " 999 /*" << a1s[j] << "-" << a2s[j] << " from " << i << "*/\n";

          }
        }

        output_file.close();
      }

      

      
    } else {
      std::cerr << "Unable to open pdb file: " << pdb << std::endl;
    }
     
    std::ifstream infile("Redcat.in");
    Redcat* r = new Redcat(infile);
    return r;

}//end Parse(const char*, int, int, const char*)

double Redcat::operator -(Matrix &PDP) {return 0.0;}

string* Redcat::SplitString(string& str, const char* delim){
    string* results;
    int cutAt;
    string temp;
    temp.assign(str);
    int i = 0;
    while ((cutAt = temp.find_first_of(delim)) != temp.npos){
        if (cutAt > 0){
            results[i]= temp.substr(0, cutAt);

        }//end if
        temp = temp.substr(cutAt + 1);
        i++;

    }//end while
    if (temp.length() > 0) {
        results[i] = temp;

    }//end if
    return results;

}//end StringSplit(string&, const char*, string*)

Matrix* Redcat::CleanMat(){
    Matrix *matArray = new Matrix[4];
    matArray[0]= Matrix(Coordinates->GetRow(),1);
    int p, q;
    p = q = 0;
    bool hasNines = false;
    for(int i = 0; i < Coordinates->GetRow(); i++){
        for(int j = 0; j < Coordinates->GetCol(); j++){
            if(Coordinates->Get_Mij(i,j)==999){
                hasNines = true;
                break;

            }//end if

        }//end for j
        if(!(hasNines) && RDC->Get_Mij(i,0)!=999){
            matArray[0].Set_Mij(p,0,(i+1));
            p++;

        }//end if
        hasNines = false;

    }//end for i

    Matrix *temp = matArray[0].submatrix(p);
    matArray[0] = *temp;
    delete temp;
    matArray[1]= Matrix(p,5);
    matArray[2]= Matrix(p,1);
    matArray[3]= Matrix(p,1);

    for(int i = 0; i < p; i++){
        q=(matArray[0].Get_Mij(i,0) - 1);
        for(int j = 0; j < Coordinates->GetCol(); j++)
            matArray[1].Set_Mij(i, j, Coordinates->Get_Mij(q, j));

        matArray[2].Set_Mij(i,0, RDC->Get_Mij(q,0));
        matArray[3].Set_Mij(i,0,Data[q][8]);

    }//end for i
    /*<debug>*
    cout << "print index: " << endl;
    for(int i = 0; i < p; i++){
        cout << matArray[0].Get_Mij(i,0) << endl;

    }//end for
    cout << "print cleanCoord: " << endl;
    for(int i = 0; i < p; i++){
        for(int j=0; j< 5; j++)
            cout << matArray[1].Get_Mij(i,j);

        cout << endl;

    }//end for
    cout << "print cleanRDC: " << endl;
    for(int i = 0; i < p; i++){
        cout << matArray[2].Get_Mij(i,0) << endl;

    }//end for
    cout << "print cleanError: " << endl;
    for(int i = 0; i < p; i++){
        cout << matArray[3].Get_Mij(i,0) << endl;

    }//end for
    *</debug>*/
    return matArray;

}//end Matrix*

void Redcat::Test(){
    /*
                    double a, b, c, alpha, beta, gamma = 0;
                    //a = 15.00 * (pi/180);
                    //b = 30.00 * (pi/180);
                    //c = -10.00 * (pi/180);
                    a = 1.93993; b = 1.39877; c = -0.00942129;

                    cout << "a: " << a << " b: " << b << " c: " << c << endl;

                    Matrix rotation;

                    //find rotation abc
                    rotation = Matrix::findRotation(a, b, c);
                    cout << "Rotation1: " << endl;
                    rotation.Print();

                    cout << "compare: " << rotation.compare(a, b, c) << endl;

                    //find angles alpha beta gamma
                    rotation.findAngles(alpha, beta, gamma);
                    cout << endl << "alpha: " << alpha << " beta: " << beta << " gamma: "
                         << gamma << endl;

                    //find rotation alpha beta gamma
                   rotation = Matrix::findRotation(alpha, beta, gamma);
                    cout << "Rotation2: " << endl;
                    rotation.Print();

                    cout << "compare: " << rotation.compare(alpha, beta, gamma) << endl;

                    //find angles abc
                    rotation.findAngles(a, b, c);
                    cout << endl << "a: " << a << " b: " << b << " c: " << c << endl;

                   //find rotation abc
                    rotation = Matrix::findRotation(a, b, c);
                    cout << "Rotation3: " << endl;
                    rotation.Print();

                    cout << "compare: " << rotation.compare(a, b, c) << endl;

                    //find angles alpha beta gamma
                    rotation.findAngles(alpha, beta, gamma);
                    cout << endl << "alpha: " << alpha << " beta: " << beta << " gamma: "
                         << gamma << endl;

                    //find rotation alpha beta gamma
                   rotation = Matrix::findRotation(alpha, beta, gamma);
                    cout << "Rotation4: " << endl;
                    rotation.Print();

                    cout << "compare: " << rotation.compare(alpha, beta, gamma) << endl;

                    //find angles abc
                    rotation.findAngles(a, b, c);
                    cout << endl << "a: " << a << " b: " << b << " c: " << c << endl;

                    //3.915909e-04 7.911877e-04 -1.182779e-03 -3.103784 170.129064 107.999336

                    double a = -3.103784 * pi / 180;
                    double b = 170.129064 * pi / 180;
                    double c = 107.999336 * pi / 180;

                    Matrix rotation = Matrix::findRotation(a,b,c);

                    cout << "rotation: " << endl << rotation << endl;

                    Matrix S(3,3);

                    S.Set_Mij(0,0,0.0003915909);
                    S.Set_Mij(1,1,0.0007911877);
                    S.Set_Mij(2,2,-0.001182779);

                    Matrix comp = rotation * S * rotation.Transpose();

                    cout << "comp: " << endl << comp << endl;
    

                    double a, b, c;
                    Matrix tensor (3,3);
                    Matrix rotation, decomp;

                    a=b=c=0;

                    tensor.M[0][0]=0.0006828506302554160;
                    tensor.M[0][1]=-0.0001294214016525075;
                    tensor.M[0][3]=0.0003253727627452463;
                    tensor.M[1][1]=0.0004430381522979587;
                    tensor.M[1][2]=-0.00003780479528359137;
                    tensor.M[2][2]=-0.001125888782553375;

                    tensor.M[1][0]=tensor.M[0][1];
                    tensor.M[2][0]=tensor.M[0][2];
                    tensor.M[2][1]=tensor.M[1][2];

                    tensor.Diag(decomp, rotation);

                    rotation.findAngles(a,b,c);

                    cout << "Decomp: " << endl << decomp << endl;

                    cout << "Rotation: " << endl << rotation << endl;

                    cout << "a: " << a * 180 / pi << " b: " << b * 180 / pi << " c: "
                         << c * 180 / pi << endl;
         

                    //0.355482 0.933181 0.052973
                    //0.918875 -0.359287 0.163040
                    //-0.171178 0.009282 0.985196

                    double a, b, c;
                    a=b=c=0;
                    Matrix r(3,3);
                    r.M[0][1]= 0.355482;
                    r.M[0][1]= 0.933181;
                    r.M[0][2] = 0.052973;
                    r.M[1][0] = 0.918875;
                    r.M[1][1] = -0.359287;
                    r.M[1][2] = 0.163040;
                    r.M[2][0] = -0.171178;
                    r.M[2][1] = 0.009282;
                    r.M[2][2] = 0.985196;

                    cout << "rotation 1: " << endl << r << endl;
                    r.Transpose().findAngles(a,b,c);
                        cout << "a: " << a * 180 / pi << " b: " << b * 180 / pi << " c: "
                         << c * 180 / pi << endl;


                    //-3.103784 170.129064 107.999336
                    int alpha, beta, gamma;
                    alpha = -3.103784 * pi / 180;
                    beta = 170.129064 * pi / 180;
                    gamma = 107.999336 * pi / 180;

                    Matrix R = Matrix::findRotation(alpha, beta, gamma);

                    cout << "rotation 2: " << endl << r << endl;
                    cout << "alpha: " << a * 180 / pi << " beta: " << b * 180 / pi << " gamma: "
                         << c * 180 / pi << endl;
    

                    Matrix a(3,3,0);
                    Matrix m(3,3,0);
                    Matrix n(3,3,0);
                    Tensor z(a);
                    z.Decompose(m,n);

                    cout << "z" << endl;
                    z.Print();
                    cout << "m" << endl;
                    m.Print();
                    cout << "n" << endl;
                    n.Print();

                    Matrix xyPlane(1,3);
                    xyPlane.Set_Mij(0,0,10);
                    xyPlane.Set_Mij(0,1,10);
    
                    Matrix vector(1,3);
                    vector.Set_Mij(0,0,1);
                    vector.Set_Mij(0,1,2);
                    vector.Set_Mij(0,2,3);

                    cout << "(xAxis:vector)" << endl;
                    cout << "scalProj: " << xyPlane.scalProj(vector.Transpose()) << endl;

                    Matrix vecProjection = xyPlane.vecProj(vector.Transpose());
                    cout << "vecProj: " << endl << vecProjection << endl;
                    cout << "vecProj mag: " << vecProjection.magnitude() << endl;
    
                    Matrix R(3,3);
                    for(int i = 0; i < 3 ; i++)
                        R.Set_Mij(i,i,1.0);

                    Matrix ans(Matrix::getSphereCoords(R));
                    cout << ans << endl;
                    ans = Matrix::getSphereCoords(-1 * R);
                    cout << ans << endl;
    

    
                    Matrix R(3,3);
                    double a,b,c;
                    R.Set_Mij(0,0,-0.411449);
                    R.Set_Mij(0,1,-0.868368);
                    R.Set_Mij(0,2,0.276850);

                    R.Set_Mij(1,0,0.101855);
                    R.Set_Mij(1,1,0.258042);
                    R.Set_Mij(1,2,0.960750);

                    R.Set_Mij(2,0,0.905724);
                    R.Set_Mij(2,1,-0.423498);
                    R.Set_Mij(2,2,0.017723);

                    R.findAngles(a,b,c);

                    cout << "a: " << a * 180 << " " << b * 180 << " " << c * 180 << endl;
    
                    Matrix R1(3,3);
                    double alpha,beta,gamma;
                    R1.Set_Mij(0,0,-0.245141);
                    R1.Set_Mij(0,1,0.754550);
                    R1.Set_Mij(0,2,-0.608737);

                    R1.Set_Mij(1,0,0.940458);
                    R1.Set_Mij(1,1,0.032578);
                    R1.Set_Mij(1,2,-0.338346);

                    R1.Set_Mij(2,0,0.235467);
                    R1.Set_Mij(2,1,0.655434);
                    R1.Set_Mij(2,2,0.717608);

                    R1.findAngles(alpha,beta,gamma);

                    cout << "alpha: " << alpha * 180 << " " << beta * 180 << " " << gamma * 180
                         << endl;

                    Matrix Coord(6,5), testCoord(9,5), Rdc(6,1), testRdc(9,1);

                    srand48(rand() * time(NULL)/ 10000);

                    //set up Coord
                    for(int i = 0; i<Coord.GetRow(); i++){
                        for(int j = 0; j<Coord.GetCol(); j++)
                            Coord.Set_Mij(i, j, drand48());
        
                        Rdc.Set_Mij(i,0, drand48());

                    }//end for i

                    //set up testCoord with row 3 0 else same as Coord
                    int c1 = 0;
                    for(int i = 0; i<testCoord.GetRow(); i++){
                        if(i==2 || i==4 || i==5){
                            for (int j = 0; j < testCoord.GetCol(); j++)
                                testCoord.Set_Mij(i, j,0);

                            testRdc.Set_Mij(i, 0, 0);

                        }//end
                        else {
                            for (int j = 0; j < testCoord.GetCol(); j++)
                                testCoord.Set_Mij(i, j, Coord.Get_Mij(c1, j));

                            testRdc.Set_Mij(i, 0, Rdc.Get_Mij(c1,0));

                            c1++;

                        }//end else

                    }//end for i

                    //test traditional
                    cout << "Traditional:===============" << endl;
                    Matrix decomp, rotation;
                    double a,b,c;
                    Matrix *lInv = Coord.pseudoInverse();
         *<debug>*cout << "printing Coord\n" << Coord << endl;*</debug>*
         *<debug>*cout << "printing RDC\n" << Rdc << endl;*</debug>*
                    Matrix tens = (*lInv * Rdc);
                    Tensor t(tens);

         *<debug>*cout << "printing Coord\n" << Coord << endl;*</debug>*

                    //decompose
                    t.getMatrix().Diag(decomp, rotation);
                    //tens.Diag(decomp, rotation);
         *<debug>*
                    cout << "compose: " << endl << t << endl;
                    cout << "decomp: " << endl << decomp << endl;
                    cout << "rotation: " << endl << rotation << endl;

                    cout << "OTM: ";

                    rotation.Transpose().findAngles(a,b,c);
                    for(int i = 0; i < decomp.GetRow(); i++)
                        cout << decomp.Get_Mij(i,i) << " ";

                    cout << a * 180 / pi << " " << b * 180 / pi << " " << c * 180 / pi << endl;

                    //test new
                    cout << "New:===============" << endl;
                    lInv = testCoord.pseudoInverse();
         *<debug>*cout << "printing testCoord\n" << testCoord << endl;*</debug>*
         *<debug>*cout << "printing testRDC\n" << testRdc << endl;*</debug>*
                    tens = (*lInv * testRdc);
                    Tensor u(tens);

         *<debug>*cout << "printing testCoord\n" << testCoord << endl;*</debug>*

                    //decompose
                    u.getMatrix().Diag(decomp, rotation);
                    //tens.Diag(decomp, rotation);
         *<debug>*
                    cout << "compose: " << endl << u << endl;
                    cout << "decomp: " << endl << decomp << endl;
                    cout << "rotation: " << endl << rotation << endl;

                    cout << "OTM: ";

                    rotation.Transpose().findAngles(a,b,c);
                    for(int i = 0; i < decomp.GetRow(); i++)
                        cout << decomp.Get_Mij(i,i) << " ";

                    cout << a * 180 / pi << " " << b * 180 / pi << " " << c * 180 / pi << endl;

                    delete lInv;

                    Matrix R(3, 3), decomp(3, 3), rotate(3, 3);
                    double a, b, c;

                    //M2
                    R.Set_Mij(0, 0, -9.3952e-04);
                    R.Set_Mij(0, 1, 3.2210e-04);
                    R.Set_Mij(0, 2, 2.6417e-04);
                    R.Set_Mij(1, 0, 3.2210e-04);
                    R.Set_Mij(1, 1, 1.2518e-03);
                    R.Set_Mij(1, 2, -4.4699e-04);
                    R.Set_Mij(2, 0, 2.6417e-04);
                    R.Set_Mij(2, 1, -4.4699e-04);
                    R.Set_Mij(2, 2, -3.1228e-04);


                    R.Diag(decomp, rotate);
                    rotate.findAngles(a, b, c);

                    cout << "Decomp: " << endl << decomp << endl;
                    cout << "A: " << a * 180 / PI << " B: " << b * 180 / PI << " C: "
                            << c * 180 / PI << endl;
                    cout << endl << endl;

                    //M3
                    R.Set_Mij(0, 0, -1.7482e-04);
                    R.Set_Mij(0, 1, 3.3415e-04);
                    R.Set_Mij(0, 2, 2.9418e-05);
                    R.Set_Mij(1, 0, 3.3415e-04);
                    R.Set_Mij(1, 1, -1.7399e-04);
                    R.Set_Mij(1, 2, -5.0241e-04);
                    R.Set_Mij(2, 0, 2.9418e-05);
                    R.Set_Mij(2, 1, -5.0241e-04);
                    R.Set_Mij(2, 2, 3.4882e-04);

                    R.Diag(decomp, rotate);
                    rotate.findAngles(a, b, c);

                    cout << "Decomp: " << endl << decomp << endl;
                    cout << "A: " << a * 180 / PI << " B: " << b * 180 / PI << " C: "
                            << c * 180 / PI << endl;

                    R.Set_Mij(0, 0, 0.3080);
                    R.Set_Mij(0, 1, 0.0159);
                    R.Set_Mij(0, 2, 0.9513);
                    R.Set_Mij(1, 0, -0.8140);
                    R.Set_Mij(1, 1, 0.5219);
                    R.Set_Mij(1, 2, 0.2549);
                    R.Set_Mij(2, 0, -0.4924);
                    R.Set_Mij(2, 1, -0.8529);
                    R.Set_Mij(2, 2, 0.1736);

                    R.findAngles(a, b, c);

                    cout << "A: " << a * 180 / PI << " B: " << b * 180 / PI << " C: "
                            << c * 180 / PI << endl;
         *
                Matrix R(3, 3), decomp(3, 3), rotate(3, 3);
                double a, b, c;
        
                R.Set_Mij(0, 0, -0.245141);
                R.Set_Mij(0, 1, 0.754550);
                R.Set_Mij(0, 2, -0.608737);

                R.Set_Mij(1, 0, 0.940458);
                R.Set_Mij(1, 1, 0.032578);
                R.Set_Mij(1, 2, -0.338346);

                R.Set_Mij(2, 0, 0.235467);
                R.Set_Mij(2, 1, 0.655434);
                R.Set_Mij(2, 2, 0.717608);

                R.findAngles(a, b, c);

                cout << "A: " << a * 180 / PI << " B: " << b * 180 / PI << " C: "
                        << c * 180 / PI << endl;
         */

        Matrix A(3, 3), R(3, 3), decomp(3, 3), rotate(3, 3);
        double a, b, c;

        A.Set_Mij(0, 0, 1.1133520e-04);
        A.Set_Mij(0, 1, -3.8270639e-04);
        A.Set_Mij(0, 2,  7.1215577e-04);
        A.Set_Mij(1, 0, -3.8270639e-04);
        A.Set_Mij(1, 1, -4.6240327e-04);
        A.Set_Mij(1, 2, -4.4198890e-04);
        A.Set_Mij(2, 0,  7.1215577e-04);
        A.Set_Mij(2, 1, -4.4198890e-04);
        A.Set_Mij(2, 2,  3.5106807e-04);

        A.Diag(decomp, R);

        R.findAngles(a, b, c);

        cout << "R: \n" << R << endl;
        cout << "decomp: \n" << decomp << endl;
        cout << "A: " << a * 180 / PI << " B: " << b * 180 / PI << " C: "
                << c * 180 / PI << endl;



        cout << "Back Calc Rotation: \n" << Matrix::findRotation(a, b, c) << endl;

    }//end Test()

}//end namespace redcat
