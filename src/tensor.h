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
 ***************************************************************************/
#ifndef STDTENSOR_H
#define STDTENSOR_H
#include <math.h>
#include "matrix.h"

using namespace std;

namespace redcat {
/**
@author Homayoun Valafar
*/
class Tensor
{
    friend ostream &operator<< (ostream &, const Tensor &);
    friend istream &operator>> (istream &, Tensor &);
public:
  Tensor(double = 3e-4, double = 0, double = 0, double = 5e-4, double =0);
  Tensor(const Tensor &);
  Tensor(const Matrix &);
//  Tensor(const Matrix &) //check 3 by 3 and then take the 5 elements
  ~Tensor();

  Matrix getMatrix();
  void CorrectAngles(double &, double &, double &);
  void SetElements(double, double, double, double, double);
  void ConvR2EA();
  void Decompose(Matrix & S, Matrix & R);
  void Point2Hull();
  void Rotate(double, double, double);
  void Print();

  double getXX()const{return S.M[0][0];};
  double getXY()const{return S.M[0][1];};
  double getXZ()const{return S.M[0][2];};
  double getYY()const{return S.M[1][1];};
  double getYZ()const{return S.M[1][2];};
  double getZZ()const{return S.M[2][2];};

  double otmDistance(const Tensor &);

//trace = 0, & diagonal.  

  Tensor & operator=(Tensor &);
//  void Switch(int, int);
//  void CorrectRH();
//  void Order();

private:
  Matrix S;
};

}//end namespace redcat
#endif
