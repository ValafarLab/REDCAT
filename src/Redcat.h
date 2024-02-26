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
#ifndef STDRedcat_H
#define STDRedcat_H
#define denom 2.5066
#define Pi 3.141592

#include <iostream>
#include <cmath>
#include <sstream>
#include "matrix.h"
#include "tensor.h"

using namespace std;

namespace redcat {
/**
@author Homayoun Valafar
*/
class Redcat {
    friend ostream &operator<< (ostream &, const Redcat &);
    friend istream &operator>> (istream &, Redcat &);
public:
    Redcat(int);
    Redcat(istream&);
    Redcat(const Redcat &);
    ~Redcat();
    Redcat & operator= (const Redcat &);

    void Prune(Redcat &);
    void Shift(int);
    bool IsValid(int);
    void CalcStat();
    void PrintStat();
    void PrintVect();
    /*
    Matrix & CalcPDP(double, double, double, int);
    Matrix & CalcPDP(Redcat &, double, double, double, int, double, double, double, int);
    double CalcDiffPDP(Matrix &, double, double, double, int);
    double CalcDiffPDP(Redcat &, Matrix &, double, double, double, int, double, double, double, int);
    */
    void Rotate(double, double, double, double, double, double, bool = false);
    void Rotate(double, double, double);
    static void Rotation(double, double, double);
    static void Rotation(double, double, double, double, double, double,
                         double ,double ,double);
    /*
    void Plot(double, double, double);
    void Plot(Matrix R);
    */
    void Solve(int);
    void SolveBest();
    void Debug();
    void CalculateRDC(double **, double **, double **, double Sxx, double Syy,
                      double, double, double, double, bool, double, bool);
    void Save(ofstream&);
    static Redcat* Parse(const char*, int, int, const char*);
    static string* SplitString(string&, const char*);
    Matrix* CleanMat();
    /*
    Matrix CleanCoord();
    Matrix CleanRDC();
    */
    void Test();
    double operator-(Matrix &);

private:
    int Count, MatchCount;
    double Mean, Std, Min, Max;
    double **Data;
    string *Comments;
    Matrix* Coordinates;
    Matrix* RDC;
    Matrix* tensor;

};

}//end namespace redcat

#endif
