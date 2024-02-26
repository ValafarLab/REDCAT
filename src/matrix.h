/***************************************************************************
 *   Copyright (C) 2005 by Dr.Homayoun Valafar                             *
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
 
#ifndef MATRIX_H
#define MATRIX_H
#include <iostream>
#include <math.h>

#define SMALL_NUMBER 1e-6
#define SIGN(a,b) ((b) > 0.0 ? fabs(a) : -fabs(a))
#define SQR(a) (((a)) == 0.0 ? 0.0 : a*a)
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define PYTH(a,b) sqrt(a*a+b*b)
#define ROTATE(a,i,j,k,l) g=a[i][j];h=a[k][l];a[i][j]=g-s*(h+g*tau);\
			    a[k][l] = h + s * (g - h * tau);
#define PI 3.14159

using namespace std;

namespace redcat {
/**
  This class provides the basic matrix data type and operations.

  @author Homayoun Valafar
 * @edited Chris Schmidt
 */
class Matrix {
	
    public:
	//constructors
	Matrix(int = 3, int = 3, double = 0.0);
	Matrix(const Matrix &);
	~Matrix();

        //getters
	double Get_Mij(int i, int j) const {
		return M[i][j];
	}
	int GetRow() const {
		return row;
	}
	int GetCol() const {
		return col;
	}
        Matrix getRow(int);
        Matrix getCol(int);

        //setters
        void Set_Mij(int i, int j, double x) {
		M[i][j] = x;
	}
        void setRow(int, Matrix);
        void setCol(int, Matrix);
        void setM(size_t, size_t, double**);

        //operator overload
        friend ostream &operator<< (ostream &, const Matrix &);
	friend istream &operator>> (istream &, Matrix &);

	const Matrix & operator= (const Matrix &);

        //Matrix by Matrix
	Matrix operator+ (const Matrix &);
        Matrix operator- (const Matrix &);
	Matrix operator* (const Matrix &);

        //Matrix and double
	Matrix operator+ (double);
        Matrix operator- (double);
	Matrix operator* (double);
        Matrix operator/ (double);

        //double and Matrix
        friend Matrix operator+ (double, const Matrix &);
        friend Matrix operator- (double, const Matrix &);
	friend Matrix operator* (double, const Matrix &);
        friend Matrix operator/ (double, const Matrix &);

	bool operator==  (const Matrix &);
	bool operator!=  (const Matrix &);

        //Functions
        Matrix Transpose();
	Matrix * pseudoInverse();
	void SVD(Matrix &, Matrix &, Matrix &);
	void Diag(Matrix &, Matrix &);
        
	double Det();
	bool isSquare();
	bool isSym();
	double Trace();

	void makeIdentity();
	bool isIdentity();
        void swapCol(size_t, size_t);
        void swapRow(size_t, size_t);
        Matrix * submatrix(size_t);
	void AppendRows(Matrix &N);
	void Copy(Matrix &N);
        Matrix * canon();
        double compare(double, double, double);
        double magnitude();
        double scalProj(Matrix);
        Matrix vecProj(Matrix);
        Matrix getRx(double);
        Matrix getRy(double);
        Matrix getRz(double);
        int findAngles(double &, double &, double &);
        int doFindAngles(double &, double &, double &);
        static Matrix findRotation(double, double, double);
        static Matrix getSphereCoords(Matrix);
	void Print();
        static void printVersion(){ cout << "Matrix.....v1.0\n"; }

        //member variables
	double **M;
    protected:
	int row, col;
	
};

}//end namespace redcat
#endif
