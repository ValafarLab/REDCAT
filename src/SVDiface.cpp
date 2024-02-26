#include "SVDiface.h"
#include <iostream>
#include <cstdlib>

using namespace std;
using redcat::Matrix;

void SVDiface::singValDecomp(redcat::Matrix A, redcat::Matrix *U, double *S, redcat::Matrix *V){
    Eigen::MatrixXd eigenA= RedMatToEigenMat(A);
    /*<debug>*cout << "Matrix A: " << endl << eigenA << endl;*</debug>*/
    Eigen::VectorXd sVec(5);
    Eigen::JacobiSVD<Eigen::MatrixXd> svd(eigenA, Eigen::ComputeThinU | Eigen::ComputeThinV);
    sVec = svd.singularValues();
    for(int i = 0; i < sVec.rows(); i++){
       S[i] = sVec(i);

    }//end for
    *U = EigenMatToRedMat(svd.matrixU());
    *V = EigenMatToRedMat(svd.matrixV());
    /*<debug>*
    cout << "Matrix U: " << endl << svd.matrixU() << "\nMatrix V: " << endl
         << svd.matrixV() << "\nMatrix S: " << svd.singularValues() << endl;
    *</debug>*/

}//end singValDecomp(redcat::Matrix *, redcat::Matrix **, double **, redcat::Matrix **)

Eigen::MatrixXd SVDiface::RedMatToEigenMat(Matrix inMat){
    Eigen::MatrixXd answer(inMat.GetRow(), inMat.GetCol());
    for(int i = 0; i < inMat.GetRow(); i++)
        for (int j = 0; j < inMat.GetCol(); j++)
            answer(i,j) = inMat.Get_Mij(i,j);
    
    return answer;

}//end MatrixXd RedMatToEigenMat(Matrix);

Matrix SVDiface::EigenMatToRedMat(Eigen::MatrixXd inMat){
    Matrix answer(inMat.rows(), inMat.cols());
    for(int i = 0; i < inMat.rows(); i++)
        for (int j = 0; j < inMat.cols(); j++)
            answer.Set_Mij(i, j, inMat(i,j));

    return answer;

}//end static Matrix EigenMatToRedMat(Eigen::MatrixXd);