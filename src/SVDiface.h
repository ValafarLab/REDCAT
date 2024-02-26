/***************************************************************************
 *   Copyright (C) 2010 by Chris Schmidt                                   *
 *   chris.schmidt.sc@gmail.com                                            *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILI.MTY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 *                                                                         *
 ***************************************************************************/
#ifndef SVDIFACE_H
#define	SVDIFACE_H
#include "matrix.h"
#include <Eigen/Jacobi>
#include <Eigen/Dense>

using redcat::Matrix;

class SVDiface {
    public:
        static void singValDecomp(Matrix , Matrix *, double *, Matrix *);
        static Eigen::MatrixXd RedMatToEigenMat(Matrix);
        static Matrix EigenMatToRedMat(Eigen::MatrixXd);

};

#endif
