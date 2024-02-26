/* 
 * File:   main.cpp
 * Author: Homayoun Valafar
 *
 * Created on March 20, 2010, 7:30 PM
 * Edited by Chris Schmidt on June 2010
 */

#include "Redcat.h"
#include <cstdlib>
#include <iostream>
#include <cstring>
#include <fstream>

using namespace std;
using namespace redcat;

void usage(string e = "") {
	if (e.compare("")) cerr << "Error: " << e << endl << endl;
	cerr << "Usage: " << "Redcat" << " [options] \"TBD\"" << endl;
	cerr << "\t-b\t: generate the best (lease squares) solution" << endl;
        cerr << "\t-c <Sxx> <Syy> <Szz> <alpha> <beta> <gamma> "
             << "<error from file> [error range] [y/n]" << endl;
        cerr << "\t\t: back calculates RDC's from Diagnal matrix and alpha, "
             << "beta and gamma" << endl;
        cerr << "\t\t if the user would like to calculate RDC data within the"
             << " error range" << endl;
        cerr << "\t\t provided by the redcat file, a y should be placed"
             << " in the" << endl;
        cerr << "\t\t <error from file> location and the error seed should"
             << " be omited," << endl;
        cerr <<"\t\t otherwise provide an \'n\' in the "
             << "<error from file> location and" << endl;
        cerr <<"\t\t provide the range in the [error seed] location" << endl;
        cerr << "\t\t y for substitution of backcalculated RDC values into the"
             << " redcat file" << endl;
        cerr << "\t\t n for no subsitution" << endl;
        cerr << "\t-d\t: display debug information" << endl;
        //TODO: add -g here
        cerr << "\t-h\t: display help message" << endl;
        cerr << "\t-i\t: read input (redcat) file into the program" << endl;
        cerr << "\t-o\t: write output file" << endl;
        cerr << "\t-p <pdb-file> <start-residue> <end-residue> <schema-file>"
             << endl;
        cerr << "\t\t: parse pdb-file to create a redcat file" << endl;
        cerr << "\t-q\t: quiets generation of labels for each option "
             << "presented after -q option" << endl;
        cerr << "\t-r [<alpha> <beta> <gamma>] [<x1> <y1> <z1> <x2> <y2> "
                "<z2> <x3> <y3> <z3>]" << endl;
        cerr << "\t\t: returns the rotation matrix from given euler angles or "
                "vice versa" << endl;
        cerr << "\t-s <iterations>" << endl;
        cerr << "\t\t: generate all possible solutions where " << endl;
        cerr << "\t\t iterations is the number of Monte Carlo samples taken"
             << endl;
        //TODO: add u here
        cerr << "\t-v\t: prints version information" << endl;
//a e f j k l m n w x y z
	exit(EXIT_FAILURE);

}//end usage(string)

int main(int argc, char** argv) {
    bool rCatIsSet, quietFlag, errFromFile;
    string endChar = "~";
    rCatIsSet = quietFlag = false;
    int repeats, resolution;
    double sxx, syy, szz, a, b, c, x2, y2 ,z2 ,x3 ,y3 ,z3 , error, *rmsd,
           *qfactor, *sfactor, rdc, dMax, atomDist;
    int startRes, endRes;
    string version = "Redcat v2-1-11";
    string dataPath, pdb;
    Redcat* redcat;
    ifstream* infile= new ifstream();
    ofstream* outfile= new ofstream();
    stringstream* stdInString= new stringstream();

    if (argc <= 1) usage();
	while (argc > 1 && argv[1][0] == '-') {
		switch (argv[1][1]) {
                    case 'b' :  //best solution
                        if(!quietFlag)
                            cout << "best solution" << endl << endl;

                        redcat->SolveBest();
                        break;
                    
                    case 'c' : //calculate RMSD and RDC
                        if(!quietFlag)
                            cout << "calculating RDC and Q-Factor" << endl
                                 << endl;

                        if(argv[1][2] == NULL){
                            sxx = atof(&argv[2][0]);
                            ++argv;--argc;

                        }//end if
                        else
                            sxx = atof(&argv[1][2]);

                        syy = atof(&argv[2][0]);
                        ++argv;--argc;
                        szz = atof(&argv[2][0]);
                        ++argv;--argc;
                        a = atof(&argv[2][0]);
                        ++argv;--argc;
                        b = atof(&argv[2][0]);
                        ++argv;--argc;
                        c = atof(&argv[2][0]);
                        ++argv;--argc;
                        if (argv[2][0] == 'n'){
                            errFromFile = false;
                            ++argv;--argc;
                            error = atof(&argv[2][0]);

                        }//end if
                        else{
                            errFromFile = true;
                            error = 0;

                        }//end else
                        ++argv;--argc;
                        if (argc > 2 && argv[2][0] != '-') {
                            redcat->CalculateRDC(&rmsd, &qfactor, &sfactor,
                                                 sxx, syy, szz, a, b, c,
                                                 errFromFile, error,
                                                 (argv[2][0] == 'y'));
                            ++argv;--argc;

                        }//end if
                        else
                            redcat->CalculateRDC(&rmsd, &qfactor, &sfactor, 
                                                 sxx, syy, szz, a, b, c,
                                                 errFromFile, error, false);
                        
                        cout << "RMSD: " << *rmsd << "\t" << endl
                             << "Q-Factor: " << *qfactor << "\t"  << endl
                             << "S-Factor: " << *sfactor << "\t" << endl;
                        break;

                    case 'd' : //debug
                        if(!quietFlag)
                            cout << "debug" << endl << endl;

                        redcat->Debug();
                        break;
                        
                    case 'i':  //inputfile
                        if(!quietFlag)
                            cout << "reading input file" << endl << endl;

                        if(argv[1][2] == NULL){
                            dataPath = &argv[2][0];
                            ++argv;--argc;

                        }//end if
                        else
                            dataPath = &argv[1][2];

                        delete infile;
                        infile = new ifstream(dataPath.c_str());
                        redcat = new Redcat(*infile);
                        rCatIsSet = true;
                        break;

                    case 'h': //help
                        usage();
                        break;

                    case 'f': //std input
                        cout << "reading standard input" << endl << endl;
                        cout << "not yet working" << endl;
                        exit(EXIT_FAILURE);
                        do{
                            cin >> dataPath;
                            dataPath = &argv[2][0];
                            *stdInString << dataPath << " ";
                            ++argv;--argc;

                        } while(dataPath != endChar);
                        redcat = new Redcat(*stdInString);
                        rCatIsSet = true;
                        break;
                        
                    case 'n':  //nullspace
                        if(!quietFlag)
                            cout << "nullspace" << endl << endl;
                        
                        break;

                    case 'o':  //outputfile
                        if(!quietFlag)
                            cout << "write output file" << endl << endl;

                        if(argv[1][2] == NULL){
                            dataPath = &argv[2][0];
                            ++argv;--argc;

                        }//end if
                        else
                            dataPath = &argv[1][2];
                        
                        delete outfile;
                        outfile = new ofstream(dataPath.c_str());
                        redcat->Save(*outfile);
                        break;

                    case 'p': //parse
                        if(!quietFlag)
                            cout << "parse pdb" << endl << endl;

                        if(argv[1][2] == NULL){
                            pdb = &argv[2][0];
                            ++argv;--argc;
                            

                        }//end if
                        else
                            pdb = &argv[1][2];
                        startRes = atoi(&argv[2][0]);
                        ++argv;--argc;
                        endRes = atoi(&argv[2][0]);
                        ++argv;--argc;
                        dataPath = &argv[2][0];
                        ++argv;--argc;
                        redcat = Redcat::Parse(pdb.c_str(), startRes, endRes,
                                               dataPath.c_str());
                        rCatIsSet= true;
                        break;

                    case 'q': //quiet
                        quietFlag = true;
                        break;

                    case 'r': //rotation
                        if(!quietFlag)
                            cout << "rotation matrix" << endl << endl;

                        if(argv[1][2] == NULL){
                            a = atof(&argv[2][0]);
                            ++argv;--argc;

                        }//end if
                        else{
                            a = atof(&argv[1][2]);

                        }//end else
                        b = atof(&argv[2][0]);
                        ++argv;--argc;
                        c = atof(&argv[2][0]);
                        ++argv;--argc;
                        if (argc > 2 && (isdigit(argv[2][0]) || 
                            isdigit(argv[2][1]) || isdigit(argv[2][2]))) {
                            x2 = atof(&argv[2][0]);
                            ++argv;--argc;
                            y2 = atof(&argv[2][0]);
                            ++argv;--argc;
                            z2 = atof(&argv[2][0]);
                            ++argv;--argc;
                            x3 = atof(&argv[2][0]);
                            ++argv;--argc;
                            y3 = atof(&argv[2][0]);
                            ++argv;--argc;
                            z3 = atof(&argv[2][0]);
                            ++argv;--argc;

                            Redcat::Rotation(a, b, c, x2, y2, z2, x3, y3, z3);
                        }//end if
                        else {
                            Redcat::Rotation(a, b, c);

                        }//end else
                        break;

                    case 's':  //solve
                        if(!quietFlag)
                            cout << "solve" << endl << endl;

                        if(argv[1][2] == NULL){
                            repeats = atoi(&argv[2][0]);
                            ++argv;--argc;

                        }//end if
                        else{
                            repeats = atoi(&argv[1][2]);

                        }//end else
                        redcat->Solve(repeats);
                        break;

                    case 't': //test
                        if(!quietFlag)
                            cout << "testing" << endl << endl;

                        redcat->Test();
                        break;

                    case 'v': //version
                        cout << version << endl << endl;
                        break;
                    /*
                    case '-': //textOption
                        if(argv[1][0] == 'k'){ //use sockets


                            break;

                        }//end if
                        else {
                            cout << "Unknown option" << endl;
                            usage();

                        }//end else
                    */
                    default :
                        cerr << "Unknown parameter " << argv[1] << endl << endl;
                        usage();

		}//end switch
		++argv;--argc;

	}//end while
    
    //clean up
    delete infile;
    delete outfile;
    delete stdInString;
    if(rCatIsSet)
        delete redcat;

    return (EXIT_SUCCESS);

}//end main(int,char**)

