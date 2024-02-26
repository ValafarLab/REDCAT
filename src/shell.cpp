/**
 * shell.cpp
 * Julian Rachele
 * Wrapper for accessing REDCAT binaries via the command line
 */
#include <algorithm>
#include <iostream>
#include <cstdlib>
#include <cstring>
#include <string>
#include <sstream>

#include <unistd.h>
#include <limits.h>

void usage() {
    std::cout << "Usage: redcat [-S|--script] <binary|script> [options]\nSee redcat --help or the "
				 "REDCAT documentation for details\n";
}

std::string INSTALL_PREFIX = std::string(INSTALL_PATH);

int main(int argc, char* argv[]) {
    if (argc < 2) {
        usage();
        return 0;
    }
    std::string PATH = INSTALL_PREFIX + "/redcat/";
    int offset;
    if (strcmp("-S", argv[1]) == 0 || strcmp("--script", argv[1]) == 0) {
        // execute a redcat script
        PATH += "/scripts/";
        offset = 2;
        if (argc < 3) {
            usage();
            return 0;
        }
    } else if (strcmp("-H", argv[1]) == 0 || strcmp("--help", argv[1]) == 0) {
        usage();
	std::string cmd = "ls ";
#ifdef _WIN32
	cmd = "dir ";
#endif
	std::string full_path = INSTALL_PREFIX + "/redcat/";
    	std::cout << "\nList of available binaries:" << std::endl;
	system((cmd + "\"" + full_path + "bin" + "\"").c_str());
	std::cout << "\nList of available scripts:" << std::endl;
	system((cmd + "\"" + full_path + "scripts" + "\"").c_str());
	return 0;
    } else {
        // execute a redcat binary
        PATH += "/bin/";
        offset = 1;
    }
    std::stringstream soptions;
    std::string options;
    // PATH = "\"" + PATH + "\"";
    soptions << "\"" << PATH << argv[offset];
    for (int i = offset+2; i <= argc; i++) {
        soptions << " \"" << argv[i-1] << "\"";
    }
    soptions << "\"";
    try {
        options = soptions.str();
    } catch (const std::exception &) {
        std::cerr << "Improper formatting\n";
        return -1;
    }
    options = "\"" + options + "\"";
//    std::cout << options << std::endl;
    system(options.c_str());
}

