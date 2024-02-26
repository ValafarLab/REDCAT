cmake_minimum_required (VERSION 3.1)
set(CMAKE_CXX_STANDARD 11)

if(APPLE)
    set(CMAKE_C_COMPILER "/usr/local/bin/gcc-11")
    set(CMAKE_CXX_COMPILER "/usr/local/bin/g++-11")
endif()
if(WIN32)
    set(CMAKE_C_COMPILER "gcc.exe")
    set(CMAKE_CXX_COMPILER "g++.exe")
endif()


project (Redcat)

# Enable multicore processing
include(FindOpenMP)
if(OPENMP_FOUND)
    message("OpenMP works, here are the flags: ${OpenMP_CXX_FLAGS}")
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
endif()


# REDCAT binaries
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib)
 
# Includes the Eigen project
include_directories(src)

set (REDCAT
        src/tensor.cpp
        src/main.cpp
        src/SVDiface.cpp
        src/matrix.cpp)

add_library(redcat_lib ${REDCAT})

add_executable(redcat src/shell.cpp)
target_compile_definitions(redcat PRIVATE INSTALL_PATH="${CMAKE_INSTALL_PREFIX}")

add_executable(redcatbin src/Redcat.cpp)
target_link_libraries(redcatbin redcat_lib)
add_executable(dynamic src/Dynamic.cpp)

#add_custom_target(uninstall
#	COMMAND
#		xargs rm < install_manifest.txt
#	COMMENT "Uninstalled REDCAT"
#)

install(TARGETS redcat DESTINATION ${CMAKE_INSTALL_PREFIX}/bin)
install(TARGETS redcatbin DESTINATION ${CMAKE_INSTALL_PREFIX}/redcat/bin)
install(TARGETS dynamic DESTINATION ${CMAKE_INSTALL_PREFIX}/redcat/bin)
install(DIRECTORY scripts/ DESTINATION ${CMAKE_INSTALL_PREFIX}/redcat/scripts USE_SOURCE_PERMISSIONS)
install(DIRECTORY vmd/ DESTINATION ${CMAKE_INSTALL_PREFIX}/redcat/vmd USE_SOURCE_PERMISSIONS)

add_custom_target(uninstall
	COMMAND
		xargs rm < install_manifest.txt
	COMMENT "Uninstalled REDCAT"
)

if(UNIX AND NOT APPLE)
file(MAKE_DIRECTORY /usr/share/Redcat/)
file(WRITE /usr/share/Redcat/redcat.conf ${CMAKE_INSTALL_PREFIX}/redcat)
endif()
