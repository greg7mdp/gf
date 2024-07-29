##
## Example of use:
##    cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_CXX_COMPILER=g++ -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -GNinja ..
##
## ----------------------------------------------------------------------------------

cmake_minimum_required(VERSION 3.24)

project(gf)

set( CMAKE_CXX_STANDARD 20 )
set( CMAKE_CXX_EXTENSIONS OFF )
set( CXX_STANDARD_REQUIRED ON)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

## ----------------- Required packages
find_package(Freetype REQUIRED)
find_package(X11 REQUIRED)
find_package(Threads REQUIRED)

## ----------------- gf
set(EXE_NAME gf)
add_executable(${EXE_NAME} src/gf2.cpp src/luigi2.cpp)

target_link_libraries(${EXE_NAME} ${FREETYPE_LIBRARIES} ${X11_LIBRARIES} Threads::Threads)
target_include_directories(${EXE_NAME} PRIVATE ${FREETYPE_INCLUDE_DIRS} ${X11_INCLUDE_DIR} )
target_compile_options(${EXE_NAME} PRIVATE -DUI_LINUX -DUI_FONT_PATH -DUI_SSE2 -DUI_FREETYPE)

target_compile_options(${EXE_NAME} PRIVATE -Wall -Wextra -Wno-unused-parameter -Wno-unused-result
                                   -Wno-missing-field-initializers -Wno-format-truncation)