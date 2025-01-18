##
## Example of use:
##    cmake -DCMAKE_BUILD_TYPE=Debug -DCMAKE_CXX_COMPILER=clang++-18 -DCMAKE_EXPORT_COMPILE_COMMANDS=1 -GNinja ..
##
## ----------------------------------------------------------------------------------

cmake_minimum_required(VERSION 3.24...3.31)

project(gf)

set( CMAKE_CXX_STANDARD 23 )
set( CMAKE_CXX_EXTENSIONS OFF )
set( CXX_STANDARD_REQUIRED ON)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

## ------------------------------------------------------------------------------------
##                                  Required packages
## ------------------------------------------------------------------------------------
if (UNIX)
    find_package(Freetype REQUIRED)
    find_package(X11 REQUIRED)
    find_package(Threads REQUIRED)
endif()

## ------------------------------------------------------------------------------------
##                                  Compiler options
## ------------------------------------------------------------------------------------
if(NOT MSVC)
   add_compile_options(-Wall -Wextra -Wdisabled-optimization -Winit-self -Wmissing-include-dirs -Wswitch-default
                       -Wno-unused -Wno-unused-parameter -Wno-missing-field-initializers
                       -Wno-deprecated-anon-enum-enum-conversion -Wno-deprecated-enum-enum-conversion
                       -Wno-gnu-conditional-omitted-operand)
   if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
      add_compile_options(-Wlogical-op -Wstrict-null-sentinel)
   else()
      add_compile_options(-pedantic)
   endif()
else()
   add_compile_options(/W4 /Zc:__cplusplus)
endif()

## ------------------------------------------------------------------------------------
##                                    luigi
## ------------------------------------------------------------------------------------
add_library(luigi STATIC src/luigi.cpp)
set_property(TARGET luigi PROPERTY PUBLIC_HEADER src/luigi.hpp)

if (NOT MSVC)
    target_link_libraries(luigi ${FREETYPE_LIBRARIES} ${X11_LIBRARIES})
    target_include_directories(luigi PUBLIC ${FREETYPE_INCLUDE_DIRS} PRIVATE ${X11_INCLUDE_DIR})
    
    target_compile_options(luigi PRIVATE -DUI_SSE2 -DUI_UNICODE)
    target_compile_options(luigi PUBLIC  -DUI_LINUX -DUI_FREETYPE -DUI_FREETYPE_SUBPIXEL)
else()
    target_compile_options(luigi PRIVATE -DUI_SSE2)
    target_compile_options(luigi PUBLIC -DUI_WINDOWS)
endif()

## ------------------------------------------------------------------------------------
##                                    gf
## ------------------------------------------------------------------------------------
if (NOT MSVC)
    add_executable(${PROJECT_NAME} WIN32 src/gf.cpp src/re/cpp.cpp src/re/gdb.cpp)

    ## dmalloc: link with `-ldmalloc` and run command: `dmalloc -l logfile -i 100 high`
    
    target_include_directories(${PROJECT_NAME} PRIVATE src deps/include )
    target_link_libraries(${PROJECT_NAME} luigi Threads::Threads)
endif ()


## ------------------------------------------------------------------------------------
##                                    Sanitizers
## ------------------------------------------------------------------------------------
option(ENABLE_SANITIZERS         "Build with sanitizers enabled" OFF)
option(ENABLE_ADDRESS_SANITIZER  "Build with address sanitizers enabled" OFF)

if (ENABLE_SANITIZERS OR ENABLE_ADDRESS_SANITIZER)
    if (ENABLE_ADDRESS_SANITIZER)
        message( STATUS "Configuring address sanitizer" )
        set(clang_san_options -g -fsanitize=address -fno-omit-frame-pointer)
    else()
        message( STATUS "Configuring other sanitizers" )
        set(clang_san_options -g -fsanitize=undefined -fno-sanitize-recover=all -fsanitize=float-divide-by-zero
                                 -fsanitize=float-cast-overflow -fno-sanitize=null -fno-sanitize=alignment -fPIE
                                 -fno-omit-frame-pointer -fsanitize-ignorelist=${CMAKE_SOURCE_DIR}/san_ignore_list.txt)
    endif()
    
    target_compile_options(luigi PUBLIC ${clang_san_options} )
    target_link_options(   luigi PUBLIC ${clang_san_options} )

    target_compile_options(${PROJECT_NAME} PUBLIC ${clang_san_options} )
    target_link_options(   ${PROJECT_NAME} PUBLIC ${clang_san_options} )

endif()


## ------------------------------------------------------------------------------------
##                                    Subdirectories
## ------------------------------------------------------------------------------------
add_subdirectory(examples)
add_subdirectory(tests)