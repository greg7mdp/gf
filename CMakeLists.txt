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
if (UNIX)
    find_package(Freetype REQUIRED)
    find_package(X11 REQUIRED)
    find_package(Threads REQUIRED)
endif (UNIX)

## ----------------- gf
set(cpp_files "src/luigi.cpp" "examples/luigi_example.cpp")
file(GLOB_RECURSE cpp_files "src/*.cpp")
#file(GLOB_RECURSE header_files "src/*.hpp")

add_executable(${PROJECT_NAME} WIN32 ${cpp_files} ${header_files})
set_property(TARGET ${PROJECT_NAME} PROPERTY PUBLIC_HEADER ${header_files})

## dmalloc: link with `-ldmalloc` and run command: `dmalloc -l logfile -i 100 high`

if (UNIX)
    target_link_libraries(${PROJECT_NAME} ${FREETYPE_LIBRARIES} ${X11_LIBRARIES} Threads::Threads)
    target_include_directories(${PROJECT_NAME} PRIVATE ${FREETYPE_INCLUDE_DIRS} ${X11_INCLUDE_DIR} )
    target_compile_options(${PROJECT_NAME} PRIVATE -DUI_LINUX -DUI_FONT_PATH -DUI_SSE2 -DUI_FREETYPE -DUI_FREETYPE_SUBPIXEL -DUI_UNICODE)

    target_compile_options(${PROJECT_NAME} PRIVATE 
                           -Wall -Wextra -Wno-unused-parameter
                           -Wno-missing-field-initializers  -Wno-deprecated-anon-enum-enum-conversion)

    set(clang_san_options -g -fsanitize=address)
    #set(clang_san_options -g -fsanitize=memory -fsanitize-memory-track-origins -fsanitize-recover=memory -fPIE -pie -fno-omit-frame-pointer -fsanitize-ignorelist=${CMAKE_SOURCE_DIR}/san_ignore_list.txt)

    target_compile_options(${PROJECT_NAME} PUBLIC ${clang_san_options} )
    target_link_options(   ${PROJECT_NAME} PUBLIC ${clang_san_options} )

endif (UNIX)

if (MSVC)
    target_compile_options(${PROJECT_NAME} PRIVATE -DUI_WINDOWS -DUI_SSE2)
endif (MSVC)
