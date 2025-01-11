file(GLOB cpp "*.cpp")

get_filename_component(dirname ${CMAKE_CURRENT_SOURCE_DIR} NAME)
string(REPLACE " " "_" dirname ${dirname})

foreach(f IN LISTS cpp)
    get_filename_component(target ${f} NAME_WE)
    add_executable("${target}" "${f}")
    target_include_directories(${target} PRIVATE ${CMAKE_SOURCE_DIR}/deps/include ${CMAKE_SOURCE_DIR}/src)
    target_link_libraries(${target} luigi)
    set_target_properties("${target}" PROPERTIES OUTPUT_NAME "${target}" FOLDER "${dirname}")
endforeach()
