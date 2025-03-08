
find_package(unofficial-tree-sitter CONFIG REQUIRED)

add_library(parser STATIC 
    documentParser.cpp
    documentContext.cpp
)

target_link_libraries(parser PUBLIC unofficial::tree-sitter::tree-sitter tree-sitter-branescript types utilities)
target_include_directories(parser PRIVATE ${treesitterbranescript_SOURCE_DIR}/src)
target_include_directories(parser PUBLIC ${treesitterbranescript_SOURCE_DIR}/bindings/c)
target_include_directories(parser PUBLIC ${braneutilities_SOURCE_DIR}/src)

if(MINGW)
    target_compile_options(parser PRIVATE -Wmissing-field-initializers)
    if(${CMAKE_BUILD_TYPE} STREQUAL "Debug")
        target_compile_options(parser PRIVATE  -g -Og)
    endif()
endif(MINGW)
