
add_library(compiler STATIC 
    compiler.cpp
)

target_link_libraries(compiler PUBLIC ir parser)


if(MINGW)
    if(${CMAKE_BUILD_TYPE} STREQUAL "Debug")
        target_compile_options(compiler PRIVATE -g -Og)
    endif()
endif(MINGW)
