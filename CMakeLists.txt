cmake_minimum_required(VERSION 3.23)
include(FetchContent)

project(BraneScript
        VERSION "0.2.0"
        DESCRIPTION "BraneScript"
        HOMEPAGE_URL "https://github.com/wirewhiz/branescript"
        LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
option(BS_BUILD_TESTS "Build tests" ON)
option(TreeSitterBS_DIR "Set a manual path to use for the TreeSitterBraneScript repo")


# Fetch and run our parser generator
set(TSBS_REPO https://github.com/BraneReality/TreeSitterBraneScript.git)
if(TSBS_DIR)
    FetchContent_Declare(
        TreeSitterBraneScript
        GIT_REPOSITORY ${TSBS_REPO} 
        GIT_TAG main
        SOURCE_DIR ${TSBS_DIR}
    )
else()
    FetchContent_Declare(
        TreeSitterBraneScript
        GIT_REPOSITORY ${TSBS_REPO} 
        GIT_TAG main
    )
endif()
FetchContent_MakeAvailable(TreeSitterBraneScript)

set(GRAMMAR_DIR ${treesitterbranescript_SOURCE_DIR})

message("Grammar at ${GRAMMAR_DIR}")
set(GRAMMAR_FILE "${GRAMMAR_DIR}/grammar.js")
set(GRAMMAR_BUILD_DIR ${CMAKE_BINARY_DIR}/ts-branescript)
set(GENERATE_PARSER_CMD tree-sitter generate -b ${GRAMMAR_FILE})
set(PARSER_LIB_NAME parser${CMAKE_SHARED_LIBRARY_SUFFIX})
set(BUILD_PARSER_CMD tree-sitter build -o ${GRAMMAR_BUILD_DIR}/${PARSER_LIB_NAME})

if(MINGW)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS_DEBUG} -static -static-libgcc -static-libstdc++")
    if(${CMAKE_BUILD_TYPE} STREQUAL "Debug")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g2")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g2")
    endif()
endif(MINGW)

#[[file(MAKE_DIRECTORY ${GRAMMAR_BUILD_DIR})
message("Watching grammar at ${GRAMMAR_FILE}")
add_custom_command(
    OUTPUT ${GRAMMAR_BUILD_DIR}/ts-build-command.stamp
    COMMAND echo ${GENERATE_PARSER_CMD} 
    COMMAND ${GENERATE_PARSER_CMD} 
    COMMAND echo ${BUILD_PARSER_CMD}
    COMMAND ${BUILD_PARSER_CMD}

    WORKING_DIRECTORY ${GRAMMAR_BUILD_DIR}
    DEPENDS ${GRAMMAR_FILE}
    COMMENT "Generate ts parser because grammar.js changed"
)
add_custom_target(
    tree_sitter_bs_parser 
    DEPENDS 
    ${GRAMMAR_BUILD_DIR}/ts-build-command.stamp
)]]

add_subdirectory(src)

if(BS_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()
