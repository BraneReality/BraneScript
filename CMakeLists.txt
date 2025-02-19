cmake_minimum_required(VERSION 3.23)
include(FetchContent)

project(BraneScript
        VERSION "0.2.0"
        DESCRIPTION "BraneScript"
        HOMEPAGE_URL "https://github.com/BraneReality/branescript"
        LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
option(BS_BUILD_TESTS "Build tests" ON)
option(ENABLE_TREESITTER_DEV "Disable fetch content version requirement so local dev can occur" OFF)
# Fetch and run our parser generator

set(TSBS_SOURCE_DIR ${CMAKE_SOURCE_DIR}/deps/TreeSitterBraneScript)
if(ENABLE_TREESITTER_DEV AND EXISTS ${TSBS_SOURCE_DIR}/grammar.js)
    message("Skipping fetch of TreeSitterBraneScript as ENABLE_TREESITTER_DEV is set")
    FetchContent_Declare(
        TreeSitterBraneScript
        SOURCE_DIR ${TSBS_SOURCE_DIR}
    )
else()
    FetchContent_Declare(
        TreeSitterBraneScript
        GIT_REPOSITORY https://github.com/BraneReality/TreeSitterBraneScript.git 
        GIT_TAG main
        SOURCE_DIR ${TSBS_SOURCE_DIR}
    )
endif()

set(BUTILS_SOURCE_DIR ${CMAKE_SOURCE_DIR}/deps/BraneUtilities)
FetchContent_Declare(
    BraneUtilities
    GIT_REPOSITORY https://github.com/BraneReality/BraneUtilities.git
    GIT_TAG main
    SOURCE_DIR ${BUTILS_SOURCE_DIR}
)

FetchContent_MakeAvailable(TreeSitterBraneScript)
FetchContent_MakeAvailable(BraneUtilities)

if(MINGW)
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS_DEBUG} -static -static-libgcc -static-libstdc++")
    if(${CMAKE_BUILD_TYPE} STREQUAL "Debug")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g2")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g2")
    endif()
endif(MINGW)

add_subdirectory(src)

if(BS_BUILD_TESTS)
    enable_testing()
    add_subdirectory(tests)
endif()
