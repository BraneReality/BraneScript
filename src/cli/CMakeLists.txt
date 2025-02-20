
# LLVM package stuff

#[[find_package(LLVM CONFIG REQUIRED)
list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(HandleLLVMOptions)
add_definitions(${LLVM_DEFINITIONS})
target_include_directories(main PRIVATE ${LLVM_INCLUDE_DIRS})
# Find the libraries that correspond to the LLVM components that we wish to use
llvm_map_components_to_libnames(llvm_libs Support Core IRReader ...)
# Link against LLVM libraries
target_link_libraries(main PRIVATE ${llvm_libs})]]

add_executable(BraneScriptCli main.cpp)
target_link_libraries(BraneScriptCli PRIVATE parser)

install(TARGETS BraneScriptCli
    RUNTIME_DEPENDENCIES
    PRE_EXCLUDE_REGEXES 
        ".*tree-sitter-branescript.*"
        "api-ms-.*" "ext-ms-.*"  # Windows system DLLs
        "^/lib.*"                # Linux system libraries
        "^/usr/lib.*"
    POST_EXCLUDE_REGEXES
        "libgcc_s_seh-1\\.dll"
        "libstdc++-6\\.dll" 
        ".*system32/.*\\.dll"    # Windows system directory
        "^/lib64/.*"            # Linux system directories
        "^/usr/lib64/.*"
    DESTINATION .
    COMPONENT cli)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/BraneScriptCli.pdb 
        DESTINATION .
        COMPONENT cli
        OPTIONAL)
install(FILES ${treesitterbranescript_BINARY_DIR}/${CMAKE_SHARED_LIBRARY_PREFIX}tree-sitter-branescript${CMAKE_SHARED_LIBRARY_SUFFIX}
    DESTINATION .
    COMPONENT cli)
install(DIRECTORY ${PROJECT_SOURCE_DIR}/exampleScripts 
    DESTINATION .
    COMPONENT cli)
