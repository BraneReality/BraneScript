
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
include_directories(${CMAKE_CURRENT_SOURCE_DIR})

add_subdirectory(ir)
add_subdirectory(cli)
add_subdirectory(parser)
#add_subdirectory(compiler)
add_subdirectory(types)

