


find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")


add_library(llvmJitBackend STATIC llvmJitBackend.cpp)
target_include_directories(llvmJitBackend PRIVATE ${LLVM_INCLUDE_DIRS})
target_compile_definitions(llvmJitBackend PRIVATE ${LLVM_DEFINITIONS})

llvm_map_components_to_libnames(LLVM_LIBS support core irreader executionengine asmprinter nativecodegen orcjit transformutils target analysis x86asmparser)
message(STATUS "Linking llvm libs ${LLVM_LIBS}")
target_link_libraries(llvmJitBackend PRIVATE ${LLVM_LIBS})
target_include_directories(llvmJitBackend PUBLIC ${braneutilities_SOURCE_DIR}/src)
