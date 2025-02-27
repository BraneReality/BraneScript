
add_library(ir STATIC nodes.cpp ir.cpp irTextSerializer.cpp)
target_include_directories(ir PUBLIC ${braneutilities_SOURCE_DIR}/src)
