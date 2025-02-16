
find_package(unofficial-tree-sitter CONFIG REQUIRED)
#find_package(TreeSitterBraneScript REQUIRED)

add_library(parser STATIC 
    documentParser.cpp
    documentContext.cpp
)
target_link_libraries(parser PUBLIC unofficial::tree-sitter::tree-sitter tree-sitter-branescript types)
