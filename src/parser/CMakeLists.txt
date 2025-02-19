
find_package(unofficial-tree-sitter CONFIG REQUIRED)
#find_package(TreeSitterBraneScript REQUIRED)

add_library(parser STATIC 
    documentParser.cpp
    documentContext.cpp
)

target_link_libraries(parser PUBLIC unofficial::tree-sitter::tree-sitter tree-sitter-branescript types)
target_include_directories(parser PRIVATE ${treesitterbranescript_SOURCE_DIR}/src)
target_include_directories(parser PUBLIC ${treesitterbranescript_SOURCE_DIR}/bindings/c)

find_package(Python COMPONENTS Interpreter REQUIRED)
set(PARSER_C "${treesitterbranescript_SOURCE_DIR}/src/parser.c")
set(PARSER_ENUMS "${treesitterbranescript_SOURCE_DIR}/src/parserEnums.h")
add_custom_target(generate_parser_c DEPENDS ${PARSER_C})
add_custom_command(OUTPUT "${PARSER_ENUMS}"
   DEPENDS generate_parser_c
   COMMAND "${Python_EXECUTABLE}" "${treesitterbranescript_SOURCE_DIR}/extractEnums.py" "${PARSER_C}" "${PARSER_ENUMS}" ts_symbol_identifiers ts_field_identifiers
   WORKING_DIRECTORY "${treesitterbranescript_SOURCE_DIR}"
   COMMENT "Generating parserEnums.h")

add_custom_target(generate_parser_enums DEPENDS ${PARSER_ENUMS})
add_dependencies(parser generate_parser_enums)

if(MINGW)
    target_compile_options(parser PRIVATE -Wmissing-field-initializers)
endif()
