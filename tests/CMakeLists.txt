
add_executable(bs_tests testing.cpp emptyPlaceholder.cpp)

find_package(GTest CONFIG REQUIRED)
target_link_libraries(bs_tests PRIVATE GTest::gtest_main)
#target_link_libraries(bs_tests PUBLIC GTest::gtest_main BraneScriptCompiler)

include(GoogleTest)
gtest_discover_tests(bs_tests WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/executables)

file(GLOB TEST_SCRIPTS ${CMAKE_CURRENT_SOURCE_DIR}/*.bs)
file(COPY ${TEST_SCRIPTS} DESTINATION ${CMAKE_BINARY_DIR}/executables/testScripts)
