
#include "testing.h"

#include "nativeTypes/bsLambda.h"
#include "nativeTypes/bsString.h"
#include "script.h"
#include "scriptRuntime.h"

using namespace BraneScript;

TEST(BraneScript, Lambdas)
{
    BSLambda<int> testLambda;
    ASSERT_FALSE(testLambda);

    BSLambda<int> testLambda2((FuncRef<int, void*>)[](void* data)->int{
        return std::get<0>(*(std::tuple<int, float>*)data);
    }, 5, 2.f);
    ASSERT_EQ(testLambda2(), 5);
    ASSERT_TRUE(testLambda2);

    testLambda = testLambda2;
    ASSERT_TRUE(testLambda);

    ASSERT_NE(testLambda.data(), testLambda2.data());
    ASSERT_EQ(testLambda(), 5);
    ASSERT_EQ(testLambda2(), 5);

    Parser analyzer;
    std::string path = "testScripts/lambdaTests.bs";
    analyzer.load(path);
    analyzer.validate(path);
    checkCompileErrors(analyzer, path);

    auto ir = analyzer.compile(path, CompileFlags_DebugInfo | CompileFlags_PrintIR);
    ASSERT_TRUE(ir.modules.contains("tests"));

    ScriptRuntime rt;
    rt.resetMallocDiff();
    rt.loadDefaultModules();
    auto testScript = rt.loadModule(ir.modules.at("tests"));

    auto returnInt = testScript->getFunction<void, BSLambda<int>&, int>("tests::returnInt(ref lambda::Lambda<int>,int)");
    ASSERT_TRUE(returnInt);
    BSLambda<int> lambda;
    returnInt(lambda, 5);
    ASSERT_TRUE(lambda);
    EXPECT_EQ(lambda.captureSize(), sizeof(std::tuple<int>));
    EXPECT_EQ(lambda(), 5);

    auto callIntLambda = testScript->getFunction<int, BSLambda<int>&>("tests::callIntLambda(ref lambda::Lambda<int>)");
    ASSERT_TRUE(callIntLambda);
    EXPECT_EQ(callIntLambda(lambda), 5);

    auto returnString = testScript->getFunction<void, BSLambda<BSString>&, BSString&>("tests::returnString(ref lambda::Lambda<string::string>,ref string::string)");
    ASSERT_TRUE(returnString);
    BSLambda<BSString> lambda2;
    BSString str = "Hello World!";
    returnString(lambda2, str);
    ASSERT_TRUE(lambda2);
    EXPECT_EQ(lambda2.captureSize(), sizeof(std::tuple<BSString>));

    auto callStringLambda = testScript->getFunction<void, BSString&, BSLambda<BSString>&>("tests::callStringLambda(ref string::string,ref lambda::Lambda<string::string>)");
    ASSERT_TRUE(callStringLambda);
    BSString ret;
    callStringLambda(ret, lambda2);
    EXPECT_STREQ(ret.data(), "Hello World!");

    auto testSimpleRefCapture = testScript->getFunction<int, int>("tests::testSimpleRefCapture(int)");
    ASSERT_TRUE(testSimpleRefCapture);
    EXPECT_EQ(testSimpleRefCapture(5), 5);

    auto testStructRefCapture = testScript->getFunction<void, BSString&, BSString&>("tests::testStructRefCapture(ref string::string,ref string::string)");
    ASSERT_TRUE(testStructRefCapture);
    BSString ret2;
    testStructRefCapture(ret2, str);
    EXPECT_STREQ(ret2.data(), "Hello World!");
}
