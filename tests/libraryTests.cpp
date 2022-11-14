
#include "testing.h"

#include "../scripting/compiler.h"
#include "../scripting/scriptRuntime.h"
#include "../scripting/script.h"
#include "../scripting/library.h"
#include "../scripting/linker.h"

using namespace BraneScript;

int refValue = 0;

void BS_API_CALL setRef(int newVal)
{
    refValue = newVal;
}

TEST(BraneScript, Libraries)
{
    std::string testString = R"(
    link "testLib" as "lib";

    void setRef(int v)
    {
        lib.setRef(v);
    }
)";
    Library testLib("testLib");
    testLib.addFunction("setRef", setRef);

    Linker linker;
    linker.addLibrary(&testLib);

    Compiler compiler;
    compiler.setLinker(&linker);
    auto* ir = compiler.compile(testString);
    checkCompileErrors(compiler);
    ASSERT_TRUE(ir);

    ScriptRuntime rt;
    rt.setLinker(&linker);
    Script* testScript = rt.assembleScript(ir);
    checkCompileErrors(compiler);
    ASSERT_TRUE(testScript);

    auto scriptSetRef = testScript->getFunction<void, int>("setRef");
    ASSERT_TRUE(scriptSetRef);
    scriptSetRef(5);

    EXPECT_EQ(refValue, 5);
}