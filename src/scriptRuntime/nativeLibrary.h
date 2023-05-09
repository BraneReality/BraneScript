//
// Created by wirewhiz on 2/24/23.
//

#ifndef BRANESCRIPT_NATIVELIBRARY_H
#define BRANESCRIPT_NATIVELIBRARY_H


#include <string>
#include <vector>
#include "functionHandle.h"

namespace BraneScript
{
    class Linker;
    void addNativeFunctions(Linker& linker);

    struct NativeLibrary
    {
        std::string identifier;
        std::vector<std::pair<std::string, void*>> functions;
        void addFunction(std::string signature, void* f);
        template<typename Ret, typename... Args>
        void addFunction(std::string signature, FunctionHandle<Ret, Args...> f)
        {
            addFunction(std::move(signature), (void*)f);
        }
    };
}


#endif // BRANESCRIPT_NATIVELIBRARY_H
