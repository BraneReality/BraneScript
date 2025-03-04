
// Created by WireWhiz on 10/22/2024.
//

#include <cstring>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>
#include "parser/documentParser.h"
#include "runtime/backends/llvm/llvmJitBackend.h"
#include "tree-sitter-branescript.h"
#include <string_view>

#include "compiler/compiler.h"
#include "ir/irTextSerializer.h"
#include "tree_sitter/api.h"

// #include "TSBindings.h"


void print_tree(TSNode node, std::string_view source, int currentDepth = 0)
{
    for(int i = 0; i < currentDepth; i++)
        printf(" | ");

    if(ts_node_is_null(node))
    {
        printf("NULL!");
        return;
    }

    printf("%s", ts_node_type(node));
    if(ts_node_child_count(node) == 0)
    {
        auto start = ts_node_start_byte(node);
        auto end = ts_node_end_byte(node);
        printf(" text:\"%.*s\"", end - start, source.data() + start);
    }

    if(ts_node_is_extra(node))
        printf(" - Extra");
    if(ts_node_is_missing(node))
        printf(" - Missing");
    if(!ts_node_is_named(node))
        printf(" - Unnamed");


    printf("\n");

    int n = -1; // Named Node index
    for(int i = 0; i < ts_node_child_count(node); i++)
    {
        for(int i = 0; i < currentDepth; i++)
            printf(" | ");
        TSNode child = ts_node_child(node, i);
        if(ts_node_is_named(child))
            n += 1;
        printf("%d, %d ", i, n);

        const char* field = ts_node_field_name_for_child(node, i);
        if(field)
        {
            bool matches = ts_node_child_by_field_name(node, field, strlen(field)).id == child.id;
            printf("field: \"%s\" matches expected = %s", field, matches ? "true" : "false");
            if(!matches)
                printf("Was epxecting: %s", ts_node_type(ts_node_child_by_field_name(node, field, strlen(field))));
        }
        printf("\n");
        print_tree(child, source, currentDepth + 1);
    }
}

int main(int argc, char* argv[])
{
    std::cout << "Hello world!" << std::endl;
    std::cout << "Running in dir: " << std::filesystem::current_path() << std::endl;

    if(argc < 2)
    {
        std::cout << "Must provide file to parse!" << std::endl;
        return 1;
    }

    std::ifstream f(argv[1], std::ios::binary | std::ios::ate);
    if(!f.is_open())
    {
        std::cout << "File \"" << argv[1] << "\" does not exist!" << std::endl;
        return 1;
    }
    std::string source_code;
    auto count = f.tellg();

    source_code.resize(count);
    f.seekg(0);
    f.read(source_code.data(), count);
    f.close();

    std::cout << "---Parsing---\n" << source_code << "\n---END---" << std::endl;
    TSParser* parser = ts_parser_new();
    const TSLanguage* braneScriptLang = tree_sitter_branescript();
    ts_parser_set_language(parser, braneScriptLang);

    TSTree* tree = ts_parser_parse_string(parser, nullptr, source_code.c_str(), source_code.size());

    TSNode root_node = ts_tree_root_node(tree);

    /*char *string = ts_node_string(root_node);*/
    /*printf("Syntax tree: %s\n", string);*/
    /*free(string);*/

    print_tree(root_node, source_code);

    printf("Parsing DocumentContext...\n");
    auto bs_parser = std::make_shared<BraneScript::BraneScriptParser>();

    auto doc = std::make_shared<BraneScript::ParsedDocument>(argv[1], source_code, bs_parser);

    auto parseRes = doc->getDocumentContext();

    if(!parseRes.messages.empty())
    {
        printf("Parsed with messages:\n");
        for(auto& err : parseRes.messages)
            printf("[line %d, char %d]: %s\n",
                   err.range.start_point.row,
                   err.range.start_point.column,
                   err.message.c_str());
    }

    printf("Found modules:\n");
    for(auto& mod : parseRes.document->modules)
        printf("%s\n", mod.second->identifier->text.c_str());

    BraneScript::Compiler compiler;
    Option<std::vector<BraneScript::IRModule>> compileRes = compiler.compile({doc});
    if(!compileRes)
        std::cout << "Failed to compile!" << std::endl;
    else
        std::cout << "compile completed with messages:" << std::endl;
    for(auto& m : compiler.messages())
    {
        printf("[line %d, char %d]: %s\n",
               m.source.range.value().start_point.row,
               m.source.range.value().start_point.column,
               m.message.c_str());
    }

    if(compileRes)
    {
        for(auto& mod : compileRes.value())
        {
            std::cout << "Compiled module: " << mod.id << std::endl;
            auto writeRes = BraneScript::IRSerializer::irToText(mod);
            if(writeRes)
                std::cout << "IR: " << writeRes.ok() << std::endl;
            else
                std::cout << "Failed to write ir: " << writeRes.err() << std::endl;
        }
    }
    else
        return 1;

    BraneScript::LLVMJitBackend backend;
    backend.stageModule(std::make_shared<BraneScript::IRModule>(compileRes.value()[0]));
    backend.processModules();


    for(auto& func : backend.functions())
    {
        std::cout << "Was able to load function: " << func.first << std::endl;
    }


    ts_tree_delete(tree);
    ts_parser_delete(parser);

    return 0;
}
