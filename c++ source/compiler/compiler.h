#ifndef BRANESCRIPT_COMPILER_H
#define BRANESCRIPT_COMPILER_H

#include "../ir/ir.h"
#include "../parser/documentParser.h"
#include "enums/result.h"
#include <unordered_map>
#include <vector>

namespace BraneScript {
enum class CompilerMessageType {
  Critical = 0,
  Error = 1,
  Warning = 2,
  Log = 3,
  Verbose = 4,
};

struct CompilerSource {
  std::string uri;
  Option<TSRange> range;
};

struct CompilerMessage {
  CompilerMessageType type;
  CompilerSource source;
  std::string message;
};

template <class T>
using CompileResult = Result<IRNode<T>, std::vector<CompilerMessage>>;

using Identifiable =
    std::variant<IRNode<IRModule>, IRNode<IRPipeline>, IRNode<IRFunction>>;

class Compiler {
  std::vector<CompilerMessage> _messages;

public:
  Option<std::vector<IRModule>>
  compile(const std::vector<std::shared_ptr<ParsedDocument>> &documents);

  const std::vector<CompilerMessage> &messages() const;
};
} // namespace BraneScript

#endif
