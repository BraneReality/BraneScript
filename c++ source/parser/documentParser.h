//
// Created by WireWhiz on 10/28/2024.
//

#ifndef BRANESCRIPT_TSBINDINGS_H
#define BRANESCRIPT_TSBINDINGS_H

#include "documentContext.h"
#include <memory>
#include <string>
#include <tree_sitter/api.h>

namespace BraneScript {
class BraneScriptParser {
private:
  TSParser *_value;

public:
  BraneScriptParser();
  BraneScriptParser(BraneScriptParser &&) noexcept;
  ~BraneScriptParser();

  TSParser *parser() const;
};

enum class MessageType { Error = 0, Warning, Log, Verbose };

struct ParserMessage {
  MessageType type;
  TSRange range;
  std::string message;
};

template <class T> struct ParserResult {
  Node<T> document;
  std::vector<ParserMessage> messages;
};

class ParsedDocument {
  std::filesystem::path _path;
  std::string _source;
  std::shared_ptr<BraneScriptParser> _parser;

  Option<ParserResult<DocumentContext>> _cachedResult;

public:
  ParsedDocument(std::filesystem::path path, std::string source,
                 std::shared_ptr<BraneScriptParser> parser);

  std::string_view source() const;

  void update(TSRange updateRange, std::string newtext);

  ParserResult<DocumentContext> getDocumentContext();
};

TSRange nodeToRange(TSNode node);
} // namespace BraneScript

#endif
