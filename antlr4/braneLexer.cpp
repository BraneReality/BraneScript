
// Generated from C:/Users/eli/Documents/GitHub/BraneScript/antlr4\brane.g4 by ANTLR 4.10.1


#include "braneLexer.h"


using namespace antlr4;



using namespace antlr4;

namespace {

struct BraneLexerStaticData final {
  BraneLexerStaticData(std::vector<std::string> ruleNames,
                          std::vector<std::string> channelNames,
                          std::vector<std::string> modeNames,
                          std::vector<std::string> literalNames,
                          std::vector<std::string> symbolicNames)
      : ruleNames(std::move(ruleNames)), channelNames(std::move(channelNames)),
        modeNames(std::move(modeNames)), literalNames(std::move(literalNames)),
        symbolicNames(std::move(symbolicNames)),
        vocabulary(this->literalNames, this->symbolicNames) {}

  BraneLexerStaticData(const BraneLexerStaticData&) = delete;
  BraneLexerStaticData(BraneLexerStaticData&&) = delete;
  BraneLexerStaticData& operator=(const BraneLexerStaticData&) = delete;
  BraneLexerStaticData& operator=(BraneLexerStaticData&&) = delete;

  std::vector<antlr4::dfa::DFA> decisionToDFA;
  antlr4::atn::PredictionContextCache sharedContextCache;
  const std::vector<std::string> ruleNames;
  const std::vector<std::string> channelNames;
  const std::vector<std::string> modeNames;
  const std::vector<std::string> literalNames;
  const std::vector<std::string> symbolicNames;
  const antlr4::dfa::Vocabulary vocabulary;
  antlr4::atn::SerializedATNView serializedATN;
  std::unique_ptr<antlr4::atn::ATN> atn;
};

std::once_flag branelexerLexerOnceFlag;
BraneLexerStaticData *branelexerLexerStaticData = nullptr;

void branelexerLexerInitialize() {
  assert(branelexerLexerStaticData == nullptr);
  auto staticData = std::make_unique<BraneLexerStaticData>(
    std::vector<std::string>{
      "T__0", "T__1", "T__2", "T__3", "T__4", "T__5", "T__6", "T__7", "T__8", 
      "T__9", "T__10", "T__11", "T__12", "T__13", "T__14", "T__15", "T__16", 
      "T__17", "T__18", "T__19", "T__20", "T__21", "T__22", "T__23", "T__24", 
      "T__25", "NEWLINE", "COMMENT", "BLOCK_COMMENT", "SPACE", "BOOL", "INT", 
      "FLOAT", "STRING", "ID", "MUL", "DIV", "ADD", "SUB"
    },
    std::vector<std::string>{
      "DEFAULT_TOKEN_CHANNEL", "HIDDEN"
    },
    std::vector<std::string>{
      "DEFAULT_MODE"
    },
    std::vector<std::string>{
      "", "'const'", "'ref'", "','", "'('", "')'", "'{'", "'}'", "'#include'", 
      "'link'", "'as'", "';'", "'public'", "'packed'", "'struct'", "'return'", 
      "'if'", "'while'", "'delete'", "'.'", "'=='", "'!='", "'<'", "'>'", 
      "'<='", "'>='", "'='", "", "", "", "", "", "", "", "", "", "'*'", 
      "'/'", "'+'", "'-'"
    },
    std::vector<std::string>{
      "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
      "", "", "", "", "", "", "", "", "", "", "NEWLINE", "COMMENT", "BLOCK_COMMENT", 
      "SPACE", "BOOL", "INT", "FLOAT", "STRING", "ID", "MUL", "DIV", "ADD", 
      "SUB"
    }
  );
  static const int32_t serializedATNSegment[] = {
  	4,0,39,272,6,-1,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,
  	6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,2,11,7,11,2,12,7,12,2,13,7,13,2,14,
  	7,14,2,15,7,15,2,16,7,16,2,17,7,17,2,18,7,18,2,19,7,19,2,20,7,20,2,21,
  	7,21,2,22,7,22,2,23,7,23,2,24,7,24,2,25,7,25,2,26,7,26,2,27,7,27,2,28,
  	7,28,2,29,7,29,2,30,7,30,2,31,7,31,2,32,7,32,2,33,7,33,2,34,7,34,2,35,
  	7,35,2,36,7,36,2,37,7,37,2,38,7,38,1,0,1,0,1,0,1,0,1,0,1,0,1,1,1,1,1,
  	1,1,1,1,2,1,2,1,3,1,3,1,4,1,4,1,5,1,5,1,6,1,6,1,7,1,7,1,7,1,7,1,7,1,7,
  	1,7,1,7,1,7,1,8,1,8,1,8,1,8,1,8,1,9,1,9,1,9,1,10,1,10,1,11,1,11,1,11,
  	1,11,1,11,1,11,1,11,1,12,1,12,1,12,1,12,1,12,1,12,1,12,1,13,1,13,1,13,
  	1,13,1,13,1,13,1,13,1,14,1,14,1,14,1,14,1,14,1,14,1,14,1,15,1,15,1,15,
  	1,16,1,16,1,16,1,16,1,16,1,16,1,17,1,17,1,17,1,17,1,17,1,17,1,17,1,18,
  	1,18,1,19,1,19,1,19,1,20,1,20,1,20,1,21,1,21,1,22,1,22,1,23,1,23,1,23,
  	1,24,1,24,1,24,1,25,1,25,1,26,4,26,184,8,26,11,26,12,26,185,1,26,1,26,
  	1,27,1,27,1,27,1,27,5,27,194,8,27,10,27,12,27,197,9,27,1,27,1,27,1,27,
  	1,27,1,28,1,28,1,28,1,28,5,28,207,8,28,10,28,12,28,210,9,28,1,28,1,28,
  	1,28,1,28,1,28,1,29,1,29,1,29,1,29,1,30,1,30,1,30,1,30,1,30,1,30,1,30,
  	1,30,1,30,3,30,230,8,30,1,31,4,31,233,8,31,11,31,12,31,234,1,32,1,32,
  	1,32,5,32,240,8,32,10,32,12,32,243,9,32,3,32,245,8,32,1,32,1,32,1,33,
  	1,33,5,33,251,8,33,10,33,12,33,254,9,33,1,33,1,33,1,34,1,34,5,34,260,
  	8,34,10,34,12,34,263,9,34,1,35,1,35,1,36,1,36,1,37,1,37,1,38,1,38,3,195,
  	208,252,0,39,1,1,3,2,5,3,7,4,9,5,11,6,13,7,15,8,17,9,19,10,21,11,23,12,
  	25,13,27,14,29,15,31,16,33,17,35,18,37,19,39,20,41,21,43,22,45,23,47,
  	24,49,25,51,26,53,27,55,28,57,29,59,30,61,31,63,32,65,33,67,34,69,35,
  	71,36,73,37,75,38,77,39,1,0,6,2,0,10,10,13,13,5,0,10,10,13,13,40,41,69,
  	70,79,79,2,0,9,9,32,32,1,0,48,57,3,0,65,90,95,95,97,122,4,0,48,57,65,
  	90,95,95,97,122,280,0,1,1,0,0,0,0,3,1,0,0,0,0,5,1,0,0,0,0,7,1,0,0,0,0,
  	9,1,0,0,0,0,11,1,0,0,0,0,13,1,0,0,0,0,15,1,0,0,0,0,17,1,0,0,0,0,19,1,
  	0,0,0,0,21,1,0,0,0,0,23,1,0,0,0,0,25,1,0,0,0,0,27,1,0,0,0,0,29,1,0,0,
  	0,0,31,1,0,0,0,0,33,1,0,0,0,0,35,1,0,0,0,0,37,1,0,0,0,0,39,1,0,0,0,0,
  	41,1,0,0,0,0,43,1,0,0,0,0,45,1,0,0,0,0,47,1,0,0,0,0,49,1,0,0,0,0,51,1,
  	0,0,0,0,53,1,0,0,0,0,55,1,0,0,0,0,57,1,0,0,0,0,59,1,0,0,0,0,61,1,0,0,
  	0,0,63,1,0,0,0,0,65,1,0,0,0,0,67,1,0,0,0,0,69,1,0,0,0,0,71,1,0,0,0,0,
  	73,1,0,0,0,0,75,1,0,0,0,0,77,1,0,0,0,1,79,1,0,0,0,3,85,1,0,0,0,5,89,1,
  	0,0,0,7,91,1,0,0,0,9,93,1,0,0,0,11,95,1,0,0,0,13,97,1,0,0,0,15,99,1,0,
  	0,0,17,108,1,0,0,0,19,113,1,0,0,0,21,116,1,0,0,0,23,118,1,0,0,0,25,125,
  	1,0,0,0,27,132,1,0,0,0,29,139,1,0,0,0,31,146,1,0,0,0,33,149,1,0,0,0,35,
  	155,1,0,0,0,37,162,1,0,0,0,39,164,1,0,0,0,41,167,1,0,0,0,43,170,1,0,0,
  	0,45,172,1,0,0,0,47,174,1,0,0,0,49,177,1,0,0,0,51,180,1,0,0,0,53,183,
  	1,0,0,0,55,189,1,0,0,0,57,202,1,0,0,0,59,216,1,0,0,0,61,229,1,0,0,0,63,
  	232,1,0,0,0,65,236,1,0,0,0,67,248,1,0,0,0,69,257,1,0,0,0,71,264,1,0,0,
  	0,73,266,1,0,0,0,75,268,1,0,0,0,77,270,1,0,0,0,79,80,5,99,0,0,80,81,5,
  	111,0,0,81,82,5,110,0,0,82,83,5,115,0,0,83,84,5,116,0,0,84,2,1,0,0,0,
  	85,86,5,114,0,0,86,87,5,101,0,0,87,88,5,102,0,0,88,4,1,0,0,0,89,90,5,
  	44,0,0,90,6,1,0,0,0,91,92,5,40,0,0,92,8,1,0,0,0,93,94,5,41,0,0,94,10,
  	1,0,0,0,95,96,5,123,0,0,96,12,1,0,0,0,97,98,5,125,0,0,98,14,1,0,0,0,99,
  	100,5,35,0,0,100,101,5,105,0,0,101,102,5,110,0,0,102,103,5,99,0,0,103,
  	104,5,108,0,0,104,105,5,117,0,0,105,106,5,100,0,0,106,107,5,101,0,0,107,
  	16,1,0,0,0,108,109,5,108,0,0,109,110,5,105,0,0,110,111,5,110,0,0,111,
  	112,5,107,0,0,112,18,1,0,0,0,113,114,5,97,0,0,114,115,5,115,0,0,115,20,
  	1,0,0,0,116,117,5,59,0,0,117,22,1,0,0,0,118,119,5,112,0,0,119,120,5,117,
  	0,0,120,121,5,98,0,0,121,122,5,108,0,0,122,123,5,105,0,0,123,124,5,99,
  	0,0,124,24,1,0,0,0,125,126,5,112,0,0,126,127,5,97,0,0,127,128,5,99,0,
  	0,128,129,5,107,0,0,129,130,5,101,0,0,130,131,5,100,0,0,131,26,1,0,0,
  	0,132,133,5,115,0,0,133,134,5,116,0,0,134,135,5,114,0,0,135,136,5,117,
  	0,0,136,137,5,99,0,0,137,138,5,116,0,0,138,28,1,0,0,0,139,140,5,114,0,
  	0,140,141,5,101,0,0,141,142,5,116,0,0,142,143,5,117,0,0,143,144,5,114,
  	0,0,144,145,5,110,0,0,145,30,1,0,0,0,146,147,5,105,0,0,147,148,5,102,
  	0,0,148,32,1,0,0,0,149,150,5,119,0,0,150,151,5,104,0,0,151,152,5,105,
  	0,0,152,153,5,108,0,0,153,154,5,101,0,0,154,34,1,0,0,0,155,156,5,100,
  	0,0,156,157,5,101,0,0,157,158,5,108,0,0,158,159,5,101,0,0,159,160,5,116,
  	0,0,160,161,5,101,0,0,161,36,1,0,0,0,162,163,5,46,0,0,163,38,1,0,0,0,
  	164,165,5,61,0,0,165,166,5,61,0,0,166,40,1,0,0,0,167,168,5,33,0,0,168,
  	169,5,61,0,0,169,42,1,0,0,0,170,171,5,60,0,0,171,44,1,0,0,0,172,173,5,
  	62,0,0,173,46,1,0,0,0,174,175,5,60,0,0,175,176,5,61,0,0,176,48,1,0,0,
  	0,177,178,5,62,0,0,178,179,5,61,0,0,179,50,1,0,0,0,180,181,5,61,0,0,181,
  	52,1,0,0,0,182,184,7,0,0,0,183,182,1,0,0,0,184,185,1,0,0,0,185,183,1,
  	0,0,0,185,186,1,0,0,0,186,187,1,0,0,0,187,188,6,26,0,0,188,54,1,0,0,0,
  	189,190,5,47,0,0,190,191,5,47,0,0,191,195,1,0,0,0,192,194,9,0,0,0,193,
  	192,1,0,0,0,194,197,1,0,0,0,195,196,1,0,0,0,195,193,1,0,0,0,196,198,1,
  	0,0,0,197,195,1,0,0,0,198,199,7,1,0,0,199,200,1,0,0,0,200,201,6,27,0,
  	0,201,56,1,0,0,0,202,203,5,47,0,0,203,204,5,42,0,0,204,208,1,0,0,0,205,
  	207,9,0,0,0,206,205,1,0,0,0,207,210,1,0,0,0,208,209,1,0,0,0,208,206,1,
  	0,0,0,209,211,1,0,0,0,210,208,1,0,0,0,211,212,5,42,0,0,212,213,5,47,0,
  	0,213,214,1,0,0,0,214,215,6,28,0,0,215,58,1,0,0,0,216,217,7,2,0,0,217,
  	218,1,0,0,0,218,219,6,29,0,0,219,60,1,0,0,0,220,221,5,116,0,0,221,222,
  	5,114,0,0,222,223,5,117,0,0,223,230,5,101,0,0,224,225,5,102,0,0,225,226,
  	5,97,0,0,226,227,5,108,0,0,227,228,5,115,0,0,228,230,5,101,0,0,229,220,
  	1,0,0,0,229,224,1,0,0,0,230,62,1,0,0,0,231,233,7,3,0,0,232,231,1,0,0,
  	0,233,234,1,0,0,0,234,232,1,0,0,0,234,235,1,0,0,0,235,64,1,0,0,0,236,
  	244,3,63,31,0,237,241,5,46,0,0,238,240,7,3,0,0,239,238,1,0,0,0,240,243,
  	1,0,0,0,241,239,1,0,0,0,241,242,1,0,0,0,242,245,1,0,0,0,243,241,1,0,0,
  	0,244,237,1,0,0,0,244,245,1,0,0,0,245,246,1,0,0,0,246,247,5,102,0,0,247,
  	66,1,0,0,0,248,252,5,34,0,0,249,251,9,0,0,0,250,249,1,0,0,0,251,254,1,
  	0,0,0,252,253,1,0,0,0,252,250,1,0,0,0,253,255,1,0,0,0,254,252,1,0,0,0,
  	255,256,5,34,0,0,256,68,1,0,0,0,257,261,7,4,0,0,258,260,7,5,0,0,259,258,
  	1,0,0,0,260,263,1,0,0,0,261,259,1,0,0,0,261,262,1,0,0,0,262,70,1,0,0,
  	0,263,261,1,0,0,0,264,265,5,42,0,0,265,72,1,0,0,0,266,267,5,47,0,0,267,
  	74,1,0,0,0,268,269,5,43,0,0,269,76,1,0,0,0,270,271,5,45,0,0,271,78,1,
  	0,0,0,10,0,185,195,208,229,234,241,244,252,261,1,6,0,0
  };
  staticData->serializedATN = antlr4::atn::SerializedATNView(serializedATNSegment, sizeof(serializedATNSegment) / sizeof(serializedATNSegment[0]));

  antlr4::atn::ATNDeserializer deserializer;
  staticData->atn = deserializer.deserialize(staticData->serializedATN);

  const size_t count = staticData->atn->getNumberOfDecisions();
  staticData->decisionToDFA.reserve(count);
  for (size_t i = 0; i < count; i++) { 
    staticData->decisionToDFA.emplace_back(staticData->atn->getDecisionState(i), i);
  }
  branelexerLexerStaticData = staticData.release();
}

}

braneLexer::braneLexer(CharStream *input) : Lexer(input) {
  braneLexer::initialize();
  _interpreter = new atn::LexerATNSimulator(this, *branelexerLexerStaticData->atn, branelexerLexerStaticData->decisionToDFA, branelexerLexerStaticData->sharedContextCache);
}

braneLexer::~braneLexer() {
  delete _interpreter;
}

std::string braneLexer::getGrammarFileName() const {
  return "brane.g4";
}

const std::vector<std::string>& braneLexer::getRuleNames() const {
  return branelexerLexerStaticData->ruleNames;
}

const std::vector<std::string>& braneLexer::getChannelNames() const {
  return branelexerLexerStaticData->channelNames;
}

const std::vector<std::string>& braneLexer::getModeNames() const {
  return branelexerLexerStaticData->modeNames;
}

const dfa::Vocabulary& braneLexer::getVocabulary() const {
  return branelexerLexerStaticData->vocabulary;
}

antlr4::atn::SerializedATNView braneLexer::getSerializedATN() const {
  return branelexerLexerStaticData->serializedATN;
}

const atn::ATN& braneLexer::getATN() const {
  return *branelexerLexerStaticData->atn;
}




void braneLexer::initialize() {
  std::call_once(branelexerLexerOnceFlag, branelexerLexerInitialize);
}
