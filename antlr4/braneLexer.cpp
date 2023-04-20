
// Generated from /home/wirewhiz/Documents/git/BraneEngine/libraries/internal/BraneScript/antlr4/brane.g4 by ANTLR 4.12.0


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

::antlr4::internal::OnceFlag branelexerLexerOnceFlag;
BraneLexerStaticData *branelexerLexerStaticData = nullptr;

void branelexerLexerInitialize() {
  assert(branelexerLexerStaticData == nullptr);
  auto staticData = std::make_unique<BraneLexerStaticData>(
    std::vector<std::string>{
      "T__0", "T__1", "T__2", "T__3", "T__4", "T__5", "T__6", "T__7", "T__8", 
      "T__9", "T__10", "T__11", "T__12", "T__13", "T__14", "T__15", "T__16", 
      "T__17", "T__18", "T__19", "T__20", "T__21", "T__22", "T__23", "T__24", 
      "T__25", "T__26", "T__27", "T__28", "T__29", "T__30", "T__31", "T__32", 
      "T__33", "T__34", "T__35", "T__36", "T__37", "NEWLINE", "COMMENT", 
      "BLOCK_COMMENT", "SPACE", "BOOL", "INT", "FLOAT", "CHAR", "STRING", 
      "ID", "MUL", "DIV", "ADD", "SUB", "LOGIC"
    },
    std::vector<std::string>{
      "DEFAULT_TOKEN_CHANNEL", "HIDDEN"
    },
    std::vector<std::string>{
      "DEFAULT_MODE"
    },
    std::vector<std::string>{
      "", "';'", "'type'", "'expr'", "'...'", "'template'", "'<'", "','", 
      "'>'", "'::'", "'const'", "'ref'", "'constexpr'", "'opr'", "'=='", 
      "'!='", "'<='", "'>='", "'[]'", "'('", "')'", "'ext'", "'{'", "'}'", 
      "'link'", "'as'", "'export as'", "'packed'", "'struct'", "'='", "'return'", 
      "'if'", "'else'", "'while'", "'unroll'", "'sizeof'", "'.'", "'['", 
      "']'", "", "", "", "", "", "", "", "", "", "", "'*'", "'/'", "'+'", 
      "'-'"
    },
    std::vector<std::string>{
      "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
      "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
      "", "", "", "", "", "NEWLINE", "COMMENT", "BLOCK_COMMENT", "SPACE", 
      "BOOL", "INT", "FLOAT", "CHAR", "STRING", "ID", "MUL", "DIV", "ADD", 
      "SUB", "LOGIC"
    }
  );
  static const int32_t serializedATNSegment[] = {
  	4,0,53,367,6,-1,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,
  	6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,2,11,7,11,2,12,7,12,2,13,7,13,2,14,
  	7,14,2,15,7,15,2,16,7,16,2,17,7,17,2,18,7,18,2,19,7,19,2,20,7,20,2,21,
  	7,21,2,22,7,22,2,23,7,23,2,24,7,24,2,25,7,25,2,26,7,26,2,27,7,27,2,28,
  	7,28,2,29,7,29,2,30,7,30,2,31,7,31,2,32,7,32,2,33,7,33,2,34,7,34,2,35,
  	7,35,2,36,7,36,2,37,7,37,2,38,7,38,2,39,7,39,2,40,7,40,2,41,7,41,2,42,
  	7,42,2,43,7,43,2,44,7,44,2,45,7,45,2,46,7,46,2,47,7,47,2,48,7,48,2,49,
  	7,49,2,50,7,50,2,51,7,51,2,52,7,52,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,2,1,
  	2,1,2,1,2,1,2,1,3,1,3,1,3,1,3,1,4,1,4,1,4,1,4,1,4,1,4,1,4,1,4,1,4,1,5,
  	1,5,1,6,1,6,1,7,1,7,1,8,1,8,1,8,1,9,1,9,1,9,1,9,1,9,1,9,1,10,1,10,1,10,
  	1,10,1,11,1,11,1,11,1,11,1,11,1,11,1,11,1,11,1,11,1,11,1,12,1,12,1,12,
  	1,12,1,13,1,13,1,13,1,14,1,14,1,14,1,15,1,15,1,15,1,16,1,16,1,16,1,17,
  	1,17,1,17,1,18,1,18,1,19,1,19,1,20,1,20,1,20,1,20,1,21,1,21,1,22,1,22,
  	1,23,1,23,1,23,1,23,1,23,1,24,1,24,1,24,1,25,1,25,1,25,1,25,1,25,1,25,
  	1,25,1,25,1,25,1,25,1,26,1,26,1,26,1,26,1,26,1,26,1,26,1,27,1,27,1,27,
  	1,27,1,27,1,27,1,27,1,28,1,28,1,29,1,29,1,29,1,29,1,29,1,29,1,29,1,30,
  	1,30,1,30,1,31,1,31,1,31,1,31,1,31,1,32,1,32,1,32,1,32,1,32,1,32,1,33,
  	1,33,1,33,1,33,1,33,1,33,1,33,1,34,1,34,1,34,1,34,1,34,1,34,1,34,1,35,
  	1,35,1,36,1,36,1,37,1,37,1,38,4,38,269,8,38,11,38,12,38,270,1,38,1,38,
  	1,39,1,39,1,39,1,39,5,39,279,8,39,10,39,12,39,282,9,39,1,39,1,39,1,39,
  	1,39,1,40,1,40,1,40,1,40,5,40,292,8,40,10,40,12,40,295,9,40,1,40,1,40,
  	1,40,1,40,1,40,1,41,1,41,1,41,1,41,1,42,1,42,1,42,1,42,1,42,1,42,1,42,
  	1,42,1,42,3,42,315,8,42,1,43,4,43,318,8,43,11,43,12,43,319,1,44,1,44,
  	1,44,5,44,325,8,44,10,44,12,44,328,9,44,3,44,330,8,44,1,44,1,44,1,45,
  	1,45,1,45,1,45,1,46,1,46,5,46,340,8,46,10,46,12,46,343,9,46,1,46,1,46,
  	1,47,1,47,5,47,349,8,47,10,47,12,47,352,9,47,1,48,1,48,1,49,1,49,1,50,
  	1,50,1,51,1,51,1,52,1,52,1,52,1,52,3,52,366,8,52,3,280,293,341,0,53,1,
  	1,3,2,5,3,7,4,9,5,11,6,13,7,15,8,17,9,19,10,21,11,23,12,25,13,27,14,29,
  	15,31,16,33,17,35,18,37,19,39,20,41,21,43,22,45,23,47,24,49,25,51,26,
  	53,27,55,28,57,29,59,30,61,31,63,32,65,33,67,34,69,35,71,36,73,37,75,
  	38,77,39,79,40,81,41,83,42,85,43,87,44,89,45,91,46,93,47,95,48,97,49,
  	99,50,101,51,103,52,105,53,1,0,6,2,0,10,10,13,13,5,0,10,10,13,13,40,41,
  	69,70,79,79,2,0,9,9,32,32,1,0,48,57,3,0,65,90,95,95,97,122,4,0,48,57,
  	65,90,95,95,97,122,376,0,1,1,0,0,0,0,3,1,0,0,0,0,5,1,0,0,0,0,7,1,0,0,
  	0,0,9,1,0,0,0,0,11,1,0,0,0,0,13,1,0,0,0,0,15,1,0,0,0,0,17,1,0,0,0,0,19,
  	1,0,0,0,0,21,1,0,0,0,0,23,1,0,0,0,0,25,1,0,0,0,0,27,1,0,0,0,0,29,1,0,
  	0,0,0,31,1,0,0,0,0,33,1,0,0,0,0,35,1,0,0,0,0,37,1,0,0,0,0,39,1,0,0,0,
  	0,41,1,0,0,0,0,43,1,0,0,0,0,45,1,0,0,0,0,47,1,0,0,0,0,49,1,0,0,0,0,51,
  	1,0,0,0,0,53,1,0,0,0,0,55,1,0,0,0,0,57,1,0,0,0,0,59,1,0,0,0,0,61,1,0,
  	0,0,0,63,1,0,0,0,0,65,1,0,0,0,0,67,1,0,0,0,0,69,1,0,0,0,0,71,1,0,0,0,
  	0,73,1,0,0,0,0,75,1,0,0,0,0,77,1,0,0,0,0,79,1,0,0,0,0,81,1,0,0,0,0,83,
  	1,0,0,0,0,85,1,0,0,0,0,87,1,0,0,0,0,89,1,0,0,0,0,91,1,0,0,0,0,93,1,0,
  	0,0,0,95,1,0,0,0,0,97,1,0,0,0,0,99,1,0,0,0,0,101,1,0,0,0,0,103,1,0,0,
  	0,0,105,1,0,0,0,1,107,1,0,0,0,3,109,1,0,0,0,5,114,1,0,0,0,7,119,1,0,0,
  	0,9,123,1,0,0,0,11,132,1,0,0,0,13,134,1,0,0,0,15,136,1,0,0,0,17,138,1,
  	0,0,0,19,141,1,0,0,0,21,147,1,0,0,0,23,151,1,0,0,0,25,161,1,0,0,0,27,
  	165,1,0,0,0,29,168,1,0,0,0,31,171,1,0,0,0,33,174,1,0,0,0,35,177,1,0,0,
  	0,37,180,1,0,0,0,39,182,1,0,0,0,41,184,1,0,0,0,43,188,1,0,0,0,45,190,
  	1,0,0,0,47,192,1,0,0,0,49,197,1,0,0,0,51,200,1,0,0,0,53,210,1,0,0,0,55,
  	217,1,0,0,0,57,224,1,0,0,0,59,226,1,0,0,0,61,233,1,0,0,0,63,236,1,0,0,
  	0,65,241,1,0,0,0,67,247,1,0,0,0,69,254,1,0,0,0,71,261,1,0,0,0,73,263,
  	1,0,0,0,75,265,1,0,0,0,77,268,1,0,0,0,79,274,1,0,0,0,81,287,1,0,0,0,83,
  	301,1,0,0,0,85,314,1,0,0,0,87,317,1,0,0,0,89,321,1,0,0,0,91,333,1,0,0,
  	0,93,337,1,0,0,0,95,346,1,0,0,0,97,353,1,0,0,0,99,355,1,0,0,0,101,357,
  	1,0,0,0,103,359,1,0,0,0,105,365,1,0,0,0,107,108,5,59,0,0,108,2,1,0,0,
  	0,109,110,5,116,0,0,110,111,5,121,0,0,111,112,5,112,0,0,112,113,5,101,
  	0,0,113,4,1,0,0,0,114,115,5,101,0,0,115,116,5,120,0,0,116,117,5,112,0,
  	0,117,118,5,114,0,0,118,6,1,0,0,0,119,120,5,46,0,0,120,121,5,46,0,0,121,
  	122,5,46,0,0,122,8,1,0,0,0,123,124,5,116,0,0,124,125,5,101,0,0,125,126,
  	5,109,0,0,126,127,5,112,0,0,127,128,5,108,0,0,128,129,5,97,0,0,129,130,
  	5,116,0,0,130,131,5,101,0,0,131,10,1,0,0,0,132,133,5,60,0,0,133,12,1,
  	0,0,0,134,135,5,44,0,0,135,14,1,0,0,0,136,137,5,62,0,0,137,16,1,0,0,0,
  	138,139,5,58,0,0,139,140,5,58,0,0,140,18,1,0,0,0,141,142,5,99,0,0,142,
  	143,5,111,0,0,143,144,5,110,0,0,144,145,5,115,0,0,145,146,5,116,0,0,146,
  	20,1,0,0,0,147,148,5,114,0,0,148,149,5,101,0,0,149,150,5,102,0,0,150,
  	22,1,0,0,0,151,152,5,99,0,0,152,153,5,111,0,0,153,154,5,110,0,0,154,155,
  	5,115,0,0,155,156,5,116,0,0,156,157,5,101,0,0,157,158,5,120,0,0,158,159,
  	5,112,0,0,159,160,5,114,0,0,160,24,1,0,0,0,161,162,5,111,0,0,162,163,
  	5,112,0,0,163,164,5,114,0,0,164,26,1,0,0,0,165,166,5,61,0,0,166,167,5,
  	61,0,0,167,28,1,0,0,0,168,169,5,33,0,0,169,170,5,61,0,0,170,30,1,0,0,
  	0,171,172,5,60,0,0,172,173,5,61,0,0,173,32,1,0,0,0,174,175,5,62,0,0,175,
  	176,5,61,0,0,176,34,1,0,0,0,177,178,5,91,0,0,178,179,5,93,0,0,179,36,
  	1,0,0,0,180,181,5,40,0,0,181,38,1,0,0,0,182,183,5,41,0,0,183,40,1,0,0,
  	0,184,185,5,101,0,0,185,186,5,120,0,0,186,187,5,116,0,0,187,42,1,0,0,
  	0,188,189,5,123,0,0,189,44,1,0,0,0,190,191,5,125,0,0,191,46,1,0,0,0,192,
  	193,5,108,0,0,193,194,5,105,0,0,194,195,5,110,0,0,195,196,5,107,0,0,196,
  	48,1,0,0,0,197,198,5,97,0,0,198,199,5,115,0,0,199,50,1,0,0,0,200,201,
  	5,101,0,0,201,202,5,120,0,0,202,203,5,112,0,0,203,204,5,111,0,0,204,205,
  	5,114,0,0,205,206,5,116,0,0,206,207,5,32,0,0,207,208,5,97,0,0,208,209,
  	5,115,0,0,209,52,1,0,0,0,210,211,5,112,0,0,211,212,5,97,0,0,212,213,5,
  	99,0,0,213,214,5,107,0,0,214,215,5,101,0,0,215,216,5,100,0,0,216,54,1,
  	0,0,0,217,218,5,115,0,0,218,219,5,116,0,0,219,220,5,114,0,0,220,221,5,
  	117,0,0,221,222,5,99,0,0,222,223,5,116,0,0,223,56,1,0,0,0,224,225,5,61,
  	0,0,225,58,1,0,0,0,226,227,5,114,0,0,227,228,5,101,0,0,228,229,5,116,
  	0,0,229,230,5,117,0,0,230,231,5,114,0,0,231,232,5,110,0,0,232,60,1,0,
  	0,0,233,234,5,105,0,0,234,235,5,102,0,0,235,62,1,0,0,0,236,237,5,101,
  	0,0,237,238,5,108,0,0,238,239,5,115,0,0,239,240,5,101,0,0,240,64,1,0,
  	0,0,241,242,5,119,0,0,242,243,5,104,0,0,243,244,5,105,0,0,244,245,5,108,
  	0,0,245,246,5,101,0,0,246,66,1,0,0,0,247,248,5,117,0,0,248,249,5,110,
  	0,0,249,250,5,114,0,0,250,251,5,111,0,0,251,252,5,108,0,0,252,253,5,108,
  	0,0,253,68,1,0,0,0,254,255,5,115,0,0,255,256,5,105,0,0,256,257,5,122,
  	0,0,257,258,5,101,0,0,258,259,5,111,0,0,259,260,5,102,0,0,260,70,1,0,
  	0,0,261,262,5,46,0,0,262,72,1,0,0,0,263,264,5,91,0,0,264,74,1,0,0,0,265,
  	266,5,93,0,0,266,76,1,0,0,0,267,269,7,0,0,0,268,267,1,0,0,0,269,270,1,
  	0,0,0,270,268,1,0,0,0,270,271,1,0,0,0,271,272,1,0,0,0,272,273,6,38,0,
  	0,273,78,1,0,0,0,274,275,5,47,0,0,275,276,5,47,0,0,276,280,1,0,0,0,277,
  	279,9,0,0,0,278,277,1,0,0,0,279,282,1,0,0,0,280,281,1,0,0,0,280,278,1,
  	0,0,0,281,283,1,0,0,0,282,280,1,0,0,0,283,284,7,1,0,0,284,285,1,0,0,0,
  	285,286,6,39,0,0,286,80,1,0,0,0,287,288,5,47,0,0,288,289,5,42,0,0,289,
  	293,1,0,0,0,290,292,9,0,0,0,291,290,1,0,0,0,292,295,1,0,0,0,293,294,1,
  	0,0,0,293,291,1,0,0,0,294,296,1,0,0,0,295,293,1,0,0,0,296,297,5,42,0,
  	0,297,298,5,47,0,0,298,299,1,0,0,0,299,300,6,40,0,0,300,82,1,0,0,0,301,
  	302,7,2,0,0,302,303,1,0,0,0,303,304,6,41,0,0,304,84,1,0,0,0,305,306,5,
  	116,0,0,306,307,5,114,0,0,307,308,5,117,0,0,308,315,5,101,0,0,309,310,
  	5,102,0,0,310,311,5,97,0,0,311,312,5,108,0,0,312,313,5,115,0,0,313,315,
  	5,101,0,0,314,305,1,0,0,0,314,309,1,0,0,0,315,86,1,0,0,0,316,318,7,3,
  	0,0,317,316,1,0,0,0,318,319,1,0,0,0,319,317,1,0,0,0,319,320,1,0,0,0,320,
  	88,1,0,0,0,321,329,3,87,43,0,322,326,5,46,0,0,323,325,7,3,0,0,324,323,
  	1,0,0,0,325,328,1,0,0,0,326,324,1,0,0,0,326,327,1,0,0,0,327,330,1,0,0,
  	0,328,326,1,0,0,0,329,322,1,0,0,0,329,330,1,0,0,0,330,331,1,0,0,0,331,
  	332,5,102,0,0,332,90,1,0,0,0,333,334,5,39,0,0,334,335,9,0,0,0,335,336,
  	5,39,0,0,336,92,1,0,0,0,337,341,5,34,0,0,338,340,9,0,0,0,339,338,1,0,
  	0,0,340,343,1,0,0,0,341,342,1,0,0,0,341,339,1,0,0,0,342,344,1,0,0,0,343,
  	341,1,0,0,0,344,345,5,34,0,0,345,94,1,0,0,0,346,350,7,4,0,0,347,349,7,
  	5,0,0,348,347,1,0,0,0,349,352,1,0,0,0,350,348,1,0,0,0,350,351,1,0,0,0,
  	351,96,1,0,0,0,352,350,1,0,0,0,353,354,5,42,0,0,354,98,1,0,0,0,355,356,
  	5,47,0,0,356,100,1,0,0,0,357,358,5,43,0,0,358,102,1,0,0,0,359,360,5,45,
  	0,0,360,104,1,0,0,0,361,362,5,38,0,0,362,366,5,38,0,0,363,364,5,124,0,
  	0,364,366,5,124,0,0,365,361,1,0,0,0,365,363,1,0,0,0,366,106,1,0,0,0,11,
  	0,270,280,293,314,319,326,329,341,350,365,1,6,0,0
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
  ::antlr4::internal::call_once(branelexerLexerOnceFlag, branelexerLexerInitialize);
}
