
// Generated from /home/wirewhiz/Documents/git/BraneEngine/libraries/BraneScript/antlr4/brane.g4 by ANTLR 4.12.0


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
      "T__33", "T__34", "T__35", "T__36", "T__37", "T__38", "T__39", "T__40", 
      "T__41", "COMMENT", "NEWLINE", "BLOCK_COMMENT", "SPACE", "BOOL", "INT", 
      "FLOAT", "CHAR", "STRING", "ID", "MUL", "DIV", "ADD", "SUB", "LOGIC"
    },
    std::vector<std::string>{
      "DEFAULT_TOKEN_CHANNEL", "HIDDEN"
    },
    std::vector<std::string>{
      "DEFAULT_MODE"
    },
    std::vector<std::string>{
      "", "'public'", "'link'", "'as'", "'['", "','", "']'", "'module'", 
      "'{'", "'}'", "';'", "'typedef'", "'...'", "'template'", "'<'", "'>'", 
      "'Lambda'", "'::'", "'const'", "'ref'", "'('", "')'", "'constexpr'", 
      "'opr'", "'=='", "'!='", "'<='", "'>='", "'!'", "'ext'", "'packed'", 
      "'struct'", "'return'", "'if'", "'else'", "'while'", "'for'", "'sizeof'", 
      "'.'", "'++'", "'--'", "'='", "'<-'", "", "", "", "", "", "", "", 
      "", "", "", "'*'", "'/'", "'+'", "'-'"
    },
    std::vector<std::string>{
      "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
      "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
      "", "", "", "", "", "", "", "", "", "COMMENT", "NEWLINE", "BLOCK_COMMENT", 
      "SPACE", "BOOL", "INT", "FLOAT", "CHAR", "STRING", "ID", "MUL", "DIV", 
      "ADD", "SUB", "LOGIC"
    }
  );
  static const int32_t serializedATNSegment[] = {
  	4,0,57,389,6,-1,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,2,4,7,4,2,5,7,5,2,6,7,
  	6,2,7,7,7,2,8,7,8,2,9,7,9,2,10,7,10,2,11,7,11,2,12,7,12,2,13,7,13,2,14,
  	7,14,2,15,7,15,2,16,7,16,2,17,7,17,2,18,7,18,2,19,7,19,2,20,7,20,2,21,
  	7,21,2,22,7,22,2,23,7,23,2,24,7,24,2,25,7,25,2,26,7,26,2,27,7,27,2,28,
  	7,28,2,29,7,29,2,30,7,30,2,31,7,31,2,32,7,32,2,33,7,33,2,34,7,34,2,35,
  	7,35,2,36,7,36,2,37,7,37,2,38,7,38,2,39,7,39,2,40,7,40,2,41,7,41,2,42,
  	7,42,2,43,7,43,2,44,7,44,2,45,7,45,2,46,7,46,2,47,7,47,2,48,7,48,2,49,
  	7,49,2,50,7,50,2,51,7,51,2,52,7,52,2,53,7,53,2,54,7,54,2,55,7,55,2,56,
  	7,56,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,2,1,2,1,2,1,3,
  	1,3,1,4,1,4,1,5,1,5,1,6,1,6,1,6,1,6,1,6,1,6,1,6,1,7,1,7,1,8,1,8,1,9,1,
  	9,1,10,1,10,1,10,1,10,1,10,1,10,1,10,1,10,1,11,1,11,1,11,1,11,1,12,1,
  	12,1,12,1,12,1,12,1,12,1,12,1,12,1,12,1,13,1,13,1,14,1,14,1,15,1,15,1,
  	15,1,15,1,15,1,15,1,15,1,16,1,16,1,16,1,17,1,17,1,17,1,17,1,17,1,17,1,
  	18,1,18,1,18,1,18,1,19,1,19,1,20,1,20,1,21,1,21,1,21,1,21,1,21,1,21,1,
  	21,1,21,1,21,1,21,1,22,1,22,1,22,1,22,1,23,1,23,1,23,1,24,1,24,1,24,1,
  	25,1,25,1,25,1,26,1,26,1,26,1,27,1,27,1,28,1,28,1,28,1,28,1,29,1,29,1,
  	29,1,29,1,29,1,29,1,29,1,30,1,30,1,30,1,30,1,30,1,30,1,30,1,31,1,31,1,
  	31,1,31,1,31,1,31,1,31,1,32,1,32,1,32,1,33,1,33,1,33,1,33,1,33,1,34,1,
  	34,1,34,1,34,1,34,1,34,1,35,1,35,1,35,1,35,1,36,1,36,1,36,1,36,1,36,1,
  	36,1,36,1,37,1,37,1,38,1,38,1,38,1,39,1,39,1,39,1,40,1,40,1,41,1,41,1,
  	41,1,42,1,42,1,42,1,42,5,42,294,8,42,10,42,12,42,297,9,42,1,42,1,42,1,
  	42,1,42,1,43,4,43,304,8,43,11,43,12,43,305,1,43,1,43,1,44,1,44,1,44,1,
  	44,5,44,314,8,44,10,44,12,44,317,9,44,1,44,1,44,1,44,1,44,1,44,1,45,1,
  	45,1,45,1,45,1,46,1,46,1,46,1,46,1,46,1,46,1,46,1,46,1,46,3,46,337,8,
  	46,1,47,4,47,340,8,47,11,47,12,47,341,1,48,1,48,1,48,5,48,347,8,48,10,
  	48,12,48,350,9,48,3,48,352,8,48,1,48,1,48,1,49,1,49,1,49,1,49,1,50,1,
  	50,5,50,362,8,50,10,50,12,50,365,9,50,1,50,1,50,1,51,1,51,5,51,371,8,
  	51,10,51,12,51,374,9,51,1,52,1,52,1,53,1,53,1,54,1,54,1,55,1,55,1,56,
  	1,56,1,56,1,56,3,56,388,8,56,3,295,315,363,0,57,1,1,3,2,5,3,7,4,9,5,11,
  	6,13,7,15,8,17,9,19,10,21,11,23,12,25,13,27,14,29,15,31,16,33,17,35,18,
  	37,19,39,20,41,21,43,22,45,23,47,24,49,25,51,26,53,27,55,28,57,29,59,
  	30,61,31,63,32,65,33,67,34,69,35,71,36,73,37,75,38,77,39,79,40,81,41,
  	83,42,85,43,87,44,89,45,91,46,93,47,95,48,97,49,99,50,101,51,103,52,105,
  	53,107,54,109,55,111,56,113,57,1,0,5,2,0,10,10,13,13,2,0,9,9,32,32,1,
  	0,48,57,3,0,65,90,95,95,97,122,4,0,48,57,65,90,95,95,97,122,398,0,1,1,
  	0,0,0,0,3,1,0,0,0,0,5,1,0,0,0,0,7,1,0,0,0,0,9,1,0,0,0,0,11,1,0,0,0,0,
  	13,1,0,0,0,0,15,1,0,0,0,0,17,1,0,0,0,0,19,1,0,0,0,0,21,1,0,0,0,0,23,1,
  	0,0,0,0,25,1,0,0,0,0,27,1,0,0,0,0,29,1,0,0,0,0,31,1,0,0,0,0,33,1,0,0,
  	0,0,35,1,0,0,0,0,37,1,0,0,0,0,39,1,0,0,0,0,41,1,0,0,0,0,43,1,0,0,0,0,
  	45,1,0,0,0,0,47,1,0,0,0,0,49,1,0,0,0,0,51,1,0,0,0,0,53,1,0,0,0,0,55,1,
  	0,0,0,0,57,1,0,0,0,0,59,1,0,0,0,0,61,1,0,0,0,0,63,1,0,0,0,0,65,1,0,0,
  	0,0,67,1,0,0,0,0,69,1,0,0,0,0,71,1,0,0,0,0,73,1,0,0,0,0,75,1,0,0,0,0,
  	77,1,0,0,0,0,79,1,0,0,0,0,81,1,0,0,0,0,83,1,0,0,0,0,85,1,0,0,0,0,87,1,
  	0,0,0,0,89,1,0,0,0,0,91,1,0,0,0,0,93,1,0,0,0,0,95,1,0,0,0,0,97,1,0,0,
  	0,0,99,1,0,0,0,0,101,1,0,0,0,0,103,1,0,0,0,0,105,1,0,0,0,0,107,1,0,0,
  	0,0,109,1,0,0,0,0,111,1,0,0,0,0,113,1,0,0,0,1,115,1,0,0,0,3,122,1,0,0,
  	0,5,127,1,0,0,0,7,130,1,0,0,0,9,132,1,0,0,0,11,134,1,0,0,0,13,136,1,0,
  	0,0,15,143,1,0,0,0,17,145,1,0,0,0,19,147,1,0,0,0,21,149,1,0,0,0,23,157,
  	1,0,0,0,25,161,1,0,0,0,27,170,1,0,0,0,29,172,1,0,0,0,31,174,1,0,0,0,33,
  	181,1,0,0,0,35,184,1,0,0,0,37,190,1,0,0,0,39,194,1,0,0,0,41,196,1,0,0,
  	0,43,198,1,0,0,0,45,208,1,0,0,0,47,212,1,0,0,0,49,215,1,0,0,0,51,218,
  	1,0,0,0,53,221,1,0,0,0,55,224,1,0,0,0,57,226,1,0,0,0,59,230,1,0,0,0,61,
  	237,1,0,0,0,63,244,1,0,0,0,65,251,1,0,0,0,67,254,1,0,0,0,69,259,1,0,0,
  	0,71,265,1,0,0,0,73,269,1,0,0,0,75,276,1,0,0,0,77,278,1,0,0,0,79,281,
  	1,0,0,0,81,284,1,0,0,0,83,286,1,0,0,0,85,289,1,0,0,0,87,303,1,0,0,0,89,
  	309,1,0,0,0,91,323,1,0,0,0,93,336,1,0,0,0,95,339,1,0,0,0,97,343,1,0,0,
  	0,99,355,1,0,0,0,101,359,1,0,0,0,103,368,1,0,0,0,105,375,1,0,0,0,107,
  	377,1,0,0,0,109,379,1,0,0,0,111,381,1,0,0,0,113,387,1,0,0,0,115,116,5,
  	112,0,0,116,117,5,117,0,0,117,118,5,98,0,0,118,119,5,108,0,0,119,120,
  	5,105,0,0,120,121,5,99,0,0,121,2,1,0,0,0,122,123,5,108,0,0,123,124,5,
  	105,0,0,124,125,5,110,0,0,125,126,5,107,0,0,126,4,1,0,0,0,127,128,5,97,
  	0,0,128,129,5,115,0,0,129,6,1,0,0,0,130,131,5,91,0,0,131,8,1,0,0,0,132,
  	133,5,44,0,0,133,10,1,0,0,0,134,135,5,93,0,0,135,12,1,0,0,0,136,137,5,
  	109,0,0,137,138,5,111,0,0,138,139,5,100,0,0,139,140,5,117,0,0,140,141,
  	5,108,0,0,141,142,5,101,0,0,142,14,1,0,0,0,143,144,5,123,0,0,144,16,1,
  	0,0,0,145,146,5,125,0,0,146,18,1,0,0,0,147,148,5,59,0,0,148,20,1,0,0,
  	0,149,150,5,116,0,0,150,151,5,121,0,0,151,152,5,112,0,0,152,153,5,101,
  	0,0,153,154,5,100,0,0,154,155,5,101,0,0,155,156,5,102,0,0,156,22,1,0,
  	0,0,157,158,5,46,0,0,158,159,5,46,0,0,159,160,5,46,0,0,160,24,1,0,0,0,
  	161,162,5,116,0,0,162,163,5,101,0,0,163,164,5,109,0,0,164,165,5,112,0,
  	0,165,166,5,108,0,0,166,167,5,97,0,0,167,168,5,116,0,0,168,169,5,101,
  	0,0,169,26,1,0,0,0,170,171,5,60,0,0,171,28,1,0,0,0,172,173,5,62,0,0,173,
  	30,1,0,0,0,174,175,5,76,0,0,175,176,5,97,0,0,176,177,5,109,0,0,177,178,
  	5,98,0,0,178,179,5,100,0,0,179,180,5,97,0,0,180,32,1,0,0,0,181,182,5,
  	58,0,0,182,183,5,58,0,0,183,34,1,0,0,0,184,185,5,99,0,0,185,186,5,111,
  	0,0,186,187,5,110,0,0,187,188,5,115,0,0,188,189,5,116,0,0,189,36,1,0,
  	0,0,190,191,5,114,0,0,191,192,5,101,0,0,192,193,5,102,0,0,193,38,1,0,
  	0,0,194,195,5,40,0,0,195,40,1,0,0,0,196,197,5,41,0,0,197,42,1,0,0,0,198,
  	199,5,99,0,0,199,200,5,111,0,0,200,201,5,110,0,0,201,202,5,115,0,0,202,
  	203,5,116,0,0,203,204,5,101,0,0,204,205,5,120,0,0,205,206,5,112,0,0,206,
  	207,5,114,0,0,207,44,1,0,0,0,208,209,5,111,0,0,209,210,5,112,0,0,210,
  	211,5,114,0,0,211,46,1,0,0,0,212,213,5,61,0,0,213,214,5,61,0,0,214,48,
  	1,0,0,0,215,216,5,33,0,0,216,217,5,61,0,0,217,50,1,0,0,0,218,219,5,60,
  	0,0,219,220,5,61,0,0,220,52,1,0,0,0,221,222,5,62,0,0,222,223,5,61,0,0,
  	223,54,1,0,0,0,224,225,5,33,0,0,225,56,1,0,0,0,226,227,5,101,0,0,227,
  	228,5,120,0,0,228,229,5,116,0,0,229,58,1,0,0,0,230,231,5,112,0,0,231,
  	232,5,97,0,0,232,233,5,99,0,0,233,234,5,107,0,0,234,235,5,101,0,0,235,
  	236,5,100,0,0,236,60,1,0,0,0,237,238,5,115,0,0,238,239,5,116,0,0,239,
  	240,5,114,0,0,240,241,5,117,0,0,241,242,5,99,0,0,242,243,5,116,0,0,243,
  	62,1,0,0,0,244,245,5,114,0,0,245,246,5,101,0,0,246,247,5,116,0,0,247,
  	248,5,117,0,0,248,249,5,114,0,0,249,250,5,110,0,0,250,64,1,0,0,0,251,
  	252,5,105,0,0,252,253,5,102,0,0,253,66,1,0,0,0,254,255,5,101,0,0,255,
  	256,5,108,0,0,256,257,5,115,0,0,257,258,5,101,0,0,258,68,1,0,0,0,259,
  	260,5,119,0,0,260,261,5,104,0,0,261,262,5,105,0,0,262,263,5,108,0,0,263,
  	264,5,101,0,0,264,70,1,0,0,0,265,266,5,102,0,0,266,267,5,111,0,0,267,
  	268,5,114,0,0,268,72,1,0,0,0,269,270,5,115,0,0,270,271,5,105,0,0,271,
  	272,5,122,0,0,272,273,5,101,0,0,273,274,5,111,0,0,274,275,5,102,0,0,275,
  	74,1,0,0,0,276,277,5,46,0,0,277,76,1,0,0,0,278,279,5,43,0,0,279,280,5,
  	43,0,0,280,78,1,0,0,0,281,282,5,45,0,0,282,283,5,45,0,0,283,80,1,0,0,
  	0,284,285,5,61,0,0,285,82,1,0,0,0,286,287,5,60,0,0,287,288,5,45,0,0,288,
  	84,1,0,0,0,289,290,5,47,0,0,290,291,5,47,0,0,291,295,1,0,0,0,292,294,
  	9,0,0,0,293,292,1,0,0,0,294,297,1,0,0,0,295,296,1,0,0,0,295,293,1,0,0,
  	0,296,298,1,0,0,0,297,295,1,0,0,0,298,299,7,0,0,0,299,300,1,0,0,0,300,
  	301,6,42,0,0,301,86,1,0,0,0,302,304,7,0,0,0,303,302,1,0,0,0,304,305,1,
  	0,0,0,305,303,1,0,0,0,305,306,1,0,0,0,306,307,1,0,0,0,307,308,6,43,0,
  	0,308,88,1,0,0,0,309,310,5,47,0,0,310,311,5,42,0,0,311,315,1,0,0,0,312,
  	314,9,0,0,0,313,312,1,0,0,0,314,317,1,0,0,0,315,316,1,0,0,0,315,313,1,
  	0,0,0,316,318,1,0,0,0,317,315,1,0,0,0,318,319,5,42,0,0,319,320,5,47,0,
  	0,320,321,1,0,0,0,321,322,6,44,0,0,322,90,1,0,0,0,323,324,7,1,0,0,324,
  	325,1,0,0,0,325,326,6,45,0,0,326,92,1,0,0,0,327,328,5,116,0,0,328,329,
  	5,114,0,0,329,330,5,117,0,0,330,337,5,101,0,0,331,332,5,102,0,0,332,333,
  	5,97,0,0,333,334,5,108,0,0,334,335,5,115,0,0,335,337,5,101,0,0,336,327,
  	1,0,0,0,336,331,1,0,0,0,337,94,1,0,0,0,338,340,7,2,0,0,339,338,1,0,0,
  	0,340,341,1,0,0,0,341,339,1,0,0,0,341,342,1,0,0,0,342,96,1,0,0,0,343,
  	351,3,95,47,0,344,348,5,46,0,0,345,347,7,2,0,0,346,345,1,0,0,0,347,350,
  	1,0,0,0,348,346,1,0,0,0,348,349,1,0,0,0,349,352,1,0,0,0,350,348,1,0,0,
  	0,351,344,1,0,0,0,351,352,1,0,0,0,352,353,1,0,0,0,353,354,5,102,0,0,354,
  	98,1,0,0,0,355,356,5,39,0,0,356,357,9,0,0,0,357,358,5,39,0,0,358,100,
  	1,0,0,0,359,363,5,34,0,0,360,362,9,0,0,0,361,360,1,0,0,0,362,365,1,0,
  	0,0,363,364,1,0,0,0,363,361,1,0,0,0,364,366,1,0,0,0,365,363,1,0,0,0,366,
  	367,5,34,0,0,367,102,1,0,0,0,368,372,7,3,0,0,369,371,7,4,0,0,370,369,
  	1,0,0,0,371,374,1,0,0,0,372,370,1,0,0,0,372,373,1,0,0,0,373,104,1,0,0,
  	0,374,372,1,0,0,0,375,376,5,42,0,0,376,106,1,0,0,0,377,378,5,47,0,0,378,
  	108,1,0,0,0,379,380,5,43,0,0,380,110,1,0,0,0,381,382,5,45,0,0,382,112,
  	1,0,0,0,383,384,5,38,0,0,384,388,5,38,0,0,385,386,5,124,0,0,386,388,5,
  	124,0,0,387,383,1,0,0,0,387,385,1,0,0,0,388,114,1,0,0,0,11,0,295,305,
  	315,336,341,348,351,363,372,387,1,6,0,0
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
