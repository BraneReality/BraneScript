grammar brane;

NEWLINE : [\r\n]+ -> skip;
COMMENT : '//'(.*?)[\r\n(EOF)] -> skip;
BLOCK_COMMENT : '/*'.*?'*/' -> skip;
SPACE   : (' '|'\t') -> skip;

INT     : [0-9]+;
FLOAT   : INT('.'([0-9]*))?'f';
STRING  : '"'.*?'"';

ID      : [a-zA-Z_]([a-zA-Z0-9_]*);

MUL     : '*';
DIV     : '/';
ADD     : '+';
SUB     : '-';

program     : (progSegment+ EOF | EOF);

progSegment : function
            | preprocessor NEWLINE
            | link
            //Possibly globals here as well
            ;

declaration : type=ID id=ID;
argumentList: (declaration (',' declaration)*)?;
argumentPack: (expression (',' expression)*)?;
function    : type=ID id=ID '(' arguments=argumentList ')' '{' statements=statement* '}';

preprocessor: '#include' content=.*? NEWLINE                                #include;
link        : 'link' library=STRING ('as' alias=STRING)? ';';

statement   : expression ';'                                                #exprStatement
            | '{' statement* '}'                                            #scope
            | 'return' expression ';'                                       #returnVal
            | 'return' ';'                                                  #returnVoid
            | 'if' '(' cond=expression ')' operation=statement              #if
            | 'while' '(' cond=expression ')' operation=statement           #while
            ;

expression  : INT                                                           #constInt
            | FLOAT                                                         #constFloat
            | STRING                                                        #constString
            | ('true'|'false')                                              #constBool
            | declaration                                                   #decl
            | (namespace=ID '.')? name=ID '(' argumentPack ')'              #functionCall
            | ID                                                            #id
            | left=expression op=(MUL | DIV) right=expression               #muldiv
            | left=expression op=(ADD | SUB) right=expression               #addsub
            | left=expression op=('==' | '!=' | '<' | '>' | '<=' | '>=') right=expression             #comparison
            | '(' expression ')'                                            #inlineScope
            | '(' ID ')' expression                                         #cast
            | dest=expression '=' expr=expression                           #assignment
            ;

