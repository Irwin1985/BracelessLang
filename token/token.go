package token

import "fmt"

type TokenType byte

const (
	ILLEGAL TokenType = iota
	EOF
	NEWLINE

	// Identifiers + literals
	IDENT  // add, foobar, x, y, ...
	INT    // 1234
	STRING // "foobar"

	// Operators
	ASSIGN
	BINDING
	PLUS
	PLUS_EQ
	MINUS
	MINUS_EQ
	BANG
	MUL
	MUL_EQ
	DIV
	DIV_EQ

	// Comparison operators
	LT
	LEQ
	EQ
	NEQ
	GT
	GEQ

	// Delimiters
	COMMA
	COLON
	SEMICOLON
	LPAREN
	RPAREN
	LBRACE
	RBRACE
	LBRACKET
	RBRACKET

	// keywords
	FUNCTION
	VAR
	TRUE
	FALSE
	NIL
	AND
	OR
	IF
	ELIF
	ELSE
	RETURN
)

// displayable tokens
var tokenNames = []string{
	"ILLEGAL",
	"EOF",
	"NEWLINE",

	"INDENT", // add, foobar, x, y, ...
	"INT",    // 1234
	"STRING", // "foobar"

	// Operators
	"ASSIGN",
	"BINDING",
	"PLUS",
	"PLUS_EQ",
	"MINUS",
	"MINUS_EQ",
	"BANG",
	"MUL",
	"MUL_EQ",
	"DIV",
	"DIV_EQ",

	// Comparison operators
	"LT",
	"LEQ",
	"EQ",
	"NEQ",
	"GT",
	"GEQ",

	// Delimiters
	"COMMA",
	"COLON",
	"SEMICOLON",
	"LPAREN",
	"RPAREN",
	"LBRACE",
	"RBRACE",
	"LBRACKET",
	"RBRACKET",

	// keywords
	"FUNCTION",
	"VAR",
	"TRUE",
	"FALSE",
	"NIL",
	"AND",
	"OR",
	"IF",
	"ELIF",
	"ELSE",
	"RETURN",
}

// keywords
var keywords = map[string]TokenType{
	"fn":     FUNCTION,
	"var":    VAR,
	"true":   TRUE,
	"false":  FALSE,
	"nil":    NIL,
	"and":    AND,
	"or":     OR,
	"if":     IF,
	"elif":   ELIF,
	"else":   ELSE,
	"return": RETURN,
}

// small tokens
var smallTokens = map[string]TokenType{
	"=":  ASSIGN,
	":=": BINDING,
	"+":  PLUS,
	"+=": PLUS_EQ,
	"-":  MINUS,
	"-=": MINUS_EQ,
	"!":  BANG,
	"*":  MUL,
	"*=": MUL_EQ,
	"/":  DIV,
	"/=": DIV_EQ,

	// Comparison operators
	"<":  LT,
	"<=": LEQ,
	"==": EQ,
	"!=": NEQ,
	">":  GT,
	">=": GEQ,

	// Delimiters
	",": COMMA,
	":": COLON,
	";": NEWLINE,
	"(": LPAREN,
	")": RPAREN,
	"{": LBRACE,
	"}": RBRACE,
	"[": LBRACKET,
	"]": RBRACKET,
}

// Token struct
type Token struct {
	Type   TokenType
	Lexeme string
	Line   int
	Col    int
}

func (t *Token) ToString() string {
	return fmt.Sprintf("<Ln %d, Col %d \t <%s, '%s'>", t.Line, t.Col, tokenNames[t.Type], t.Lexeme)
}

// Return the matching token keyword or IDENT
func LookupIdent(key string) TokenType {
	if value, ok := keywords[key]; ok {
		return value
	}
	return IDENT
}

// Return the matching single or double small token
func Special(name string) (TokenType, bool) {
	if tok, ok := smallTokens[name]; ok {
		return tok, true
	}
	return EOF, false
}
