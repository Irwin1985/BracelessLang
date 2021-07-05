package main

import (
	"BracelessLang/lexer"
	"BracelessLang/parser"
	"BracelessLang/token"
	"fmt"
)

func main() {
	//testLexer()
	//testStream()
	testParser()
}

func testParser() {
	input := `	
	a1 := nil
	b1 := nil
	if condition1
		a2 := nil
		b2 := nil
		if contition11
			a3 := nil
			b3 := nil
			c3 := nil
		if condition12
			a4 := nil
			b4 := nil
		if condition13
			a5 := nil
		c2 := nil
		d2 := nil	
	else
		x := y
		z := a	
	c := a + b
`
	l := lexer.NewLexer(input)
	p := parser.NewParser(l)
	program := p.Program()
	fmt.Println(program.String())
}

func testLexer() {
	input := `
	var a = 10
	var b = 20

	if a > b
		print("a es mayor")
		a = b
		print("igualo a")
	else
		print("a es menor")
		a = b
		print("igualo a")
	
	c := a + b
`
	l := lexer.NewLexer(input)
	tok := l.NextToken()
	for tok.Type != token.EOF {
		fmt.Println(tok.ToString())
		tok = l.NextToken()
	}
	fmt.Println(tok.ToString())
}

func testStream() {
	input := `if a > '0' then a else b;`
	s := lexer.NewStream(input)
	c := s.Read()
	for c != lexer.EOF_CHAR {
		fmt.Printf("%c", c)
		c = s.Read()
	}
}
