package com.mcpl.syntactic;

import java.util.List;
import java.util.ArrayList;

import com.mcpl.lexer.Token;
import com.mcpl.lexer.TokenTag;

public class Syntactic {
    private final List<Token> _source;
    private int _tokenCount;

    public Syntactic (List<Token> source) {
	_source = source;
	_tokenCount = 0;
    }

    public void analyze() {
	func();
    }

    public Token lookAhead() {
	if(_tokenCount < _source.size()) {
	    Token token = _source.get(_tokenCount);
	    _tokenCount++;
	    return token;
	}
	throw new IllegalArgumentException("Syntactical error!");
    }

    public void match(TokenTag tag) {
	Token token = lookAhead();
	if(token.getTokenTag() == tag) {
	    System.out.print(token.getAttribute() + " ");
	} else {
	    throw new IllegalArgumentException("Syntactical error!");
	} 
    }

    public void func() {
	match(TokenTag.FUNCTION);
	match(TokenTag.TYPE);
	match(TokenTag.IDENTIFIER);
	match(TokenTag.OBRACE);
	if(_source.get(_tokenCount+1).getTokenTag() != TokenTag.CBRACE) { //lookAhead + 1
	    inst();
	}
	match(TokenTag.CBRACE);
    }

    public void arithmeticExpr() {
    }

    public void varDeclare() {
	match(TokenTag.VARIABLE);
	match(TokenTag.TYPE);
	match(TokenTag.IDENTIFIER);
	if(_source.get(_tokenCount+1).getTokenTag() != TokenTag.SEMICOLON) { // lookahead + 1
	    match(TokenTag.ASSIGNMENT);
	    match(TokenTag.INTEGER); // PLACEHOLDER
	}
	match(TokenTag.SEMICOLON);
    }

    public void inst() {
	if(_source.get(_tokenCount+1).getTokenTag() != TokenTag.VARIABLE) { // lookahead + 1
	    varDeclare();
	}
    }
}
