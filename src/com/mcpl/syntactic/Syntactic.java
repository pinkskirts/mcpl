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
	switch(lookAhead().getTokenTag()) {
	case FUNCTION:
	    System.out.println("lever:");
	    analyze();
	    break;
	case EOF:
	    System.out.println("<EOF>");
	    break;
	default:
	    throw new IllegalArgumentException("Syntactical error!");
	}
    }

    public Token lookAhead() {
	if(_tokenCount < _source.size()) {
	    Token token = _source.get(_tokenCount);
	    _tokenCount++;
	    return token;
	}
	throw new IllegalArgumentException("Syntactical error!");
    }
}
