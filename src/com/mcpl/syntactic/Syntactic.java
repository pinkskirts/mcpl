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
	for(int i = 0; i < _source.size(); i++) {
	    System.out.println(_source.get(i).toString());
	    System.out.println(_source.get(i).getTokenTag());
	    TokenTag tag = _source.get(i).getTokenTag();
	    switch(tag) {
	    case tag.FUNCTION:
		System.out.printf("lexer: ");
		if(_source.get(i+1).getTokenTag() != null && _source.get(i).getTokenTag() == TokenTag.IDENTIFIER) {
		    System.out.println("indentifier ");
		} else {
		    throw new IllegalArgumentException("Syntactical error!");
		}	    
		break;
	    default:
		throw new IllegalArgumentException("Syntactical error!");
	    }
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
