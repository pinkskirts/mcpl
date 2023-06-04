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
	while(_tokenCount < _source.size() && _source.get(_tokenCount).getTokenTag() != TokenTag.EOF) {
	    if(_source.get(_tokenCount).getTokenTag() == TokenTag.FUNCTION) {
		func();
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

    public void match(TokenTag tag) {
	Token token = lookAhead();
	if(token.getTokenTag() == tag) {
	    System.out.print(token.getAttribute() + " ");
	} else {
	    throw new IllegalArgumentException("\n Syntactical error! " + token.toString());
	} 
    }

    public void func() {
	match(TokenTag.FUNCTION);
	match(TokenTag.TYPE);
	match(TokenTag.IDENTIFIER);
	match(TokenTag.OBRACE);
	if(_source.get(_tokenCount).getTokenTag() != TokenTag.CBRACE) {
	    inst();
	}
	match(TokenTag.CBRACE);
    }

    public void term() {
	Token token = _source.get(_tokenCount);
	TokenTag tokenTag = _source.get(_tokenCount).getTokenTag();
	if(tokenTag == TokenTag.INTEGER) {
	    match(TokenTag.INTEGER);
	} else if(tokenTag == TokenTag.FLOAT) {
	    match(TokenTag.FLOAT);
	} else if(tokenTag == TokenTag.IDENTIFIER) {
	    match(TokenTag.IDENTIFIER);
	} else if(tokenTag == TokenTag.OPARENTHESES) {
	    match(TokenTag.OPARENTHESES);
	    arithmeticExpr();
	    match(TokenTag.CPARENTHESES);
	} else {
	    throw new IllegalArgumentException("\n Syntactical error! " + token.toString());
	}
    }

    public void logicalTerm() {
	Token token = _source.get(_tokenCount);
	TokenTag tokenTag = _source.get(_tokenCount).getTokenTag();
	if(tokenTag == TokenTag.INTEGER ||
	   tokenTag == TokenTag.FLOAT ||
	   tokenTag == TokenTag.IDENTIFIER ||
	   tokenTag == TokenTag.OPARENTHESES) {
	    arithmeticExpr();
	    logicalOper();
	    arithmeticExpr();
	} else {
	    throw new IllegalArgumentException("\n Syntactical error! " + token.toString());
	}
    }

    public void arithmeticOper() {
	TokenTag tokenTag = _source.get(_tokenCount).getTokenTag();
	if(tokenTag == TokenTag.PLUS) {
	    match(TokenTag.PLUS);
	} else if(tokenTag == TokenTag.MINUS) {
	    match(TokenTag.MINUS);
	} else if(tokenTag == TokenTag.MULTI) {
	    match(TokenTag.MULTI);
	} else if(tokenTag == TokenTag.DIVISOR) {
	    match(TokenTag.DIVISOR);
	}
    }

    public void _arithmeticExpr() {
	TokenTag tokenTag = _source.get(_tokenCount).getTokenTag();
	if(tokenTag == TokenTag.PLUS ||
	   tokenTag == TokenTag.MINUS ||
	   tokenTag == TokenTag.MULTI ||
	   tokenTag == TokenTag.DIVISOR) {
	    arithmeticOper();
	    arithmeticExpr();
	}
    }

    public void arithmeticExpr() {
	term();
	_arithmeticExpr();
    }

    public void logicalOper() {
	TokenTag tokenTag = _source.get(_tokenCount).getTokenTag();
	if(tokenTag == TokenTag.NOTEQUALS) {
	    match(TokenTag.NOTEQUALS);
	} else if(tokenTag == TokenTag.EQUALS) {
	    match(TokenTag.EQUALS);
	} else if(tokenTag == TokenTag.GREATER) {
	    match(TokenTag.GREATER);
	} else if(tokenTag == TokenTag.GREATEREQUALS) {
	    match(TokenTag.GREATEREQUALS);
	} else if(tokenTag == TokenTag.LESS) {
	    match(TokenTag.LESS);
	} else if(tokenTag == TokenTag.LESSEQUALS) {
	    match(TokenTag.LESSEQUALS);
	}
    }

    public void _logicalExpr() {
	TokenTag tokenTag = _source.get(_tokenCount).getTokenTag();
	if(tokenTag == TokenTag.AND || tokenTag == TokenTag.OR || tokenTag == TokenTag.NOTEQUALS) {
	    logicalOper();
	    logicalExpr();
	}
    }

    public void logicalExpr() {
	TokenTag tokenTag = _source.get(_tokenCount).getTokenTag();
	if(tokenTag == TokenTag.NOT) {
	    match(TokenTag.NOT);
	}
	logicalTerm();
    }

    public void ifStatement() {
	match(TokenTag.IF);
	match(TokenTag.OPARENTHESES);
	logicalExpr();
	match(TokenTag.CPARENTHESES);
	match(TokenTag.OBRACE);
	if(_source.get(_tokenCount).getTokenTag() != TokenTag.CBRACE) {
	    inst();
	}
	match(TokenTag.CBRACE);
	if(_source.get(_tokenCount).getTokenTag() == TokenTag.ELSE) {
	    match(TokenTag.ELSE);
	    match(TokenTag.OBRACE);
	    if(_source.get(_tokenCount).getTokenTag() != TokenTag.CBRACE) {
		inst();
	    }
	    match(TokenTag.CBRACE);
	}
    }

    public void whileStatement() {
	match(TokenTag.WHILE);
	match(TokenTag.OPARENTHESES);
	logicalExpr();
	match(TokenTag.CPARENTHESES);
	match(TokenTag.OBRACE);
	if(_source.get(_tokenCount).getTokenTag() != TokenTag.CBRACE) {
	    inst();
	}
	match(TokenTag.CBRACE);
    }

    public void varDeclare() {
	match(TokenTag.VARIABLE);
	match(TokenTag.TYPE);
	match(TokenTag.IDENTIFIER);
	if(_source.get(_tokenCount).getTokenTag() != TokenTag.SEMICOLON) {
	    match(TokenTag.ASSIGNMENT);
	    arithmeticExpr();
	}
	match(TokenTag.SEMICOLON);
    }

    public void inst() {
	while(_tokenCount < _source.size() && _source.get(_tokenCount).getTokenTag() != TokenTag.CBRACE) {
	    if(_source.get(_tokenCount).getTokenTag() == TokenTag.VARIABLE) {
		varDeclare();
	    } else if (_source.get(_tokenCount).getTokenTag() == TokenTag.IF) {
		ifStatement();
	    } else if (_source.get(_tokenCount).getTokenTag() == TokenTag.WHILE) {
		whileStatement();
	    } else {
		throw new IllegalArgumentException("\n Syntactical error! " + _source.get(_tokenCount).toString());
	    }
	}
    }
}
