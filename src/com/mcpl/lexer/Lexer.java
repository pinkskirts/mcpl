package com.mcpl.lexer;

import java.util.List;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.mcpl.lexer.Token;
import com.mcpl.lexer.TokenTag;

public class Lexer {
    private final Pattern _pattern;
    private final Matcher _matcher;
    private final List<Token> _tokens;

    public Lexer(String source) {
	_pattern = Pattern.compile(TokenTag.tokenPatterns());
	_matcher = _pattern.matcher(source);
	_tokens = new ArrayList<>();
    }

    public List<Token> analyze() {	
	while(_matcher.find()) {
	    String attribute = _matcher.group();
	    if(!attribute.matches(TokenTag.WHITESPACE.regex()) && !attribute.matches(TokenTag.COMMENT.regex())) {
		TokenTag tokenTag = findTokenTag(attribute);
		Token token = new Token(tokenTag, attribute);
		_tokens.add(token);
	    }
	}
	_tokens.add(new Token(TokenTag.EOF, TokenTag.EOF.regex()));
	return _tokens;
    }

    public TokenTag findTokenTag(String token) {
	if(token.matches(TokenTag.INTEGER.regex())) {
	    return TokenTag.INTEGER;
	} else if (token.matches(TokenTag.PLUS.regex())) {
	    return TokenTag.PLUS;
	} else if (token.matches(TokenTag.MINUS.regex())) {
	    return TokenTag.MINUS;
	} else if (token.matches(TokenTag.MULTI.regex())) {
	    return TokenTag.MULTI;
	} else if (token.matches(TokenTag.DIVISOR.regex())) {
	    return TokenTag.DIVISOR;
	} else if (token.matches(TokenTag.ASSIGNMENT.regex())) {
	    return TokenTag.ASSIGNMENT;
	} else if (token.matches(TokenTag.LESS.regex())) {
	    return TokenTag.LESS;
	} else if (token.matches(TokenTag.LESSEQUALS.regex())) {
	    return TokenTag.LESSEQUALS;
	} else if (token.matches(TokenTag.GREATER.regex())) {
	    return TokenTag.GREATER;
	} else if (token.matches(TokenTag.GREATEREQUALS.regex())) {
	    return TokenTag.GREATEREQUALS;
	} else if (token.matches(TokenTag.EQUALS.regex())) {
	    return TokenTag.EQUALS;
	} else if (token.matches(TokenTag.NOTEQUALS.regex())) {
	    return TokenTag.NOTEQUALS;
	} else if (token.matches(TokenTag.AND.regex())) {
	    return TokenTag.AND;
	} else if (token.matches(TokenTag.OR.regex())) {
	    return TokenTag.OR;
	} else if (token.matches(TokenTag.NOT.regex())) {
	    return TokenTag.NOT;
	} else if (token.matches(TokenTag.IF.regex())) {
	    return TokenTag.IF;
	} else if (token.matches(TokenTag.ELSEIF.regex())) {
	    return TokenTag.ELSEIF;
	} else if (token.matches(TokenTag.ELSE.regex())) {
	    return TokenTag.ELSE;
	} else if (token.matches(TokenTag.WHILE.regex())) {
	    return TokenTag.WHILE;
	} else if (token.matches(TokenTag.FUNCTION.regex())) {
	    return TokenTag.FUNCTION;
	} else if (token.matches(TokenTag.VARIABLE.regex())) {
	    return TokenTag.VARIABLE;
	} else if (token.matches(TokenTag.TYPEINT.regex())) {
	    return TokenTag.TYPEINT;
	} else if (token.matches(TokenTag.IDENTIFIER.regex())) {
	    return TokenTag.IDENTIFIER;
	} else {
	    throw new IllegalArgumentException("Lexical problem; Invalid Token: " + token);
	}
    }
}
