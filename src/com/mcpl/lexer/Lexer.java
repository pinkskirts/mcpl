package com.mcpl.lexer;

import java.util.List;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.mcpl.lexer.Token;
import com.mcpl.lexer.TokenTag;

public class Lexer {
    private final Pattern pattern;
    private final Matcher matcher;
    private final List<Token> tokens;

    public Lexer(String source) {
	pattern = Pattern.compile(TokenTag.tokenPatterns());
	matcher = pattern.matcher(source);
	tokens = new ArrayList<>();
    }

    public List<Token> analyze() {	
	while(matcher.find()) {
	    String atribute = matcher.group();
	    if(!atribute.matches(TokenTag.WHITESPACE.regex()) && !atribute.matches(TokenTag.COMMENT.regex())) {
		TokenTag tokenTag = findTokenTag(atribute);
		Token token = new Token(tokenTag, atribute);
		tokens.add(token);
	    }
	}
	tokens.add(new Token(TokenTag.EOF, TokenTag.EOF.regex()));
	return tokens;
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
