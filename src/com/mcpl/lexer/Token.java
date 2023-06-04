package com.mcpl.lexer;

import com.mcpl.lexer.TokenTag;

public class Token {
    private final TokenTag _tag;
    private final String _attribute;

    public Token(TokenTag tag, String attribute) {
	_tag = tag;
	_attribute = attribute;
    }

    public String toString() {
	return "<" + _tag + "," + _attribute + ">";
    }
}
