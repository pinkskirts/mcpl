package com.mcpl.lexer;

import com.mcpl.lexer.TokenTag;

public class Token {
    private final TokenTag _tag;
    private final String _atribute;

    public Token(TokenTag tag, String atribute){
	_tag = tag;
	_atribute = atribute;
    }

    public String toString(){
	return "<" + _tag + "," + _atribute + ">";
    }
}
