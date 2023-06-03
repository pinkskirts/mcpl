package com.mcpl.lexer;

public enum TokenTag {
    COMMENT("\\//.*$"),
    INTEGER("\\d+"),
    PLUS("\\+"),
    MINUS("\\-"),
    MULTI("\\*"),
    DIVISOR("\\/"),
    ASSIGNMENT("\\<<"),
    LESS("\\<"),
    LESSEQUALS("\\<="),
    GREATER("\\>"),
    GREATEREQUALS("\\>="),
    EQUALS("\\="),
    NOTEQUALS("\\!="),
    AND("and"),
    OR("or"),
    NOT("not"),
    IF("compare"),
    ELSEIF("redtorch"),
    ELSE("sculk"),
    WHILE("repiater"),
    FUNCTION("lever:"),
    VARIABLE("craft:"),
    TYPEINT("int"),
    IDENTIFIER("[A-Za-z_]\\w*"),
    WHITESPACE("\\s"),
    EOF("<EOF>");

    private final String regex;

    private TokenTag(String regex) {
	this.regex = regex;
    }

    public String regex() {
	return regex;
    }

    public static String tokenPatterns() {
	String[] patterns = new String[values().length];
	int i = 0;
	for(TokenTag t : TokenTag.values()){
	    patterns[i] = t.regex();
	    i++;
	}
	return String.join("|",patterns);
    }
} 
