package com.mcpl.lexer;

public enum TokenTag {
    COMMENT("\\//.*$"),
    INTEGER("\\d+"),
    FLOAT("\\d+\\.\\d+"),
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
    OBRACE("\\{"),
    CBRACE("\\}"),
    OPARENTHESES("\\("),
    CPARENTHESES("\\)"),
    VARIABLE("craft:"),
    TYPE("int|void"),
    IDENTIFIER("[A-Za-z_]\\w*"),
    WHITESPACE("\\s"),
    SEMICOLON(";"),
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
