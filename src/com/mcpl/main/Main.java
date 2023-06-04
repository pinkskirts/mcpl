package com.mcpl.main;

import com.mcpl.lexer.*;
import com.mcpl.syntactic.*;
import java.util.List;

public class Main {
    public static void main(String[] args){
	var source = "lever: djsdjsdkjsd";
	Lexer lexer = new Lexer(source);
	List<Token> tokens = lexer.analyze();
	Syntactic syn = new Syntactic(tokens);
	syn.analyze();
    }
}
