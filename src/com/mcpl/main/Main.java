package com.mcpl.main;

import com.mcpl.lexer.*;
import java.util.List;

public class Main {
    public static void main(String[] args){
	var source = "9+99or and 44 lever: 32 int asdad";
	Lexer lexer = new Lexer(source);
	List<Token> tokens = lexer.analyze();
	System.out.println(tokens.toString());
    }
}
