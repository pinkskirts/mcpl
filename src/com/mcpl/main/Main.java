package com.mcpl.main;

import com.mcpl.lexer.*;
import com.mcpl.syntactic.*;
import java.util.List;

public class Main {
    public static void main(String[] args){
	var source = "lever: int main { craft: int a << 5+2-32+(43 + 3) ; }  ";
	Lexer lexer = new Lexer(source);
	List<Token> tokens = lexer.analyze();
	System.out.println(tokens.toString());
	Syntactic syn = new Syntactic(tokens);
	syn.analyze();
    }
}
