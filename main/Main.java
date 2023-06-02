package main;
import lexer.AnLex;
import token.Token;

public class Main {
    public static void main(String[] args) {
        AnLex lexico = new AnLex();
        char[] cadeia = "teste".toCharArray();

        for (Token token : lexico.lex_Evaluate(cadeia)) {
            System.out.print(token.getType() + "   ");
        }

    }
}
