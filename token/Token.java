/*
 * Lista de Tokens da linguagem MCPL
 */

package token;

public class Token {

    private char lexeme_Char;
    private String lexeme_String;
    private TokenType type;

    public Token(char lexeme_Char, TokenType type) {
        this.lexeme_Char = lexeme_Char;
        this.type = type;
    }
    public Token (String lexeme_String, TokenType type) {
        this.lexeme_String = lexeme_String;
        this.type = type;
    }
    public Token (TokenType type) {
        this.type = type;
    }

    //Getters
    public char getLexeme() { return this.lexeme_Char; }
    public String getLexeme_String() { return this.lexeme_String; }
    public TokenType getType() { return this.type; }

}
