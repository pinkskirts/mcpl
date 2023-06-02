/*
 * Analisador Léxico
 * 
 */
package lexer;
import java.util.ArrayList;
import token.*;

public class AnLex {

    //
    public ArrayList<Token> lex_Evaluate(char[] expression) {
        ArrayList<Token> token_List = new ArrayList<>();    // Array de Tokens da cadeia de entrada, CONSIDERANDO
                                                            // whitespaces como Tokens

        for (char c : expression) { // 
            if (Character.toString(c).matches("\\d")) {
                token_List.add(new Token(c, TokenType.NUM));
            } else if (Character.toString(c).matches("[+]|-|[*]|[//]")) {
                token_List.add(new Token(c, TokenType.ARITHM_OPERATOR));
            } else if (Character.toString(c).matches("[><]")) {
                token_List.add(new Token(c, TokenType.LOGIC_OPERATOR));
            } else if (Character.toString(c).matches("\\s")) {
                token_List.add(new Token(c, TokenType.WHITESPACE));
            } else if (Character.toString(c).matches("[A-Za-z,`!?@#$%^&|=]")) {
                token_List.add(new Token(c, TokenType.CHAR));
            } else if (Character.toString(c).matches("\\(")) {
                token_List.add(new Token(c, TokenType.PARENTHESIS_BEGIN));
            } else if (Character.toString(c).matches("\\)")) {
                token_List.add(new Token(c, TokenType.PARENTHESIS_END));
            } else if (Character.toString(c).matches("\\{")) {
                token_List.add(new Token(c, TokenType.SCOPE_BEGIN));
            } else if (Character.toString(c).matches("}")) {
                token_List.add(new Token(c, TokenType.SCOPE_END));
            } else if (Character.toString(c).matches(";")) {
                token_List.add(new Token(c, TokenType.END_INSTRUCTION));
            } else {
                token_List.add(new Token(c, TokenType.ERROR));
            }
        }

        token_List.add(new Token(TokenType.EOF)); // Adição do Token "EOF" ao fim da lista de Tokens

        return lex_Evaluate_Strings(token_List);
    }

    //
    public ArrayList<Token> lex_Evaluate_Strings(ArrayList<Token> token_List) {
        if (token_List.get(0) != null) {
            Token iterador = token_List.get(0); // Recebe o primeiro token da lista

            while (iterador.getType() != TokenType.EOF) {
                int indice_Atual = token_List.indexOf(iterador); // Recebe e atualiza o indice da lista de Tokens
                int proximo = -1;
                if (iterador.getType() != TokenType.EOF) {
                    proximo = indice_Atual + 1; // Look-ahead de 1 passo
                }

                if ( // <<
                        ((iterador.getLexeme() == '<') && (iterador.getType() == TokenType.LOGIC_OPERATOR))
                        && ((token_List.get(proximo) != null) && (token_List.get(proximo).getLexeme() == '<'))
                ) {
                    token_List.remove(proximo);
                    token_List.set(indice_Atual, new Token("<<", TokenType.ARITHM_OPERATOR));
                } else if ( // ==
                        ((iterador.getLexeme() == '=') && (iterador.getType() == TokenType.CHAR))
                                && ((token_List.get(proximo) != null) && (token_List.get(proximo).getLexeme() == '='))
                        ) {
                    token_List.remove(proximo);
                    token_List.set(indice_Atual, new Token("==", TokenType.ARITHM_OPERATOR));
                } else if ( // !=
                        ((iterador.getLexeme() == '!') && (iterador.getType() == TokenType.CHAR))
                                && ((token_List.get(proximo) != null) && (token_List.get(proximo).getLexeme() == '='))
                        ) {
                    token_List.remove(proximo);
                    token_List.set(indice_Atual, new Token("!=", TokenType.LOGIC_OPERATOR));
                } else if ( // >=
                        ((iterador.getLexeme() == '>') && (iterador.getType() == TokenType.LOGIC_OPERATOR))
                                && ((token_List.get(proximo) != null) && (token_List.get(proximo).getLexeme() == '='))
                ) {
                    token_List.remove(proximo);
                    token_List.set(indice_Atual, new Token(">=", TokenType.LOGIC_OPERATOR));
                } else if ( // <=
                        ((iterador.getLexeme() == '<') && (iterador.getType() == TokenType.LOGIC_OPERATOR))
                                && ((token_List.get(proximo) != null) && (token_List.get(proximo).getLexeme() == '='))
                ) {
                    token_List.remove(proximo);
                    token_List.set(indice_Atual, new Token("<=", TokenType.LOGIC_OPERATOR));
                }
                iterador = token_List.get(indice_Atual + 1);
            }
        }

        return token_List;
    }
}
