package token;

public enum TokenType {
    // Tipos de Tokens da linguagem
    NUM, // [0-9]
    ARITHM_OPERATOR, // +, -, *, /, <<
    LOGIC_OPERATOR, // ==, !=, and, or, not, >, <, >=, <=
    CONDITIONAL, // compare, redtorch, sculk
    LOOP, // repeater, hopper, observer-this
    CHAR, // [A-Za-z,`@#$%^&*]
    PARENTHESIS_BEGIN,
    PARENTHESIS_END,
    SCOPE_BEGIN, // {
    SCOPE_END, // }
    DATATYPE, // int, boolean, char, float, unsigned
    VAR, // Ex.: craft: int nome
    WHITESPACE,
    ERROR,
    END_INSTRUCTION, // ;
    CHAT, // chat("teste");
    EOF
}
