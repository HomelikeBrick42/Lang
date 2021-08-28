package main;

import "core:os";
import "core:fmt";
import "core:strconv";

TokenKind :: enum {
    EndOfFile,
    Identifier,
    Integer,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Colon,
    Semicolon,
    Equals,

    GreaterThan,
    LessThan,

    GreaterThanEquals,
    LessThanEquals,

    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    ExclamationMark,

    EqualsEquals,
    ExclamationMarkEquals,
}

Token :: struct {
    kind: TokenKind,
    data: union {
        u64,
        string,
    },
}

Lexer :: struct {
    source: string,
    position: uint,
}

Lexer_Create :: proc(source: string) -> Lexer {
    lexer: Lexer;
    lexer.source = source;
    lexer.position = 0;
    return lexer;
}

Lexer_Destroy :: proc(lexer: ^Lexer) {
}

Lexer_CurrentChar :: proc(lexer: ^Lexer) -> u8 {
    if lexer.position >= len(lexer.source) {
        return 0;
    }
    return lexer.source[lexer.position];
}

Lexer_NextChar :: proc(lexer: ^Lexer) -> u8 {
    current := Lexer_CurrentChar(lexer);
    lexer.position += 1;
    return current;
}

Lexer_NextToken :: proc(lexer: ^Lexer) -> Token {
    for {
        switch Lexer_CurrentChar(lexer) {
            case 0: {
                token: Token;
                token.kind = .EndOfFile;
                return token;
            }

            case ' ', '\t', '\n', '\r': {
                Lexer_NextChar(lexer);
                continue;
            }

            case '0'..'9': {
                start_pos := lexer.position;
                base := 10;

                if Lexer_CurrentChar(lexer) == '0' {
                    Lexer_NextChar(lexer);
                    for Lexer_CurrentChar(lexer) == '_' {
                        Lexer_NextChar(lexer);
                    }
                    switch Lexer_CurrentChar(lexer) {
                        case 'x', 'X': {
                            Lexer_NextChar(lexer);
                            base = 16;
                        }

                        case 'b', 'B': {
                            Lexer_NextChar(lexer);
                            base = 2;
                        }

                        case 'o', 'O': {
                            Lexer_NextChar(lexer);
                            base = 8;
                        }

                        case: {
                            base = 10;
                        }
                    }
                }

                for {
                    chr := Lexer_CurrentChar(lexer);
                    if (chr >= 'A' && chr <= 'Z') || (chr >= 'a' && chr <= 'z') || (chr >= '0' && chr <= '9') || chr == '_' {
                        Lexer_NextChar(lexer);
                    } else {
                        break;
                    }
                }

                value, ok := strconv.parse_u64_of_base(lexer.source[start_pos:lexer.position], base);
                assert(ok, "Invalid integer literal");

                token: Token;
                token.kind = .Integer;
                token.data = value;
                return token;
            }

            case 'A'..'Z', 'a'..'z', '_': {
                buffer: [dynamic]u8;

                for {
                    chr := Lexer_CurrentChar(lexer);
                    if (chr >= 'A' && chr <= 'Z') || (chr >= 'a' && chr <= 'z') || (chr >= '0' && chr <= '9') || chr == '_' {
                        append(&buffer, chr);
                        Lexer_NextChar(lexer);
                    } else {
                        break;
                    }
                }

                value := make([]u8, len(buffer));
                for chr, i in buffer {
                    value[i] = chr;
                }

                token: Token;
                token.kind = .Identifier;
                token.data = string(value);
                return token;
            }

            case '(': { Lexer_NextChar(lexer); token: Token; token.kind = .LParen; return token; }
            case ')': { Lexer_NextChar(lexer); token: Token; token.kind = .RParen; return token; }
            case '{': { Lexer_NextChar(lexer); token: Token; token.kind = .LBrace; return token; }
            case '}': { Lexer_NextChar(lexer); token: Token; token.kind = .RBrace; return token; }
            case '+': { Lexer_NextChar(lexer); token: Token; token.kind = .Plus; return token; }
            case '-': { Lexer_NextChar(lexer); token: Token; token.kind = .Minus; return token; }
            case '*': { Lexer_NextChar(lexer); token: Token; token.kind = .Asterisk; return token; }
            case '%': { Lexer_NextChar(lexer); token: Token; token.kind = .Percent; return token; }
            case ':': { Lexer_NextChar(lexer); token: Token; token.kind = .Colon; return token; }
            case ';': { Lexer_NextChar(lexer); token: Token; token.kind = .Semicolon; return token; }

            case '!': {
                Lexer_NextChar(lexer);
                if Lexer_CurrentChar(lexer) == '=' {
                    Lexer_NextChar(lexer);
                    token: Token;
                    token.kind = .ExclamationMarkEquals;
                    return token;
                }
                token: Token;
                token.kind = .ExclamationMark;
                return token;
            }

            case '=': {
                Lexer_NextChar(lexer);
                if Lexer_CurrentChar(lexer) == '=' {
                    Lexer_NextChar(lexer);
                    token: Token;
                    token.kind = .EqualsEquals;
                    return token;
                }
                token: Token;
                token.kind = .Equals;
                return token;
            }

            case '>': {
                Lexer_NextChar(lexer);
                if Lexer_CurrentChar(lexer) == '=' {
                    Lexer_NextChar(lexer);
                    token: Token;
                    token.kind = .GreaterThanEquals;
                    return token;
                }
                token: Token;
                token.kind = .GreaterThan;
                return token;
            }

            case '<': {
                Lexer_NextChar(lexer);
                if Lexer_CurrentChar(lexer) == '=' {
                    Lexer_NextChar(lexer);
                    token: Token;
                    token.kind = .LessThanEquals;
                    return token;
                }
                token: Token;
                token.kind = .LessThan;
                return token;
            }

            case '/': {
                Lexer_NextChar(lexer);
                if Lexer_CurrentChar(lexer) == '/' {
                    Lexer_NextChar(lexer);
                    for Lexer_CurrentChar(lexer) != '\n' && Lexer_CurrentChar(lexer) != 0 {
                        Lexer_NextChar(lexer);
                    }
                    continue;
                }

                token: Token;
                token.kind = .Slash;
                return token;
            }

            case: {
                message := fmt.tprintf("Unexpected character '{}'", rune(Lexer_CurrentChar(lexer)));
                assert(false, message);
                return {};
            }
        }
    }
}

Ast_Kind :: enum {
    Scope,
    Declaration,
    Assignment,
    Print,
    If,
    While,
    Unary,
    Binary,
    Name,
    Integer,
}

Ast :: struct {
    kind: Ast_Kind,
}

Ast_Statement :: struct {
    using ast: Ast,
}

Ast_Scope :: struct {
    using statement: Ast_Statement,
    statements: [dynamic]^Ast,
}

Ast_Declaration :: struct {
    using statement: Ast_Statement,
    name: string,
    value: ^Ast_Expression,
}

Ast_Assignment :: struct {
    using statement: Ast_Statement,
    name: string,
    value: ^Ast_Expression,
}

Ast_Print :: struct {
    using statement: Ast_Statement,
    value: ^Ast_Expression,
}

Ast_If :: struct {
    using statement: Ast_Statement,
    condition: ^Ast_Expression,
    then_scope: ^Ast_Scope,
    else_scope: ^Ast_Scope,
}

Ast_While :: struct {
    using statement: Ast_Statement,
    condition: ^Ast_Expression,
    scope: ^Ast_Scope,
}

Ast_Expression :: struct {
    using statement: Ast_Statement,
}

Ast_Unary :: struct {
    using expression: Ast_Expression,
    operator: Token,
    operand: ^Ast_Expression,
}

Ast_Binary :: struct {
    using expression: Ast_Expression,
    left: ^Ast_Expression,
    operator: Token,
    right: ^Ast_Expression,
}

Ast_Name :: struct {
    using expression: Ast_Expression,
    resolved_declaration: ^Ast_Declaration,
    value: string,
}

Ast_Integer :: struct {
    using expression: Ast_Expression,
    value: u64,
}

Parser :: struct {
    lexer: Lexer,
    current: Token,
}

Parser_Create :: proc(source: string) -> Parser {
    parser: Parser;
    parser.lexer = Lexer_Create(source);
    parser.current = Lexer_NextToken(&parser.lexer);
    return parser;
}

Parser_Destroy :: proc(parser: ^Parser) {
}

Parser_NextToken :: proc(parser: ^Parser) -> Token {
    current := parser.current;
    parser.current = Lexer_NextToken(&parser.lexer);
    return current;
}

Parser_ExpectToken :: proc(parser: ^Parser, kind: TokenKind) -> Token {
    if parser.current.kind != kind {
        message := fmt.tprintf("Expected '{}' got '{}'", kind, parser.current.kind);
        assert(false, message);
        return {};
    }
    return Parser_NextToken(parser);
}

Parser_Parse :: proc(parser: ^Parser) -> ^Ast {
    scope := new(Ast_Scope);
    scope.kind = .Scope;
    for parser.current.kind != .EndOfFile {
        append(&scope.statements, Parser_ParseStatement(parser));
    }
    return scope;
}

Parser_ParseStatement :: proc(parser: ^Parser) -> ^Ast_Statement {
    #partial switch parser.current.kind {
        case .Semicolon: {
            Parser_ExpectToken(parser, .Semicolon);
            return Parser_ParseStatement(parser);
        }

        case: {
            expression := Parser_ParseExpression(parser);
            if expression.kind == .Name {
                name := (cast(^Ast_Name) expression).value;
                free(expression);

                if name == "print" {
                    print := new(Ast_Print);
                    print.kind = .Print;
                    print.value = Parser_ParseExpression(parser);
                    Parser_ExpectToken(parser, .Semicolon);
                    return print;
                } else if name == "if" {
                    if_ := new(Ast_If);
                    if_.kind = .If;
                    if_.condition = Parser_ParseExpression(parser);
                    if_.then_scope = new(Ast_Scope);
                    if_.then_scope.kind = .Scope;
                    Parser_ExpectToken(parser, .LBrace);
                    for parser.current.kind != .RBrace && parser.current.kind != .EndOfFile {
                        append(&if_.then_scope.statements, Parser_ParseStatement(parser));
                    }
                    Parser_ExpectToken(parser, .RBrace);
                    if parser.current.kind == .Identifier && parser.current.data.(string) == "else" {
                        Parser_ExpectToken(parser, .Identifier);
                        if_.else_scope = new(Ast_Scope);
                        if_.else_scope.kind = .Scope;
                        Parser_ExpectToken(parser, .LBrace);
                        for parser.current.kind != .RBrace && parser.current.kind != .EndOfFile {
                            append(&if_.else_scope.statements, Parser_ParseStatement(parser));
                        }
                        Parser_ExpectToken(parser, .RBrace);
                    }
                    return if_;
                } else if name == "while" {
                    while := new(Ast_While);
                    while.kind = .While;
                    while.condition = Parser_ParseExpression(parser);
                    while.scope = new(Ast_Scope);
                    while.scope.kind = .Scope;
                    Parser_ExpectToken(parser, .LBrace);
                    for parser.current.kind != .RBrace && parser.current.kind != .EndOfFile {
                        append(&while.scope.statements, Parser_ParseStatement(parser));
                    }
                    Parser_ExpectToken(parser, .RBrace);
                    return while;
                } else if parser.current.kind == .Equals {
                    Parser_ExpectToken(parser, .Equals);
                    assignment := new(Ast_Assignment);
                    assignment.kind = .Assignment;
                    assignment.name = name;
                    assignment.value = Parser_ParseExpression(parser);
                    Parser_ExpectToken(parser, .Semicolon);
                    return assignment;
                } else {
                    Parser_ExpectToken(parser, .Colon);
                    Parser_ExpectToken(parser, .Equals);
                    declaration := new(Ast_Declaration);
                    declaration.kind = .Declaration;
                    declaration.name = name;
                    declaration.value = Parser_ParseExpression(parser);
                    Parser_ExpectToken(parser, .Semicolon);
                    return declaration;
                }
            } else {
                Parser_ExpectToken(parser, .Semicolon);
                return expression;
            }
        }
    }
}

Parser_ParseExpression :: proc(parser: ^Parser) -> ^Ast_Expression {
    return Parser_ParseBinaryExpression(parser, 0);
}

Parser_ParsePrimaryExpression :: proc(parser: ^Parser) -> ^Ast_Expression {
    #partial switch parser.current.kind {
        case .Identifier: {
            name := new(Ast_Name);
            name.kind = .Name;
            name.value = Parser_ExpectToken(parser, .Identifier).data.(string);
            return name;
        }

        case .Integer: {
            integer := new(Ast_Integer);
            integer.kind = .Integer;
            integer.value = Parser_ExpectToken(parser, .Integer).data.(u64);
            return integer;
        }

        case .LParen: {
            Parser_ExpectToken(parser, .LParen);
            expression := Parser_ParseExpression(parser);
            Parser_ExpectToken(parser, .RParen);
            return expression;
        }

        case: {
            message := fmt.tprintf("Unexpected '{}'", parser.current.kind);
            assert(false, message);
            return nil;
        }
    }
}

GetUnaryOperatorPrecendence :: proc(token: Token) -> uint {
    #partial switch token.kind {
        case .Plus, .Minus, .ExclamationMark:
            return 4;

        case:
            return 0;
    }
}

GetBinaryOperatorPrecendence :: proc(token: Token) -> uint {
    #partial switch token.kind {
        case .Asterisk, .Slash, .Percent:
            return 3;

        case .Plus, .Minus:
            return 2;

        case .EqualsEquals, .ExclamationMarkEquals,
            .LessThan, .LessThanEquals,
            .GreaterThan, .GreaterThanEquals:
            return 1;

        case:
            return 0;
    }
}

Parser_ParseBinaryExpression :: proc(parser: ^Parser, parent_presedence: uint) -> ^Ast_Expression {
    unary_precedence := GetUnaryOperatorPrecendence(parser.current);
    left: ^Ast_Expression;
    if unary_precedence > parent_presedence {
        operator := Parser_NextToken(parser);
        operand := Parser_ParseBinaryExpression(parser, unary_precedence);

        ast := new(Ast_Unary);
        ast.kind = .Unary;
        ast.operator = operator;
        ast.operand = operand;
        left = ast;
    } else {
        left = Parser_ParsePrimaryExpression(parser);
    }

    for {
        binary_precendence := GetBinaryOperatorPrecendence(parser.current);
        if binary_precendence <= parent_presedence {
            break;
        }

        operator := Parser_NextToken(parser);
        right := Parser_ParseBinaryExpression(parser, binary_precendence);

        ast := new(Ast_Binary);
        ast.kind = .Binary;
        ast.left = left;
        ast.operator = operator;
        ast.right = right;
        left = ast;
    }

    return left;
}

Interp_Value :: union {
    i64,
}

Interp :: struct {
    vars: map[string]Interp_Value,
}

Interp_Ast :: proc(interp: ^Interp, ast: ^Ast) {
    #partial switch ast.kind {
        case .Scope: {
            scope := cast(^Ast_Scope) ast;
            for statement in scope.statements {
                Interp_Ast(interp, statement);
            }
        }

        case .Declaration: {
            declaration := cast(^Ast_Declaration) ast;
            if declaration.name in interp.vars {
                message := fmt.tprintf("Variable '{}' already exists!", declaration.name);
                assert(false, message);
            }
            interp.vars[declaration.name] = Interp_Expression(interp, declaration.value);
        }

        case .Assignment: {
            assignment := cast(^Ast_Assignment) ast;
            if !(assignment.name in interp.vars) {
                message := fmt.tprintf("Variable '{}' does not exist!", assignment.name);
                assert(false, message);
            }
            interp.vars[assignment.name] = Interp_Expression(interp, assignment.value);
        }

        case .Print: {
            print := cast(^Ast_Print) ast;
            fmt.println(Interp_Expression(interp, print.value));
        }

        case .If: {
            if_ := cast(^Ast_If) ast;
            condition := Interp_Expression(interp, if_.condition);
            if condition.(i64) != 0 {
                Interp_Ast(interp, if_.then_scope);
            } else if if_.else_scope != nil {
                Interp_Ast(interp, if_.else_scope);
            }
        }

        case .While: {
            while := cast(^Ast_While) ast;
            condition := Interp_Expression(interp, while.condition);
            for condition.(i64) != 0 {
                Interp_Ast(interp, while.scope);
                condition = Interp_Expression(interp, while.condition);
            }
        }

        case: {
            assert(false, "Unexpected ast kind passed to Interp_Ast");
        }
    }
}

Interp_Expression :: proc(interp: ^Interp, expression: ^Ast_Expression) -> Interp_Value {
    #partial switch expression.kind {
        case .Integer: {
            return i64((cast(^Ast_Integer) expression).value);
        }

        case .Name: {
            value, exists := interp.vars[(cast(^Ast_Name) expression).value];
            if !exists {
                message := fmt.tprintf("Variable '{}' does not exist!", (cast(^Ast_Name) expression).value);
                assert(false, message);
            }
            return value;
        }

        case .Unary: {
            unary := cast(^Ast_Unary) expression;
            operand := Interp_Expression(interp, unary.operand);

            #partial switch unary.operator.kind {
                case .Plus: return +operand.(i64);
                case .Minus: return -operand.(i64);
                case .ExclamationMark: return operand.(i64) == 0 ? i64(1) : i64(0);

                case: {
                    assert(false, "Unexpected operator kind");
                    return nil;
                }
            }
        }

        case .Binary: {
            binary := cast(^Ast_Binary) expression;
            left := Interp_Expression(interp, binary.left);
            right := Interp_Expression(interp, binary.right);

            #partial switch binary.operator.kind {
                case .Plus: return left.(i64) + right.(i64);
                case .Minus: return left.(i64) - right.(i64);
                case .Asterisk: return left.(i64) * right.(i64);
                case .Slash: return left.(i64) / right.(i64);
                case .Percent: return left.(i64) % right.(i64);
                case .EqualsEquals: return (left.(i64) == right.(i64)) ? i64(1) : i64(0);
                case .ExclamationMarkEquals: return (left.(i64) != right.(i64)) ? i64(1) : i64(0);
                case .GreaterThan: return (left.(i64) > right.(i64)) ? i64(1) : i64(0);
                case .GreaterThanEquals: return (left.(i64) >= right.(i64)) ? i64(1) : i64(0);
                case .LessThan: return (left.(i64) < right.(i64)) ? i64(1) : i64(0);
                case .LessThanEquals: return (left.(i64) <= right.(i64)) ? i64(1) : i64(0);

                case: {
                    assert(false, "Unexpected operator kind");
                    return nil;
                }
            }
        }

        case: {
            assert(false, "Unexpected ast kind passed to Interp_Expression");
            return nil;
        }
    }
}

main :: proc() {
    data: [1]byte;
    defer os.read(os.stdin, data[:]);

    args := os._alloc_command_line_arguments();
    if len(args) != 2 {
        fmt.printf("usage: {} <file>\n", args[0]);
        return;
    }

    bytes, ok := os.read_entire_file(args[1]);
    if !ok {
        fmt.printf("Unable to open file '{}'\n", args[1]);
        return;
    }

    source := string(bytes);

    // Lexer_Test(source);
    // fmt.print("\n\n");
    // Parser_Test(source);

    parser := Parser_Create(source);
    defer Parser_Destroy(&parser);
    ast := Parser_Parse(&parser);

    interp: Interp;
    Interp_Ast(&interp, ast);
}

/*
Lexer_Test :: proc(source: string) {
    lexer := Lexer_Create(source);
    defer Lexer_Destroy(&lexer);

    for {
        token := Lexer_NextToken(&lexer);
        fmt.println(token.kind);

        if token.kind == .EndOfFile {
            break;
        }
    }
}

Parser_Test :: proc(source: string) {
    parser := Parser_Create(source);
    defer Parser_Destroy(&parser);

    ast := Parser_Parse(&parser);

    Print_Ast :: proc(ast: ^Ast, indent: uint) {
        Print_Indent :: proc(indent: uint) {
            for _ in 0..indent-1 {
                fmt.print("    ");
            }
        }

        if ast == nil {
            fmt.print("(nil)");
        } else {
            switch ast.kind {
                case .Scope: {
                    fmt.print("(Scope");
                    for statement in (cast(^Ast_Scope) ast).statements {
                        fmt.println();
                        Print_Indent(indent + 1);
                        Print_Ast(statement, indent + 1);
                    }
                    fmt.print(")");
                }

                case .Declaration: {
                    fmt.printf("(Declcaration '{}' := ", (cast(^Ast_Declaration) ast).name);
                    Print_Ast((cast(^Ast_Declaration) ast).value, indent);
                    fmt.print(")");
                }

                case .Assignment: {
                    fmt.printf("(Assignment '{}' = ", (cast(^Ast_Assignment) ast).name);
                    Print_Ast((cast(^Ast_Assignment) ast).value, indent);
                    fmt.print(")");
                }

                case .Print: {
                    fmt.print("(Print ");
                    Print_Ast((cast(^Ast_Print) ast).value, indent);
                    fmt.print(")");
                }
                
                case .Unary: {
                    fmt.println("(Unary");
                    Print_Indent(indent + 1);
                    fmt.printf("{}\n", (cast(^Ast_Unary) ast).operator.kind);
                    Print_Indent(indent + 2);
                    Print_Ast((cast(^Ast_Unary) ast).operand, indent + 2);
                    fmt.print(")");
                }

                case .Binary: {
                    fmt.println("(Binary");
                    Print_Indent(indent + 1);
                    fmt.printf("{}\n", (cast(^Ast_Binary) ast).operator.kind);
                    Print_Indent(indent + 2);
                    Print_Ast((cast(^Ast_Binary) ast).left, indent + 2);
                    fmt.println();
                    Print_Indent(indent + 2);
                    Print_Ast((cast(^Ast_Binary) ast).right, indent + 2);
                    fmt.print(")");
                }

                case .Name: {
                    fmt.printf("(Name '{}'')", (cast(^Ast_Name) ast).value);
                }

                case .Integer: {
                    fmt.printf("(Integer {})", (cast(^Ast_Integer) ast).value);
                }
            }
        }
    }

    Print_Ast(ast, 0);
    fmt.println();
}
*/
