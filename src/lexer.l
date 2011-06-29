/*
�������(lexer.mll)
 
����ԥ塼���ˤȤäƤϡ�ML�ץ������Ȥ����ɤ⡢�Ϥ���Ϥ�����ʸ����Ǥ������Ȥ��Ф��ä���gcd���ä��顢
��l�ס�e�ס�t�ס� �ס�r�ס�e�ס�c�ס� �ס�g�ס�c�ס�d�ס� �ס�m�ס� �ס�n�ס� �ס�=��...
�Τ褦�˸�����櫓�Ǥ������ΤޤޤǤϲ���Ǥ��ʤ��Τǡ��ޤ�
��let�ס�rec�ס�gcd�ס�m�ס�n�ס�=��...
�Τ褦�ʻ���˶��ڤ�ޤ������ν����������ϤȤ����ޤ���
 
������ϤˤϤ�����������ˡ������ޤ����������Ǥ�ocamllex�Ȥ������ޤ���OCaml�ǻ�����Ϥ򤹤뤿��Υġ�������Ѥ��ޤ������Υե����뤬lexer.mll�Ǥ���ocamllex�ˤĤ��Ƥξܺ٤ϥޥ˥奢��ʤʤ��������ˤ򻲾Ȥ��Ƥ�餦���Ȥˤ��ơ����פ�����������ȡ�
 
| '-'? digit+
    { INT(int_of_string (Lexing.lexeme lexbuf)) }
 
�Ȥ��ä��ѥ�����ޥå��󥰤Τ褦�ʹ�ʸ�ˤ�ꡢ������ɽ��'-'? digit+�˥ޥå����������INT���֤������Υ롼����¤٤ƽ񤱤�OK�Ǥ��������ɽ���ǡ�������INT�ʤɡˤϡ����˽Ҥ٤�parser.mly���������Ƥ��ޤ���Lexing.lexeme lexbuf�Ȥ�����ʬ�ϡ��������ʸ�����ɽ���֤��ޤ��ʤ��פ��ȻפäƤ���������
 
*/
package mincaml;
import java.io.*;
import mincaml.*;
%%

%byaccj

%{
  private Parser yyparser;

  public Yylex(java.io.Reader r, Parser yyparser) {
    this(r);
    this.yyparser = yyparser;
  }

  public static void main(String[] args) {
    try {
    Parser yyparser = new Parser(new FileReader(args[0]));
    while(true) {
      int rc = yyparser.yylex();
      System.out.println(rc);
      if (rc <= 0) break;
      if (rc == Parser.EOF) break;
    }
    } catch (Exception e){
      e.printStackTrace();
    }
  }
%}

space = [ \t\n\r]
digit = [0-9]
lower = [a-z]
upper = [A-Z]
let = let
%%

{space}+ {} // { token lexbuf } skip
"(*" {} // { comment lexbuf; token lexbuf } // �ͥ��Ȥ��������ȤΤ���Υȥ�å�
"(" { return Parser.LPAREN; }
")" { return Parser.RPAREN; }
"true"  { yyparser.yylval = new ParserVal(new Boolean(true)); return Parser.BOOL; }
"false" { yyparser.yylval = new ParserVal(new Boolean(false)); return Parser.BOOL; }
"not"   { return Parser.NOT; }
{digit}+ // �����������Ϥ���롼�� (caml2html: lexer_int)
    { yyparser.yylval = new ParserVal(Integer.parseInt(yytext())); return Parser.INT; }
{digit}+ ("." {digit}*)? ([eE] [+-]? {digit}+)?
    { yyparser.yylval = new ParserVal(Double.parseDouble(yytext())); return Parser.FLOAT; }
"-" // -.����󤷤ˤ��ʤ��Ƥ��ɤ�? ��Ĺ����?
    { return Parser.MINUS; }
"+" // +.����󤷤ˤ��ʤ��Ƥ��ɤ�? ��Ĺ����?
    { return Parser.PLUS; }
"-."
    { return Parser.MINUS_DOT; }
"+."
    { return Parser.PLUS_DOT; }
"*."
    { return Parser.AST_DOT; }
"/."
    { return Parser.SLASH_DOT; }
"="
    { return Parser.EQUAL; }
"<>"
    { return Parser.LESS_GREATER; }
"<="
    { return Parser.LESS_EQUAL; }
">="
    { return Parser.GREATER_EQUAL; }
"<"
    { return Parser.LESS; }
">"
    { return Parser.GREATER; }
"if"
    { return Parser.IF; }
"then"
    { return Parser.THEN; }
"else"
    { return Parser.ELSE; }
"let"
    { return Parser.LET; }

"in"
    { return Parser.IN; }
"rec"
    { return Parser.REC; }
","
    { return Parser.COMMA; }
"_"
    { yyparser.yylval = new ParserVal((Object)Id.gentmp(new Type.Unit())); return Parser.IDENT; }
"Array.create" // [XX] ad hoc
    { return Parser.ARRAY_CREATE; }
"."
    { return Parser.DOT; }
"<-"
    { return Parser.LESS_MINUS; }
";"
    { return Parser.SEMICOLON; }
<<EOF>>
    { /*return Parser.EOF;*/ return YYEOF;}
{lower} ({digit}|{lower}|{upper}|"_")* // ¾�Ρ�ͽ���פ���Ǥʤ��Ȥ����ʤ� *)
    { yyparser.yylval = new ParserVal(""+yytext()); return Parser.IDENT;  }


/*
| _
    { failwith
	(Printf.sprintf "unknown token %s near characters %d-%d"
	   (Lexing.lexeme lexbuf)
	   (Lexing.lexeme_start lexbuf)
	   (Lexing.lexeme_end lexbuf)) }
and comment = parse
| "*)"
    { () }
| "(*"
    { comment lexbuf;
      comment lexbuf }
| eof
    { Format.eprintf "warning: unterminated comment@." }
| _
    { comment lexbuf }
*/

.      {  // token desconocido: se produce un mensaje de error 
          yyparser.yyerror("el(los) caracter(es) '"+yytext()+"' no forma(n) ningun token conocido"); 
       }