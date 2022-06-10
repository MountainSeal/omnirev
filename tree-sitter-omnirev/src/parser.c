#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 13
#define STATE_COUNT 62
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 39
#define ALIAS_COUNT 0
#define TOKEN_COUNT 30
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 0
#define MAX_ALIAS_SEQUENCE_LENGTH 6
#define PRODUCTION_ID_COUNT 1

enum {
  anon_sym_type = 1,
  anon_sym_EQ = 2,
  anon_sym_expr = 3,
  anon_sym_COLON = 4,
  anon_sym_term = 5,
  anon_sym_I = 6,
  anon_sym_PLUS = 7,
  anon_sym_STAR = 8,
  anon_sym_DASH_GT = 9,
  anon_sym_rec = 10,
  anon_sym_DOT = 11,
  anon_sym_LPAREN = 12,
  anon_sym_RPAREN = 13,
  anon_sym_unit = 14,
  anon_sym_inl = 15,
  anon_sym_inr = 16,
  anon_sym_COMMA = 17,
  anon_sym_EQ_GT = 18,
  anon_sym_fold = 19,
  anon_sym_LBRACK = 20,
  anon_sym_RBRACK = 21,
  anon_sym_PIPE = 22,
  anon_sym_trace = 23,
  anon_sym_SEMI = 24,
  anon_sym_TILDE = 25,
  anon_sym_empty = 26,
  anon_sym_id = 27,
  anon_sym_AT = 28,
  sym_identifier = 29,
  sym_source_file = 30,
  sym_definition = 31,
  sym_type = 32,
  sym__parenthesis_type = 33,
  sym_term = 34,
  sym__parenthesis_term = 35,
  sym_expression = 36,
  sym__parenthesis_expr = 37,
  aux_sym_source_file_repeat1 = 38,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_type] = "type",
  [anon_sym_EQ] = "=",
  [anon_sym_expr] = "expr",
  [anon_sym_COLON] = ":",
  [anon_sym_term] = "term",
  [anon_sym_I] = "I",
  [anon_sym_PLUS] = "+",
  [anon_sym_STAR] = "*",
  [anon_sym_DASH_GT] = "->",
  [anon_sym_rec] = "rec",
  [anon_sym_DOT] = ".",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_unit] = "unit",
  [anon_sym_inl] = "inl",
  [anon_sym_inr] = "inr",
  [anon_sym_COMMA] = ",",
  [anon_sym_EQ_GT] = "=>",
  [anon_sym_fold] = "fold",
  [anon_sym_LBRACK] = "[",
  [anon_sym_RBRACK] = "]",
  [anon_sym_PIPE] = "|",
  [anon_sym_trace] = "trace",
  [anon_sym_SEMI] = ";",
  [anon_sym_TILDE] = "~",
  [anon_sym_empty] = "empty",
  [anon_sym_id] = "id",
  [anon_sym_AT] = "@",
  [sym_identifier] = "identifier",
  [sym_source_file] = "source_file",
  [sym_definition] = "definition",
  [sym_type] = "type",
  [sym__parenthesis_type] = "_parenthesis_type",
  [sym_term] = "term",
  [sym__parenthesis_term] = "_parenthesis_term",
  [sym_expression] = "expression",
  [sym__parenthesis_expr] = "_parenthesis_expr",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_type] = anon_sym_type,
  [anon_sym_EQ] = anon_sym_EQ,
  [anon_sym_expr] = anon_sym_expr,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_term] = anon_sym_term,
  [anon_sym_I] = anon_sym_I,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_STAR] = anon_sym_STAR,
  [anon_sym_DASH_GT] = anon_sym_DASH_GT,
  [anon_sym_rec] = anon_sym_rec,
  [anon_sym_DOT] = anon_sym_DOT,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_unit] = anon_sym_unit,
  [anon_sym_inl] = anon_sym_inl,
  [anon_sym_inr] = anon_sym_inr,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [anon_sym_EQ_GT] = anon_sym_EQ_GT,
  [anon_sym_fold] = anon_sym_fold,
  [anon_sym_LBRACK] = anon_sym_LBRACK,
  [anon_sym_RBRACK] = anon_sym_RBRACK,
  [anon_sym_PIPE] = anon_sym_PIPE,
  [anon_sym_trace] = anon_sym_trace,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [anon_sym_TILDE] = anon_sym_TILDE,
  [anon_sym_empty] = anon_sym_empty,
  [anon_sym_id] = anon_sym_id,
  [anon_sym_AT] = anon_sym_AT,
  [sym_identifier] = sym_identifier,
  [sym_source_file] = sym_source_file,
  [sym_definition] = sym_definition,
  [sym_type] = sym_type,
  [sym__parenthesis_type] = sym__parenthesis_type,
  [sym_term] = sym_term,
  [sym__parenthesis_term] = sym__parenthesis_term,
  [sym_expression] = sym_expression,
  [sym__parenthesis_expr] = sym__parenthesis_expr,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_type] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_expr] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_term] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_I] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PLUS] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_STAR] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_rec] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_unit] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_inl] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_inr] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_fold] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PIPE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_trace] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SEMI] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_TILDE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_empty] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_id] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AT] = {
    .visible = true,
    .named = false,
  },
  [sym_identifier] = {
    .visible = true,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym_definition] = {
    .visible = true,
    .named = true,
  },
  [sym_type] = {
    .visible = true,
    .named = true,
  },
  [sym__parenthesis_type] = {
    .visible = false,
    .named = true,
  },
  [sym_term] = {
    .visible = true,
    .named = true,
  },
  [sym__parenthesis_term] = {
    .visible = false,
    .named = true,
  },
  [sym_expression] = {
    .visible = true,
    .named = true,
  },
  [sym__parenthesis_expr] = {
    .visible = false,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(36);
      if (lookahead == '(') ADVANCE(51);
      if (lookahead == ')') ADVANCE(52);
      if (lookahead == '*') ADVANCE(46);
      if (lookahead == '+') ADVANCE(45);
      if (lookahead == ',') ADVANCE(59);
      if (lookahead == '-') ADVANCE(5);
      if (lookahead == '.') ADVANCE(50);
      if (lookahead == ':') ADVANCE(41);
      if (lookahead == ';') ADVANCE(68);
      if (lookahead == '=') ADVANCE(39);
      if (lookahead == '@') ADVANCE(74);
      if (lookahead == 'I') ADVANCE(43);
      if (lookahead == '[') ADVANCE(63);
      if (lookahead == ']') ADVANCE(64);
      if (lookahead == 'e') ADVANCE(20);
      if (lookahead == 'f') ADVANCE(23);
      if (lookahead == 'i') ADVANCE(10);
      if (lookahead == 'r') ADVANCE(12);
      if (lookahead == 't') ADVANCE(13);
      if (lookahead == 'u') ADVANCE(22);
      if (lookahead == '|') ADVANCE(65);
      if (lookahead == '~') ADVANCE(69);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      END_STATE();
    case 1:
      if (lookahead == '\'') ADVANCE(93);
      END_STATE();
    case 2:
      if (lookahead == '\'') ADVANCE(1);
      if (lookahead == '_') ADVANCE(1);
      END_STATE();
    case 3:
      if (lookahead == '(') ADVANCE(51);
      if (lookahead == ')') ADVANCE(52);
      if (lookahead == ',') ADVANCE(59);
      if (lookahead == ';') ADVANCE(68);
      if (lookahead == '=') ADVANCE(6);
      if (lookahead == '@') ADVANCE(74);
      if (lookahead == 'e') ADVANCE(85);
      if (lookahead == 'f') ADVANCE(87);
      if (lookahead == 'i') ADVANCE(78);
      if (lookahead == 't') ADVANCE(89);
      if (lookahead == 'u') ADVANCE(86);
      if (lookahead == '|') ADVANCE(65);
      if (lookahead == '~') ADVANCE(69);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(3)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 4:
      if (lookahead == '(') ADVANCE(51);
      if (lookahead == 'I') ADVANCE(44);
      if (lookahead == 'r') ADVANCE(81);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(4)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 5:
      if (lookahead == '>') ADVANCE(47);
      END_STATE();
    case 6:
      if (lookahead == '>') ADVANCE(60);
      END_STATE();
    case 7:
      if (lookahead == 'a') ADVANCE(9);
      END_STATE();
    case 8:
      if (lookahead == 'c') ADVANCE(48);
      END_STATE();
    case 9:
      if (lookahead == 'c') ADVANCE(16);
      END_STATE();
    case 10:
      if (lookahead == 'd') ADVANCE(72);
      if (lookahead == 'n') ADVANCE(19);
      END_STATE();
    case 11:
      if (lookahead == 'd') ADVANCE(61);
      END_STATE();
    case 12:
      if (lookahead == 'e') ADVANCE(8);
      END_STATE();
    case 13:
      if (lookahead == 'e') ADVANCE(27);
      if (lookahead == 'r') ADVANCE(7);
      if (lookahead == 'y') ADVANCE(25);
      END_STATE();
    case 14:
      if (lookahead == 'e') ADVANCE(27);
      if (lookahead == 'y') ADVANCE(25);
      END_STATE();
    case 15:
      if (lookahead == 'e') ADVANCE(37);
      END_STATE();
    case 16:
      if (lookahead == 'e') ADVANCE(66);
      END_STATE();
    case 17:
      if (lookahead == 'i') ADVANCE(30);
      END_STATE();
    case 18:
      if (lookahead == 'l') ADVANCE(11);
      END_STATE();
    case 19:
      if (lookahead == 'l') ADVANCE(55);
      if (lookahead == 'r') ADVANCE(57);
      END_STATE();
    case 20:
      if (lookahead == 'm') ADVANCE(24);
      if (lookahead == 'x') ADVANCE(26);
      END_STATE();
    case 21:
      if (lookahead == 'm') ADVANCE(42);
      END_STATE();
    case 22:
      if (lookahead == 'n') ADVANCE(17);
      END_STATE();
    case 23:
      if (lookahead == 'o') ADVANCE(18);
      END_STATE();
    case 24:
      if (lookahead == 'p') ADVANCE(29);
      END_STATE();
    case 25:
      if (lookahead == 'p') ADVANCE(15);
      END_STATE();
    case 26:
      if (lookahead == 'p') ADVANCE(28);
      END_STATE();
    case 27:
      if (lookahead == 'r') ADVANCE(21);
      END_STATE();
    case 28:
      if (lookahead == 'r') ADVANCE(40);
      END_STATE();
    case 29:
      if (lookahead == 't') ADVANCE(32);
      END_STATE();
    case 30:
      if (lookahead == 't') ADVANCE(53);
      END_STATE();
    case 31:
      if (lookahead == 'x') ADVANCE(26);
      END_STATE();
    case 32:
      if (lookahead == 'y') ADVANCE(70);
      END_STATE();
    case 33:
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(33)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 34:
      if (eof) ADVANCE(36);
      if (lookahead == ')') ADVANCE(52);
      if (lookahead == '*') ADVANCE(46);
      if (lookahead == '+') ADVANCE(45);
      if (lookahead == '-') ADVANCE(5);
      if (lookahead == '=') ADVANCE(38);
      if (lookahead == ']') ADVANCE(64);
      if (lookahead == 'e') ADVANCE(31);
      if (lookahead == 't') ADVANCE(14);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(34)
      END_STATE();
    case 35:
      if (eof) ADVANCE(36);
      if (lookahead == ')') ADVANCE(52);
      if (lookahead == ',') ADVANCE(59);
      if (lookahead == ';') ADVANCE(68);
      if (lookahead == '=') ADVANCE(6);
      if (lookahead == '@') ADVANCE(74);
      if (lookahead == 'e') ADVANCE(31);
      if (lookahead == 't') ADVANCE(14);
      if (lookahead == '|') ADVANCE(65);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(35)
      END_STATE();
    case 36:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(anon_sym_type);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(anon_sym_EQ);
      if (lookahead == '>') ADVANCE(60);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(anon_sym_expr);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(anon_sym_term);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(anon_sym_I);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(anon_sym_I);
      if (lookahead == '\'') ADVANCE(2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(anon_sym_DASH_GT);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(anon_sym_rec);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(anon_sym_rec);
      if (lookahead == '\'') ADVANCE(2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(anon_sym_unit);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(anon_sym_unit);
      if (lookahead == '\'') ADVANCE(2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(anon_sym_inl);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(anon_sym_inl);
      if (lookahead == '\'') ADVANCE(2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(anon_sym_inr);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(anon_sym_inr);
      if (lookahead == '\'') ADVANCE(2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(anon_sym_EQ_GT);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(anon_sym_fold);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(anon_sym_fold);
      if (lookahead == '\'') ADVANCE(2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(anon_sym_LBRACK);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(anon_sym_PIPE);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(anon_sym_trace);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(anon_sym_trace);
      if (lookahead == '\'') ADVANCE(2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(anon_sym_TILDE);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(anon_sym_empty);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(anon_sym_empty);
      if (lookahead == '\'') ADVANCE(2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(anon_sym_id);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(anon_sym_id);
      if (lookahead == '\'') ADVANCE(2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(anon_sym_AT);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'a') ADVANCE(76);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'c') ADVANCE(80);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'c') ADVANCE(49);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'd') ADVANCE(73);
      if (lookahead == 'n') ADVANCE(84);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'd') ADVANCE(62);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'e') ADVANCE(67);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'e') ADVANCE(77);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'i') ADVANCE(91);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'l') ADVANCE(79);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'l') ADVANCE(56);
      if (lookahead == 'r') ADVANCE(58);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 85:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'm') ADVANCE(88);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 86:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'n') ADVANCE(82);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 87:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'o') ADVANCE(83);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'p') ADVANCE(90);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'r') ADVANCE(75);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 90:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 't') ADVANCE(92);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 91:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 't') ADVANCE(54);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 92:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'y') ADVANCE(71);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    case 93:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(93);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 3},
  [3] = {.lex_state = 3},
  [4] = {.lex_state = 3},
  [5] = {.lex_state = 3},
  [6] = {.lex_state = 3},
  [7] = {.lex_state = 3},
  [8] = {.lex_state = 3},
  [9] = {.lex_state = 3},
  [10] = {.lex_state = 3},
  [11] = {.lex_state = 3},
  [12] = {.lex_state = 3},
  [13] = {.lex_state = 35},
  [14] = {.lex_state = 34},
  [15] = {.lex_state = 35},
  [16] = {.lex_state = 34},
  [17] = {.lex_state = 35},
  [18] = {.lex_state = 35},
  [19] = {.lex_state = 35},
  [20] = {.lex_state = 35},
  [21] = {.lex_state = 35},
  [22] = {.lex_state = 35},
  [23] = {.lex_state = 34},
  [24] = {.lex_state = 35},
  [25] = {.lex_state = 34},
  [26] = {.lex_state = 34},
  [27] = {.lex_state = 34},
  [28] = {.lex_state = 35},
  [29] = {.lex_state = 35},
  [30] = {.lex_state = 0},
  [31] = {.lex_state = 0},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 4},
  [34] = {.lex_state = 4},
  [35] = {.lex_state = 4},
  [36] = {.lex_state = 4},
  [37] = {.lex_state = 4},
  [38] = {.lex_state = 4},
  [39] = {.lex_state = 0},
  [40] = {.lex_state = 4},
  [41] = {.lex_state = 4},
  [42] = {.lex_state = 3},
  [43] = {.lex_state = 0},
  [44] = {.lex_state = 4},
  [45] = {.lex_state = 3},
  [46] = {.lex_state = 0},
  [47] = {.lex_state = 0},
  [48] = {.lex_state = 0},
  [49] = {.lex_state = 34},
  [50] = {.lex_state = 34},
  [51] = {.lex_state = 0},
  [52] = {.lex_state = 0},
  [53] = {.lex_state = 0},
  [54] = {.lex_state = 34},
  [55] = {.lex_state = 0},
  [56] = {.lex_state = 33},
  [57] = {.lex_state = 0},
  [58] = {.lex_state = 0},
  [59] = {.lex_state = 33},
  [60] = {.lex_state = 33},
  [61] = {.lex_state = 33},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_type] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_expr] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_term] = ACTIONS(1),
    [anon_sym_I] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_STAR] = ACTIONS(1),
    [anon_sym_DASH_GT] = ACTIONS(1),
    [anon_sym_rec] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_unit] = ACTIONS(1),
    [anon_sym_inl] = ACTIONS(1),
    [anon_sym_inr] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_EQ_GT] = ACTIONS(1),
    [anon_sym_fold] = ACTIONS(1),
    [anon_sym_LBRACK] = ACTIONS(1),
    [anon_sym_RBRACK] = ACTIONS(1),
    [anon_sym_PIPE] = ACTIONS(1),
    [anon_sym_trace] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_TILDE] = ACTIONS(1),
    [anon_sym_empty] = ACTIONS(1),
    [anon_sym_id] = ACTIONS(1),
    [anon_sym_AT] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(55),
    [sym_definition] = STATE(43),
    [aux_sym_source_file_repeat1] = STATE(43),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_type] = ACTIONS(5),
    [anon_sym_expr] = ACTIONS(7),
    [anon_sym_term] = ACTIONS(9),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 9,
    ACTIONS(11), 1,
      anon_sym_LPAREN,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    STATE(24), 1,
      sym__parenthesis_term,
    STATE(28), 1,
      sym_term,
    STATE(31), 1,
      sym__parenthesis_expr,
    STATE(46), 1,
      sym_expression,
    ACTIONS(15), 2,
      anon_sym_inl,
      anon_sym_inr,
    ACTIONS(17), 2,
      anon_sym_fold,
      anon_sym_trace,
    ACTIONS(13), 4,
      anon_sym_unit,
      anon_sym_empty,
      anon_sym_id,
      sym_identifier,
  [33] = 9,
    ACTIONS(11), 1,
      anon_sym_LPAREN,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    STATE(24), 1,
      sym__parenthesis_term,
    STATE(31), 1,
      sym__parenthesis_expr,
    STATE(42), 1,
      sym_term,
    STATE(51), 1,
      sym_expression,
    ACTIONS(15), 2,
      anon_sym_inl,
      anon_sym_inr,
    ACTIONS(17), 2,
      anon_sym_fold,
      anon_sym_trace,
    ACTIONS(13), 4,
      anon_sym_unit,
      anon_sym_empty,
      anon_sym_id,
      sym_identifier,
  [66] = 7,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_LPAREN,
    STATE(15), 1,
      sym_term,
    STATE(24), 1,
      sym__parenthesis_term,
    ACTIONS(15), 2,
      anon_sym_inl,
      anon_sym_inr,
    ACTIONS(17), 2,
      anon_sym_fold,
      anon_sym_trace,
    ACTIONS(13), 4,
      anon_sym_unit,
      anon_sym_empty,
      anon_sym_id,
      sym_identifier,
  [93] = 7,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_LPAREN,
    STATE(17), 1,
      sym_term,
    STATE(24), 1,
      sym__parenthesis_term,
    ACTIONS(15), 2,
      anon_sym_inl,
      anon_sym_inr,
    ACTIONS(17), 2,
      anon_sym_fold,
      anon_sym_trace,
    ACTIONS(13), 4,
      anon_sym_unit,
      anon_sym_empty,
      anon_sym_id,
      sym_identifier,
  [120] = 7,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_LPAREN,
    STATE(18), 1,
      sym_term,
    STATE(24), 1,
      sym__parenthesis_term,
    ACTIONS(15), 2,
      anon_sym_inl,
      anon_sym_inr,
    ACTIONS(17), 2,
      anon_sym_fold,
      anon_sym_trace,
    ACTIONS(13), 4,
      anon_sym_unit,
      anon_sym_empty,
      anon_sym_id,
      sym_identifier,
  [147] = 7,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_LPAREN,
    STATE(19), 1,
      sym_term,
    STATE(24), 1,
      sym__parenthesis_term,
    ACTIONS(15), 2,
      anon_sym_inl,
      anon_sym_inr,
    ACTIONS(17), 2,
      anon_sym_fold,
      anon_sym_trace,
    ACTIONS(13), 4,
      anon_sym_unit,
      anon_sym_empty,
      anon_sym_id,
      sym_identifier,
  [174] = 7,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_LPAREN,
    STATE(20), 1,
      sym_term,
    STATE(24), 1,
      sym__parenthesis_term,
    ACTIONS(15), 2,
      anon_sym_inl,
      anon_sym_inr,
    ACTIONS(17), 2,
      anon_sym_fold,
      anon_sym_trace,
    ACTIONS(13), 4,
      anon_sym_unit,
      anon_sym_empty,
      anon_sym_id,
      sym_identifier,
  [201] = 7,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_LPAREN,
    STATE(21), 1,
      sym_term,
    STATE(24), 1,
      sym__parenthesis_term,
    ACTIONS(15), 2,
      anon_sym_inl,
      anon_sym_inr,
    ACTIONS(17), 2,
      anon_sym_fold,
      anon_sym_trace,
    ACTIONS(13), 4,
      anon_sym_unit,
      anon_sym_empty,
      anon_sym_id,
      sym_identifier,
  [228] = 7,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_LPAREN,
    STATE(24), 1,
      sym__parenthesis_term,
    STATE(45), 1,
      sym_term,
    ACTIONS(15), 2,
      anon_sym_inl,
      anon_sym_inr,
    ACTIONS(17), 2,
      anon_sym_fold,
      anon_sym_trace,
    ACTIONS(13), 4,
      anon_sym_unit,
      anon_sym_empty,
      anon_sym_id,
      sym_identifier,
  [255] = 7,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_LPAREN,
    STATE(13), 1,
      sym_term,
    STATE(24), 1,
      sym__parenthesis_term,
    ACTIONS(15), 2,
      anon_sym_inl,
      anon_sym_inr,
    ACTIONS(17), 2,
      anon_sym_fold,
      anon_sym_trace,
    ACTIONS(13), 4,
      anon_sym_unit,
      anon_sym_empty,
      anon_sym_id,
      sym_identifier,
  [282] = 7,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_LPAREN,
    STATE(24), 1,
      sym__parenthesis_term,
    STATE(29), 1,
      sym_term,
    ACTIONS(15), 2,
      anon_sym_inl,
      anon_sym_inr,
    ACTIONS(17), 2,
      anon_sym_fold,
      anon_sym_trace,
    ACTIONS(13), 4,
      anon_sym_unit,
      anon_sym_empty,
      anon_sym_id,
      sym_identifier,
  [309] = 5,
    ACTIONS(25), 1,
      anon_sym_COMMA,
    ACTIONS(27), 1,
      anon_sym_EQ_GT,
    ACTIONS(29), 1,
      anon_sym_PIPE,
    ACTIONS(31), 1,
      anon_sym_SEMI,
    ACTIONS(23), 6,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_RPAREN,
      anon_sym_AT,
  [330] = 3,
    ACTIONS(35), 1,
      anon_sym_PLUS,
    ACTIONS(37), 1,
      anon_sym_STAR,
    ACTIONS(33), 8,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_EQ,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_DASH_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
  [347] = 5,
    ACTIONS(25), 1,
      anon_sym_COMMA,
    ACTIONS(27), 1,
      anon_sym_EQ_GT,
    ACTIONS(29), 1,
      anon_sym_PIPE,
    ACTIONS(31), 1,
      anon_sym_SEMI,
    ACTIONS(39), 6,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_RPAREN,
      anon_sym_AT,
  [368] = 1,
    ACTIONS(41), 10,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_EQ,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_PLUS,
      anon_sym_STAR,
      anon_sym_DASH_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
  [381] = 5,
    ACTIONS(25), 1,
      anon_sym_COMMA,
    ACTIONS(27), 1,
      anon_sym_EQ_GT,
    ACTIONS(29), 1,
      anon_sym_PIPE,
    ACTIONS(31), 1,
      anon_sym_SEMI,
    ACTIONS(43), 6,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_RPAREN,
      anon_sym_AT,
  [402] = 4,
    ACTIONS(25), 1,
      anon_sym_COMMA,
    ACTIONS(27), 1,
      anon_sym_EQ_GT,
    ACTIONS(29), 1,
      anon_sym_PIPE,
    ACTIONS(45), 7,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_RPAREN,
      anon_sym_SEMI,
      anon_sym_AT,
  [421] = 3,
    ACTIONS(25), 1,
      anon_sym_COMMA,
    ACTIONS(27), 1,
      anon_sym_EQ_GT,
    ACTIONS(45), 8,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_RPAREN,
      anon_sym_PIPE,
      anon_sym_SEMI,
      anon_sym_AT,
  [438] = 2,
    ACTIONS(25), 1,
      anon_sym_COMMA,
    ACTIONS(45), 9,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_RPAREN,
      anon_sym_EQ_GT,
      anon_sym_PIPE,
      anon_sym_SEMI,
      anon_sym_AT,
  [453] = 1,
    ACTIONS(45), 10,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_EQ_GT,
      anon_sym_PIPE,
      anon_sym_SEMI,
      anon_sym_AT,
  [466] = 1,
    ACTIONS(47), 10,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_EQ_GT,
      anon_sym_PIPE,
      anon_sym_SEMI,
      anon_sym_AT,
  [479] = 4,
    ACTIONS(35), 1,
      anon_sym_PLUS,
    ACTIONS(37), 1,
      anon_sym_STAR,
    ACTIONS(51), 1,
      anon_sym_DASH_GT,
    ACTIONS(49), 7,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_EQ,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
  [498] = 1,
    ACTIONS(53), 10,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_RPAREN,
      anon_sym_COMMA,
      anon_sym_EQ_GT,
      anon_sym_PIPE,
      anon_sym_SEMI,
      anon_sym_AT,
  [511] = 1,
    ACTIONS(55), 10,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_EQ,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_PLUS,
      anon_sym_STAR,
      anon_sym_DASH_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
  [524] = 2,
    ACTIONS(37), 1,
      anon_sym_STAR,
    ACTIONS(33), 9,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_EQ,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_PLUS,
      anon_sym_DASH_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
  [539] = 1,
    ACTIONS(33), 10,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_EQ,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_PLUS,
      anon_sym_STAR,
      anon_sym_DASH_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
  [552] = 5,
    ACTIONS(25), 1,
      anon_sym_COMMA,
    ACTIONS(27), 1,
      anon_sym_EQ_GT,
    ACTIONS(29), 1,
      anon_sym_PIPE,
    ACTIONS(31), 1,
      anon_sym_SEMI,
    ACTIONS(57), 5,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_AT,
  [572] = 5,
    ACTIONS(25), 1,
      anon_sym_COMMA,
    ACTIONS(27), 1,
      anon_sym_EQ_GT,
    ACTIONS(29), 1,
      anon_sym_PIPE,
    ACTIONS(31), 1,
      anon_sym_SEMI,
    ACTIONS(59), 4,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_expr,
      anon_sym_term,
  [591] = 4,
    ACTIONS(35), 1,
      anon_sym_PLUS,
    ACTIONS(37), 1,
      anon_sym_STAR,
    ACTIONS(51), 1,
      anon_sym_DASH_GT,
    ACTIONS(61), 4,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_expr,
      anon_sym_term,
  [607] = 1,
    ACTIONS(57), 6,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_RPAREN,
      anon_sym_AT,
  [616] = 1,
    ACTIONS(63), 6,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_expr,
      anon_sym_term,
      anon_sym_RPAREN,
      anon_sym_AT,
  [625] = 5,
    ACTIONS(67), 1,
      anon_sym_rec,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    STATE(16), 1,
      sym__parenthesis_type,
    STATE(23), 1,
      sym_type,
    ACTIONS(65), 2,
      anon_sym_I,
      sym_identifier,
  [642] = 5,
    ACTIONS(67), 1,
      anon_sym_rec,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    STATE(16), 1,
      sym__parenthesis_type,
    STATE(48), 1,
      sym_type,
    ACTIONS(65), 2,
      anon_sym_I,
      sym_identifier,
  [659] = 5,
    ACTIONS(67), 1,
      anon_sym_rec,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    STATE(16), 1,
      sym__parenthesis_type,
    STATE(30), 1,
      sym_type,
    ACTIONS(65), 2,
      anon_sym_I,
      sym_identifier,
  [676] = 5,
    ACTIONS(67), 1,
      anon_sym_rec,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    STATE(14), 1,
      sym_type,
    STATE(16), 1,
      sym__parenthesis_type,
    ACTIONS(65), 2,
      anon_sym_I,
      sym_identifier,
  [693] = 5,
    ACTIONS(67), 1,
      anon_sym_rec,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    STATE(16), 1,
      sym__parenthesis_type,
    STATE(26), 1,
      sym_type,
    ACTIONS(65), 2,
      anon_sym_I,
      sym_identifier,
  [710] = 5,
    ACTIONS(67), 1,
      anon_sym_rec,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    STATE(16), 1,
      sym__parenthesis_type,
    STATE(50), 1,
      sym_type,
    ACTIONS(65), 2,
      anon_sym_I,
      sym_identifier,
  [727] = 5,
    ACTIONS(71), 1,
      ts_builtin_sym_end,
    ACTIONS(73), 1,
      anon_sym_type,
    ACTIONS(76), 1,
      anon_sym_expr,
    ACTIONS(79), 1,
      anon_sym_term,
    STATE(39), 2,
      sym_definition,
      aux_sym_source_file_repeat1,
  [744] = 5,
    ACTIONS(67), 1,
      anon_sym_rec,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    STATE(16), 1,
      sym__parenthesis_type,
    STATE(49), 1,
      sym_type,
    ACTIONS(65), 2,
      anon_sym_I,
      sym_identifier,
  [761] = 5,
    ACTIONS(67), 1,
      anon_sym_rec,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    STATE(16), 1,
      sym__parenthesis_type,
    STATE(27), 1,
      sym_type,
    ACTIONS(65), 2,
      anon_sym_I,
      sym_identifier,
  [778] = 6,
    ACTIONS(25), 1,
      anon_sym_COMMA,
    ACTIONS(27), 1,
      anon_sym_EQ_GT,
    ACTIONS(29), 1,
      anon_sym_PIPE,
    ACTIONS(31), 1,
      anon_sym_SEMI,
    ACTIONS(57), 1,
      anon_sym_AT,
    ACTIONS(82), 1,
      anon_sym_RPAREN,
  [797] = 5,
    ACTIONS(5), 1,
      anon_sym_type,
    ACTIONS(7), 1,
      anon_sym_expr,
    ACTIONS(9), 1,
      anon_sym_term,
    ACTIONS(84), 1,
      ts_builtin_sym_end,
    STATE(39), 2,
      sym_definition,
      aux_sym_source_file_repeat1,
  [814] = 5,
    ACTIONS(67), 1,
      anon_sym_rec,
    ACTIONS(69), 1,
      anon_sym_LPAREN,
    STATE(16), 1,
      sym__parenthesis_type,
    STATE(47), 1,
      sym_type,
    ACTIONS(65), 2,
      anon_sym_I,
      sym_identifier,
  [831] = 5,
    ACTIONS(25), 1,
      anon_sym_COMMA,
    ACTIONS(27), 1,
      anon_sym_EQ_GT,
    ACTIONS(29), 1,
      anon_sym_PIPE,
    ACTIONS(31), 1,
      anon_sym_SEMI,
    ACTIONS(82), 1,
      anon_sym_RPAREN,
  [847] = 2,
    ACTIONS(86), 1,
      anon_sym_AT,
    ACTIONS(59), 4,
      ts_builtin_sym_end,
      anon_sym_type,
      anon_sym_expr,
      anon_sym_term,
  [857] = 4,
    ACTIONS(35), 1,
      anon_sym_PLUS,
    ACTIONS(37), 1,
      anon_sym_STAR,
    ACTIONS(51), 1,
      anon_sym_DASH_GT,
    ACTIONS(88), 1,
      anon_sym_RBRACK,
  [870] = 4,
    ACTIONS(35), 1,
      anon_sym_PLUS,
    ACTIONS(37), 1,
      anon_sym_STAR,
    ACTIONS(51), 1,
      anon_sym_DASH_GT,
    ACTIONS(90), 1,
      anon_sym_RPAREN,
  [883] = 4,
    ACTIONS(35), 1,
      anon_sym_PLUS,
    ACTIONS(37), 1,
      anon_sym_STAR,
    ACTIONS(51), 1,
      anon_sym_DASH_GT,
    ACTIONS(92), 1,
      anon_sym_EQ,
  [896] = 4,
    ACTIONS(35), 1,
      anon_sym_PLUS,
    ACTIONS(37), 1,
      anon_sym_STAR,
    ACTIONS(51), 1,
      anon_sym_DASH_GT,
    ACTIONS(94), 1,
      anon_sym_EQ,
  [909] = 2,
    ACTIONS(86), 1,
      anon_sym_AT,
    ACTIONS(96), 1,
      anon_sym_RPAREN,
  [916] = 1,
    ACTIONS(98), 1,
      anon_sym_COLON,
  [920] = 1,
    ACTIONS(100), 1,
      anon_sym_COLON,
  [924] = 1,
    ACTIONS(102), 1,
      anon_sym_EQ,
  [928] = 1,
    ACTIONS(104), 1,
      ts_builtin_sym_end,
  [932] = 1,
    ACTIONS(106), 1,
      sym_identifier,
  [936] = 1,
    ACTIONS(108), 1,
      anon_sym_LBRACK,
  [940] = 1,
    ACTIONS(110), 1,
      anon_sym_DOT,
  [944] = 1,
    ACTIONS(112), 1,
      sym_identifier,
  [948] = 1,
    ACTIONS(114), 1,
      sym_identifier,
  [952] = 1,
    ACTIONS(116), 1,
      sym_identifier,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 33,
  [SMALL_STATE(4)] = 66,
  [SMALL_STATE(5)] = 93,
  [SMALL_STATE(6)] = 120,
  [SMALL_STATE(7)] = 147,
  [SMALL_STATE(8)] = 174,
  [SMALL_STATE(9)] = 201,
  [SMALL_STATE(10)] = 228,
  [SMALL_STATE(11)] = 255,
  [SMALL_STATE(12)] = 282,
  [SMALL_STATE(13)] = 309,
  [SMALL_STATE(14)] = 330,
  [SMALL_STATE(15)] = 347,
  [SMALL_STATE(16)] = 368,
  [SMALL_STATE(17)] = 381,
  [SMALL_STATE(18)] = 402,
  [SMALL_STATE(19)] = 421,
  [SMALL_STATE(20)] = 438,
  [SMALL_STATE(21)] = 453,
  [SMALL_STATE(22)] = 466,
  [SMALL_STATE(23)] = 479,
  [SMALL_STATE(24)] = 498,
  [SMALL_STATE(25)] = 511,
  [SMALL_STATE(26)] = 524,
  [SMALL_STATE(27)] = 539,
  [SMALL_STATE(28)] = 552,
  [SMALL_STATE(29)] = 572,
  [SMALL_STATE(30)] = 591,
  [SMALL_STATE(31)] = 607,
  [SMALL_STATE(32)] = 616,
  [SMALL_STATE(33)] = 625,
  [SMALL_STATE(34)] = 642,
  [SMALL_STATE(35)] = 659,
  [SMALL_STATE(36)] = 676,
  [SMALL_STATE(37)] = 693,
  [SMALL_STATE(38)] = 710,
  [SMALL_STATE(39)] = 727,
  [SMALL_STATE(40)] = 744,
  [SMALL_STATE(41)] = 761,
  [SMALL_STATE(42)] = 778,
  [SMALL_STATE(43)] = 797,
  [SMALL_STATE(44)] = 814,
  [SMALL_STATE(45)] = 831,
  [SMALL_STATE(46)] = 847,
  [SMALL_STATE(47)] = 857,
  [SMALL_STATE(48)] = 870,
  [SMALL_STATE(49)] = 883,
  [SMALL_STATE(50)] = 896,
  [SMALL_STATE(51)] = 909,
  [SMALL_STATE(52)] = 916,
  [SMALL_STATE(53)] = 920,
  [SMALL_STATE(54)] = 924,
  [SMALL_STATE(55)] = 928,
  [SMALL_STATE(56)] = 932,
  [SMALL_STATE(57)] = 936,
  [SMALL_STATE(58)] = 940,
  [SMALL_STATE(59)] = 944,
  [SMALL_STATE(60)] = 948,
  [SMALL_STATE(61)] = 952,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [7] = {.entry = {.count = 1, .reusable = true}}, SHIFT(60),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [15] = {.entry = {.count = 1, .reusable = false}}, SHIFT(11),
  [17] = {.entry = {.count = 1, .reusable = false}}, SHIFT(57),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [23] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_term, 2),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [33] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type, 3),
  [35] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [37] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [39] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_term, 5),
  [41] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type, 1),
  [43] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression, 3),
  [45] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_term, 3),
  [47] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__parenthesis_term, 3),
  [49] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type, 4),
  [51] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [53] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_term, 1),
  [55] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__parenthesis_type, 3),
  [57] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_expression, 1),
  [59] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_definition, 6),
  [61] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_definition, 4),
  [63] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__parenthesis_expr, 3),
  [65] = {.entry = {.count = 1, .reusable = false}}, SHIFT(16),
  [67] = {.entry = {.count = 1, .reusable = false}}, SHIFT(61),
  [69] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [71] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [73] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(59),
  [76] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(60),
  [79] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(56),
  [82] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [84] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [86] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [88] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [90] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [92] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [96] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [98] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [100] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [102] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [104] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [106] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [108] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [110] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [112] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [114] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [116] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_omnirev(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
