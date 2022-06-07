#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 13
#define STATE_COUNT 19
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 28
#define ALIAS_COUNT 0
#define TOKEN_COUNT 25
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 0
#define MAX_ALIAS_SEQUENCE_LENGTH 4
#define PRODUCTION_ID_COUNT 1

enum {
  anon_sym_type = 1,
  anon_sym_EQ = 2,
  anon_sym_I = 3,
  anon_sym_PLUS = 4,
  anon_sym_STAR = 5,
  anon_sym_DASH_GT = 6,
  anon_sym_rec = 7,
  anon_sym_DOT = 8,
  anon_sym_unit = 9,
  anon_sym_inl = 10,
  anon_sym_inr = 11,
  anon_sym_COMMA = 12,
  anon_sym_EQ_GT = 13,
  anon_sym_fold = 14,
  anon_sym_LBRACK = 15,
  anon_sym_RBRACK = 16,
  anon_sym_PIPE = 17,
  anon_sym_trace = 18,
  anon_sym_SEMI = 19,
  anon_sym_TILDE = 20,
  anon_sym_empty = 21,
  anon_sym_id = 22,
  anon_sym_AT = 23,
  sym_identifier = 24,
  sym_source_file = 25,
  sym_definition = 26,
  sym_type = 27,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_type] = "type",
  [anon_sym_EQ] = "=",
  [anon_sym_I] = "I",
  [anon_sym_PLUS] = "+",
  [anon_sym_STAR] = "*",
  [anon_sym_DASH_GT] = "->",
  [anon_sym_rec] = "rec",
  [anon_sym_DOT] = ".",
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
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_type] = anon_sym_type,
  [anon_sym_EQ] = anon_sym_EQ,
  [anon_sym_I] = anon_sym_I,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_STAR] = anon_sym_STAR,
  [anon_sym_DASH_GT] = anon_sym_DASH_GT,
  [anon_sym_rec] = anon_sym_rec,
  [anon_sym_DOT] = anon_sym_DOT,
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
      if (eof) ADVANCE(26);
      if (lookahead == '*') ADVANCE(33);
      if (lookahead == '+') ADVANCE(32);
      if (lookahead == ',') ADVANCE(41);
      if (lookahead == '-') ADVANCE(4);
      if (lookahead == '.') ADVANCE(37);
      if (lookahead == ';') ADVANCE(48);
      if (lookahead == '=') ADVANCE(29);
      if (lookahead == '@') ADVANCE(52);
      if (lookahead == 'I') ADVANCE(30);
      if (lookahead == '[') ADVANCE(44);
      if (lookahead == ']') ADVANCE(45);
      if (lookahead == 'e') ADVANCE(16);
      if (lookahead == 'f') ADVANCE(18);
      if (lookahead == 'i') ADVANCE(8);
      if (lookahead == 'r') ADVANCE(10);
      if (lookahead == 't') ADVANCE(21);
      if (lookahead == 'u') ADVANCE(17);
      if (lookahead == '|') ADVANCE(46);
      if (lookahead == '~') ADVANCE(49);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      END_STATE();
    case 1:
      if (lookahead == '\'') ADVANCE(55);
      END_STATE();
    case 2:
      if (lookahead == '\'') ADVANCE(1);
      if (lookahead == '_') ADVANCE(1);
      END_STATE();
    case 3:
      if (lookahead == '=') ADVANCE(28);
      if (lookahead == 'I') ADVANCE(31);
      if (lookahead == 'r') ADVANCE(54);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(3)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 4:
      if (lookahead == '>') ADVANCE(34);
      END_STATE();
    case 5:
      if (lookahead == 'a') ADVANCE(7);
      END_STATE();
    case 6:
      if (lookahead == 'c') ADVANCE(35);
      END_STATE();
    case 7:
      if (lookahead == 'c') ADVANCE(12);
      END_STATE();
    case 8:
      if (lookahead == 'd') ADVANCE(51);
      if (lookahead == 'n') ADVANCE(15);
      END_STATE();
    case 9:
      if (lookahead == 'd') ADVANCE(43);
      END_STATE();
    case 10:
      if (lookahead == 'e') ADVANCE(6);
      END_STATE();
    case 11:
      if (lookahead == 'e') ADVANCE(27);
      END_STATE();
    case 12:
      if (lookahead == 'e') ADVANCE(47);
      END_STATE();
    case 13:
      if (lookahead == 'i') ADVANCE(23);
      END_STATE();
    case 14:
      if (lookahead == 'l') ADVANCE(9);
      END_STATE();
    case 15:
      if (lookahead == 'l') ADVANCE(39);
      if (lookahead == 'r') ADVANCE(40);
      END_STATE();
    case 16:
      if (lookahead == 'm') ADVANCE(19);
      END_STATE();
    case 17:
      if (lookahead == 'n') ADVANCE(13);
      END_STATE();
    case 18:
      if (lookahead == 'o') ADVANCE(14);
      END_STATE();
    case 19:
      if (lookahead == 'p') ADVANCE(22);
      END_STATE();
    case 20:
      if (lookahead == 'p') ADVANCE(11);
      END_STATE();
    case 21:
      if (lookahead == 'r') ADVANCE(5);
      if (lookahead == 'y') ADVANCE(20);
      END_STATE();
    case 22:
      if (lookahead == 't') ADVANCE(24);
      END_STATE();
    case 23:
      if (lookahead == 't') ADVANCE(38);
      END_STATE();
    case 24:
      if (lookahead == 'y') ADVANCE(50);
      END_STATE();
    case 25:
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(25)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 26:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 27:
      ACCEPT_TOKEN(anon_sym_type);
      END_STATE();
    case 28:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 29:
      ACCEPT_TOKEN(anon_sym_EQ);
      if (lookahead == '>') ADVANCE(42);
      END_STATE();
    case 30:
      ACCEPT_TOKEN(anon_sym_I);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(anon_sym_I);
      if (lookahead == '\'') ADVANCE(2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(anon_sym_DASH_GT);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(anon_sym_rec);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(anon_sym_rec);
      if (lookahead == '\'') ADVANCE(2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_unit);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(anon_sym_inl);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(anon_sym_inr);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(anon_sym_EQ_GT);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(anon_sym_fold);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(anon_sym_LBRACK);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(anon_sym_PIPE);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(anon_sym_trace);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(anon_sym_TILDE);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(anon_sym_empty);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(anon_sym_id);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(anon_sym_AT);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'c') ADVANCE(36);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (lookahead == 'e') ADVANCE(53);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == '\'') ADVANCE(2);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(55);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 3},
  [3] = {.lex_state = 0},
  [4] = {.lex_state = 0},
  [5] = {.lex_state = 3},
  [6] = {.lex_state = 3},
  [7] = {.lex_state = 3},
  [8] = {.lex_state = 3},
  [9] = {.lex_state = 0},
  [10] = {.lex_state = 0},
  [11] = {.lex_state = 0},
  [12] = {.lex_state = 0},
  [13] = {.lex_state = 25},
  [14] = {.lex_state = 0},
  [15] = {.lex_state = 0},
  [16] = {.lex_state = 3},
  [17] = {.lex_state = 25},
  [18] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_type] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_I] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_STAR] = ACTIONS(1),
    [anon_sym_DASH_GT] = ACTIONS(1),
    [anon_sym_rec] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
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
    [sym_source_file] = STATE(14),
    [sym_definition] = STATE(15),
    [anon_sym_type] = ACTIONS(3),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 3,
    ACTIONS(7), 1,
      anon_sym_rec,
    STATE(4), 1,
      sym_type,
    ACTIONS(5), 2,
      anon_sym_I,
      sym_identifier,
  [11] = 1,
    ACTIONS(9), 4,
      ts_builtin_sym_end,
      anon_sym_PLUS,
      anon_sym_STAR,
      anon_sym_DASH_GT,
  [18] = 4,
    ACTIONS(11), 1,
      ts_builtin_sym_end,
    ACTIONS(13), 1,
      anon_sym_PLUS,
    ACTIONS(15), 1,
      anon_sym_STAR,
    ACTIONS(17), 1,
      anon_sym_DASH_GT,
  [31] = 3,
    ACTIONS(7), 1,
      anon_sym_rec,
    STATE(9), 1,
      sym_type,
    ACTIONS(5), 2,
      anon_sym_I,
      sym_identifier,
  [42] = 3,
    ACTIONS(7), 1,
      anon_sym_rec,
    STATE(10), 1,
      sym_type,
    ACTIONS(5), 2,
      anon_sym_I,
      sym_identifier,
  [53] = 3,
    ACTIONS(7), 1,
      anon_sym_rec,
    STATE(11), 1,
      sym_type,
    ACTIONS(5), 2,
      anon_sym_I,
      sym_identifier,
  [64] = 3,
    ACTIONS(7), 1,
      anon_sym_rec,
    STATE(12), 1,
      sym_type,
    ACTIONS(5), 2,
      anon_sym_I,
      sym_identifier,
  [75] = 2,
    ACTIONS(15), 1,
      anon_sym_STAR,
    ACTIONS(19), 3,
      ts_builtin_sym_end,
      anon_sym_PLUS,
      anon_sym_DASH_GT,
  [84] = 1,
    ACTIONS(19), 4,
      ts_builtin_sym_end,
      anon_sym_PLUS,
      anon_sym_STAR,
      anon_sym_DASH_GT,
  [91] = 3,
    ACTIONS(13), 1,
      anon_sym_PLUS,
    ACTIONS(15), 1,
      anon_sym_STAR,
    ACTIONS(19), 2,
      ts_builtin_sym_end,
      anon_sym_DASH_GT,
  [102] = 4,
    ACTIONS(13), 1,
      anon_sym_PLUS,
    ACTIONS(15), 1,
      anon_sym_STAR,
    ACTIONS(17), 1,
      anon_sym_DASH_GT,
    ACTIONS(21), 1,
      ts_builtin_sym_end,
  [115] = 1,
    ACTIONS(23), 1,
      sym_identifier,
  [119] = 1,
    ACTIONS(25), 1,
      ts_builtin_sym_end,
  [123] = 1,
    ACTIONS(27), 1,
      ts_builtin_sym_end,
  [127] = 1,
    ACTIONS(29), 1,
      anon_sym_EQ,
  [131] = 1,
    ACTIONS(31), 1,
      sym_identifier,
  [135] = 1,
    ACTIONS(33), 1,
      anon_sym_DOT,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 11,
  [SMALL_STATE(4)] = 18,
  [SMALL_STATE(5)] = 31,
  [SMALL_STATE(6)] = 42,
  [SMALL_STATE(7)] = 53,
  [SMALL_STATE(8)] = 64,
  [SMALL_STATE(9)] = 75,
  [SMALL_STATE(10)] = 84,
  [SMALL_STATE(11)] = 91,
  [SMALL_STATE(12)] = 102,
  [SMALL_STATE(13)] = 115,
  [SMALL_STATE(14)] = 119,
  [SMALL_STATE(15)] = 123,
  [SMALL_STATE(16)] = 127,
  [SMALL_STATE(17)] = 131,
  [SMALL_STATE(18)] = 135,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [5] = {.entry = {.count = 1, .reusable = false}}, SHIFT(3),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(17),
  [9] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type, 1),
  [11] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_definition, 4),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [19] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type, 3),
  [21] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type, 4),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [25] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [27] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
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
