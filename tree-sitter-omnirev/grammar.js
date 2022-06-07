module.exports = grammar({
  name: 'omnirev',

  rules: {
    source_file: $ => $.definition,

    definition: $ => choice(
      seq('type', $.identifier, '=', $.type),
      seq('expr', $.identifier, ':', $.type, '=', $.expression),
      seq('term', $.identifier, ':', $.type, '=', $.term),
    ),

    type: $ => choice(
      $.identifier,
      'I',
      prec. left(2, seq($.type, '+', $.type)),
      prec. left(3, seq($.type, '*', $.type)),
      prec. left(1, seq($.type, '->', $.type)),
      seq('rec', $.identifier, '.', $.type),
    ),

    term: $ => choice(
      $.identifier,
      'unit',
      seq('inl', $.term),
      seq('inr', $.term),
      prec. left(4, seq($.term, ',', $.term)),
      prec. left(3, seq($.term, '=>', $.term)),
      seq('fold', '[', $.type, ']', $.term),
      prec. left(2, seq($.term, '|', $.term)),
      seq('trace', '[', $.type, ']', $.term),
      prec. left(1, seq($.term, ';', $.term)),
      seq('~', $.term),
      'empty',
      'id',
    ),

    expression: $ => choice(
      $.term,
      seq($.expression, '@', $.term),
    ),

    identifier: $ => /[a-zA-Z](\w|'_'|'\'')*/
  }
});