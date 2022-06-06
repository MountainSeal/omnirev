module.exports = grammar({
  name: 'omnirev',

  rules: {
    // TODO: add the actual grammar rules
    source_file: $ => repeat($._definition),

    // definition
    _definition: $ => choice(
      'type', $._identifier, '=', $._type,
      'expr', $._identifier, ':', $._type, '=', $._expression,
      'term', $._identifier, ':', $._type, '=', $._term
    ),

    // type
    _type: $ => choice(
      $._identifier,
      'I',
      // prec. left(or right)を使って結合度を設定
      choice($._type, '+', $._type),
      choice($._type, '*', $._type),
      choice($._type, '->', $._type),
    ),

    

    _term: $ => (

    ),

    _expression: $ => (

    ),

    _identifier: $ => /letter (letter | digit | '_' | '\'')*/
  }
});