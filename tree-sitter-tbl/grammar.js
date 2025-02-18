function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)));
}

function commaSep(rule) {
  return optional(commaSep1(rule));
}

module.exports = grammar({
  name: "tbl",

  rules: {
    source_file: ($) => repeat(choice($.line_comment, $._decl)),

    line_comment: ($) => /\/\/[^\n]*\n/,

    _decl: ($) =>
      choice(
        $.extern_task,
        $.extern_global,
        $.use,
        $.task,
        $.global,
        $.struct,
        $.enum,
      ),

    extern_task: ($) =>
      seq(
        "extern",
        "task",
        $.ident,
        $.param_list,
        optional(seq("->", $.type)),
        ";",
      ),

    extern_global: ($) => seq("extern", "global", $.ident, ":", $.type, ";"),

    use: ($) => seq("use", $.ident, ";"),

    task: ($) =>
      seq(
        "task",
        field("name", $.ident),
        field("params", $.param_list),
        field("returns", optional(seq("->", $.type))),
        field("body", $.task_body),
      ),

    param_list: ($) => seq("(", commaSep(seq($.ident, ":", $.type)), ")"),
    param_ty_list: ($) => seq("(", commaSep(seq($.type)), ")"),

    task_body: ($) => seq("{", repeat($.stmt), "}"),

    global: ($) => seq("global", $.ident, ":", $.type, "=", $.expr, ";"),

    struct: ($) =>
      seq("struct", $.ident, "{", commaSep(seq($.ident, ":", $.type)), "}"),

    enum: ($) => seq("enum", $.ident, "{", /* todo */ "}"),

    expr: ($) =>
      choice($.literal, $.call, seq(choice("&", "*", "-"), $.expr), $.ident),

    call: ($) => seq(field("name", $.ident), "(", commaSep($.expr), ")"),

    stmt: ($) =>
      choice(
        seq("return", optional($.expr), ";"),
        seq("schedule", $.expr, "every", $.expr, ";"),
        $.let,
        $.uninit,
        seq($.expr, ";"),
      ),

    let: ($) => seq("let", $.ident, ":", $.type, "=", $.expr, ";"),
    uninit: ($) => seq("uninit", $.ident, ":", $.type, ";"),

    type: ($) =>
      choice(
        "bool",
        $.ident,
        /[iu](8|16|32|64)/,
        seq("[", $.type, ";", $.int, "]"),
        seq("&", $.type),
        seq("task", $.param_ty_list, optional(seq("->", $.type))),
      ),

    ident: ($) => /[A-Za-z_][A-Za-z0-9_]*/,

    literal: ($) => choice($.string, $.int),

    string: ($) => /"([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*"/,

    int: ($) => /\d+/,
  },
});
