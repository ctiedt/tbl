"extern" @keyword
"global" @keyword
"task" @keyword
"return" @keyword
"use" @keyword
"struct" @keyword
; "if" @keyword
; "else" @keyword
"schedule" @keyword
"let" @keyword
"uninit" @keyword
"every" @keyword
(task name: (ident) @function)
; (call name: (ident) @function)
(ident) @variable
(type) @type
(line_comment) @comment
(string) @string
(int) @numeric
