# constFold
Implementation of constant folding in R6RS scheme.

Constant folding is a strength reduction compiler optimization that recognizes and evaluates constant expressions during
compliation instead of at runtime.

This is an implementation of constant folding in Chez Scheme, something I did as an exercise during a job interview a few years
ago. At the time, I used Petite Chez, the interpreter which was freely available. More recently, the Chez Scheme compiler has been
made open-source, so this example can be compiled now.

Example of constant folding:

{asgn_expr {var RES} {add {add {add {var A} {add {const 1} {const 4}}} {var A}} {add {add {const 18} {const 4}} {const 6}}}}
reduces to
{asgn_expr {var RES} {add {add {add {var A} {const 5}} {var A}} {const 28}}}

In this example, grammar for expressions is as follows:

S     -->    asgn_exp var-decl exp
exp  -->    { add exp exp }
               | var-decl
               | { const num }
var-decl --> { var id }
id -->   Scheme identifier
num --> Scheme number


