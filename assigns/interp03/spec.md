# Mini-Project 3: Type Inference 

## CAS CS 320: Principles of Programming Languages

Due December 10, 2024 by 11:59PM

In this project, you'll be building yet another interpreter for a subset of OCaml. It'll be your task to implement the following functions (these are the only functions we will be testing):

```
\triangleright val unify : ty -> constr list -> ty_scheme option
\triangleright val type_of : stc_env -> expr -> ty_scheme option
\triangleright val eval_expr : dyn_env -> expr -> value
```

This signature appears (in part) in the file interp03/lib/lib.mli. The types used in the above signature appear in the module Utils. Your implementation of these functions should appear in the file interp03/lib/lib.ml. Please read the following instructions completely and carefully.

## Part 0: Parsing

For this project, you will be given the parser for the language. That said, you should still read through and grok what's going on in the grammar so that you know how to write test cases. It's very simliar to the grammar used in mini-project 2 (and we expect that you could have written it yourself) except that there are a few more constructs, and type annotations are optional.

A program in our language is given by the grammar in Figure 1. We present the operators and their associativity in order of increasing precedence in Figure 2. Note also that the function type arrow is rightassociative.

Finally, note that there is a desugaring process going on within the parser, so the targeted type expr does not match the grammar exactly ${ }^{1}$ Please make sure you understand how programs in the language correspond to expressions in expr, and how the given code for evaluating programs depends on your code for evaluating expressions.

## Part 1: Type Inference

We will be using a constraint-based inference system to describe the type inference procedure for our language. We write $\Gamma \vdash e: \tau \dashv \mathcal{C}$ to means that $e$ (expr) has type $\tau$ (ty) in the context $\Gamma$ (stc_env $=$ ty_scheme env) relative to the set of constraints $\mathcal{C}$ (const list). See the file lib/utils/utils.ml for more details.

[^0]```
<prog> ::= \{<toplet>\}
<toplet> ::= let [rec] <var> \{<arg>\}[<annot>] = <expr>
<annot> ::= : <ty>
    <arg> ::= <var> | ( <var> <annot>)
        <ty> ::= unit | int | float | bool |<ty> list | <ty> option | <tyvar>
            | <ty>* <ty> | <ty> -> <ty> |（<ty>）
<expr> ::= let [rec] <var> \{<arg>\} [<annot>] = <expr> in <expr>
            | if <expr> then <expr> else <expr>
            | fun <arg> \{<arg>\} -> <expr>
            | match <expr> with | <var>, <var> -> <expr>
            | match <expr> with | Some <var> -> <expr> | None -> <expr>
            | match <expr> with | <var> : : <var> -> <expr> | [] -> <expr>
            | <expr2>
<expr2> ::= <expr2> <bop> <expr2>
            | assert <expr3> | Some <expr3>
            | <expr3> \{<expr3>\}
<expr3> ::=（）|true|false|[]|None|[<expr> \{; <expr>\}]
            | <int> | <float> | <var>
            | ( <expr> [<annot>] )
<bop> $::=+|-|*| /|\bmod |+.|-.|* .|/| * *$.
```

![](https://cdn.mathpix.com/cropped/2024_12_10_05216d59f88a2b88d2adg-2.jpg?height=47&width=521&top_left_y=1364&top_left_x=645)
![](https://cdn.mathpix.com/cropped/2024_12_10_05216d59f88a2b88d2adg-2.jpg?height=58&width=199&top_left_y=1408&top_left_x=643)

```
    <int> ::= handled by lexer
<float> ::= handled by lexer
    <var> ::= handler by lexer
<tyvar> ::= handler by lexer
```

Figure 1: The grammar for our language

| Operators | Associativity |
| :---: | :---: |
| i। | $n / a$ |
| $\& \&$ | right |
| $<,<=,>,>=,=,<>$ | right |
| $@$ | left |
| $::$ | right |
| ,,.,.+-+- | right |
| $*, /$ left |  |
| $* *, * ., /$ | left |
| function application | left |
| left |  |

Figure 2: The operators (and their associativity) of our language in order of increasing precedence

## Literals

$$
\begin{gathered}
\overline{\Gamma \vdash \text { () : unit } \dashv \emptyset} \text { (unit) } \overline{\Gamma \vdash \text { true : bool } \dashv \emptyset} \text { (true) } \overline{\Gamma \vdash \text { false : bool } \dashv \emptyset} \text { (false) } \\
\frac{\mathrm{n} \text { is an integer literal }}{\Gamma \vdash n \text { : int } \dashv \emptyset} \text { (int) } \quad \frac{\mathrm{n} \text { is an floating-point literal }}{\Gamma \vdash n: \text { float } \dashv \emptyset} \text { (float) }
\end{gathered}
$$

## Options

$$
\frac{\alpha \text { is fresh }}{\Gamma \vdash \text { None }: \alpha \text { option } \dashv \emptyset}(\text { none }) \quad \frac{\Gamma \vdash e: \tau \dashv \mathcal{C}}{\Gamma \vdash \text { Some } e: \tau \text { option } \dashv \mathcal{C}} \text { (some) }
$$

Here (and below), fresh means not appearing anywhere in the derivation. You should use gensym to create fresh variables (as in the demo during lecture).

$$
\frac{\Gamma \vdash e: \tau \dashv \mathcal{C} \quad \alpha \text { is fresh } \quad \Gamma, x: \alpha \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash \text { match } e \text { with } \mid \text { Some } x->e_{1} \mid \text { None }->e_{2}: \tau_{2} \dashv \tau \doteq \alpha \text { option, } \tau_{1} \doteq \tau_{2}, \mathcal{C}, \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (matchOpt) }
$$

Note that this isn't really pattern matching. We're not using any notion of a pattern, but instead defining a shallow "destructor".

## Lists

$$
\begin{gathered}
\frac{\alpha \text { is fresh }}{\Gamma \vdash[]: \alpha \text { list } \dashv \emptyset} \text { (nil) } \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash e_{1}:: e_{2}: \tau_{1} \text { list } \dashv \tau_{2} \doteq \tau_{1} \text { list, } \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (cons) } \\
\frac{\Gamma \vdash e: \tau \dashv \mathcal{C} \quad \alpha \text { is fresh } \quad \Gamma, h: \alpha, t: \alpha \text { list } \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash \text { match } e \text { with }\left|h:: t->e_{1}\right|[]->e_{2}: \tau_{2} \dashv \tau \doteq \alpha \text { list, } \tau_{1} \doteq \tau_{2}, \mathcal{C}, \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (matchList) }
\end{gathered}
$$

## Pairs

$$
\begin{gathered}
\frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad}{\Gamma \vdash e_{1}, e_{2}: \tau_{1} * \tau_{2} \dashv \mathcal{C}_{1}, \mathcal{C}_{2}} \frac{\mathcal{C}_{2}}{} \text { (pair) } \\
\frac{\Gamma \vdash e: \tau \dashv \mathcal{C} \quad \alpha, \beta \text { are fresh } \quad \Gamma, x: \alpha, y: \beta \vdash e^{\prime}: \tau^{\prime} \dashv \mathcal{C}^{\prime}}{\Gamma \vdash \operatorname{match} e \text { with } \mid x, y->e^{\prime}: \tau^{\prime} \dashv \tau \doteq \alpha * \beta, \mathcal{C}, \mathcal{C}^{\prime}} \text { (matchPair) }
\end{gathered}
$$

## Variables

$$
\frac{\left(x: \forall \alpha_{1} \cdot \alpha_{2} \ldots \alpha_{k} \cdot \tau\right) \in \Gamma \quad \beta_{1}, \beta_{2}, \ldots, \beta_{k} \text { are fresh }}{\Gamma \vdash x:\left[\beta_{1} / \alpha_{1}\right]\left[\beta_{2} / \alpha_{2}\right] \ldots\left[\beta_{k} / \alpha_{k}\right] \tau \dashv \emptyset}(\text { var) }
$$

Note that this rule will require implementing substitution on types.

## Annotations

$$
\frac{\Gamma \vdash e: \tau^{\prime} \dashv \mathcal{C}}{\Gamma \vdash(e: \tau): \tau \dashv \tau \doteq \tau^{\prime}, \mathcal{C}} \text { (annot) }
$$

## Assertions

$$
\frac{\alpha \text { is fresh }}{\Gamma \vdash \text { assert false }: \alpha \dashv \emptyset} \text { (assertFalse) } \quad \frac{\Gamma \vdash e: \tau \dashv \mathcal{C} \quad e \neq f \text { false }}{\Gamma \vdash \text { assert } e: \text { unit } \vdash \tau \doteq \text { bool, } \mathcal{C}} \text { (assert) }
$$

## Operators

$$
\begin{aligned}
& \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash e_{1}+e_{2}: \text { int } \dashv \tau_{1} \doteq \text { int, } \tau_{2} \doteq \text { int, } \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (add) } \quad \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1}}{\Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}} \overline{\Gamma \vdash e_{1}-e_{2}: \text { int } \dashv \tau_{1} \doteq \text { int, } \tau_{2} \doteq \text { int, } \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (sub) } \\
& \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1}}{\Gamma \vdash e_{1} * e_{2}: \text { int } \dashv \tau_{1} \doteq \text { int }, \tau_{2}: \tau_{2} \dashv \mathcal{C}_{2}} \text { int, } \mathcal{C}_{1}, \mathcal{C}_{2} \quad(m u l) \quad \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash e_{1} / e_{2}: \text { int } \dashv \tau_{1} \doteq \text { int, } \tau_{2} \doteq \text { int, } \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (div) }
\end{aligned}
$$

$$
\begin{aligned}
& \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash e_{1} \bmod e_{2}: \operatorname{int} \dashv \tau_{1} \doteq \text { int, } \tau_{2} \doteq \text { int, } \mathcal{C}_{1}, \mathcal{C}_{2}}(\bmod ) \\
& \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1}}{\Gamma \vdash e_{1}+. e_{2}: f \text { float } \dashv \tau_{1} \doteq \text { float }, \tau_{2} \doteq \tau_{2} \dashv \mathcal{C}_{2}}=\text { float, } \mathcal{C}_{1}, \mathcal{C}_{2} \quad \text { (addFloat) } \\
& \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1}}{\Gamma \vdash e_{1}-. e_{2}: \text { float } \dashv \tau_{1} \doteq \text { float }, \tau_{2} \doteq \text { float }, \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (subFloat) } \\
& \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash e_{1} * . e_{2}: \mathrm{float} \dashv \tau_{1} \doteq \mathrm{float}, \tau_{2} \doteq \mathrm{float}, \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (mulFloat) } \\
& \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1}}{\Gamma \vdash e_{1} / \cdot e_{2}: \text { float } \dashv \tau_{1} \doteq \text { float }, \tau_{2} \doteq \text { float }, \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (divFloat) } \\
& \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash e_{1} * * e_{2}: f l o a t \dashv \tau_{1} \doteq \text { float }, \tau_{2} \doteq \text { float }, \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (powFloat) } \\
& \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash e_{1}<e_{2}: \text { bool } \dashv \tau_{1} \doteq \tau_{2}, \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (It) } \\
& \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash e_{1}<=e_{2}: \text { bool } \dashv \tau_{1} \doteq \tau_{2}, \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (Ite) } \\
& \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash e_{1}>e_{2}: \text { bool } \dashv \tau_{1} \doteq \tau_{2}, \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (gt) } \\
& \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash e_{1}>=e_{2}: \text { bool } \dashv \tau_{1} \doteq \tau_{2}, \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (gte) } \\
& \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash e_{1}=e_{2}: \text { bool } \dashv \tau_{1} \doteq \tau_{2}, \mathcal{C}_{1}, \mathcal{C}_{2}}(\text { eq }) \quad \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash e_{1}<>e_{2}: \text { bool } \dashv \tau_{1} \doteq \tau_{2}, \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (neq) } \\
& \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash e_{1} \& \& e_{2}: \text { bool } \dashv \tau_{1} \doteq \text { bool }, \tau_{2} \doteq \text { bool, } \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (and) } \frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash e_{1} \| \mid e_{2}: \text { bool } \dashv \tau_{1} \doteq \text { bool }, \tau_{2} \doteq \text { bool }, \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (or) }
\end{aligned}
$$

$$
\frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2} \quad \alpha \text { is fresh }}{\Gamma \vdash e_{1} @ e_{2}: \alpha \text { list } \dashv \tau_{1} \doteq \alpha \text { list }, \tau_{2} \doteq \alpha \text { list }, \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (concat) }
$$

## Conditionals

$$
\frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2} \quad \Gamma \vdash e_{3}: \tau_{3} \dashv \mathcal{C}_{3}}{\Gamma \vdash \text { if } e_{1} \text { then } e_{2} \text { else } e_{3}: \tau_{3} \dashv e_{1} \doteq \text { bool, } e_{2} \doteq e_{3}, \mathcal{C}_{1}, \mathcal{C}_{2}, \mathcal{C}_{3}} \text { (if) }
$$

## Functions

$$
\begin{gathered}
\frac{\alpha \text { is fresh } \quad \Gamma, x: \alpha \vdash e: \tau \dashv \mathcal{C}}{\Gamma \vdash \operatorname{fun} x \rightarrow e: \alpha \mapsto \tau \dashv \mathcal{C}} \text { (fun) } \frac{\Gamma, x: \tau \vdash e: \tau^{\prime} \dashv \mathcal{C}}{\Gamma \vdash \text { fun }(x: \tau)->: \tau-\tau^{\prime} \dashv \mathcal{C}} \text { (funAnnot) } \\
\frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2} \quad \alpha \text { is fresh }}{\Gamma \vdash e_{1} e_{2}: \alpha \dashv \tau_{1} \doteq \tau_{2} \rightarrow \alpha, \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (app) }
\end{gathered}
$$

## Let-Expressions

$$
\begin{gathered}
\frac{\Gamma \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma, x: \tau_{1} \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash \operatorname{let} x=e_{1} \text { in } e_{2}: \tau_{2} \dashv \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (let) } \\
\frac{\alpha, \beta \text { is fresh } \quad \Gamma, f: \alpha \rightarrow \beta \vdash e_{1}: \tau_{1} \dashv \mathcal{C}_{1} \quad \Gamma, f: \tau_{1} \vdash e_{2}: \tau_{2} \dashv \mathcal{C}_{2}}{\Gamma \vdash \operatorname{let} \operatorname{rec} f=e_{1} \text { in } e_{2}: \tau_{2} \dashv \tau_{1} \doteq \alpha \rightarrow \beta, \mathcal{C}_{1}, \mathcal{C}_{2}} \text { (letRec) }
\end{gathered}
$$

This completes the description of our typing rules. The function type_of, given a context $\Gamma$ and an expression $e$, should return Some $\tau^{\prime}$ where $\tau^{\prime}$ is the principle type (scheme) of $e$ in the context $\Gamma$. That is, given that $\Gamma \vdash e: \tau \dashv \mathcal{C}$, you must
$\triangleright$ determine the most general unifier $\mathcal{S}$ of the unification problem defined by $\mathcal{C}$;
$\triangleright$ determine the type $\mathcal{S} \tau$, i.e., the type $\tau$ after the substitution $\mathcal{S}$;
$\triangleright$ quantify over the free variables of $\mathcal{S} \tau$ to get the principle type $\tau^{\prime}$.
type_of should return None if there is no unifier for $\mathcal{C}$.

## Part 2: Evaluation

The evaluation of a program in our language is given by the big-step operational semantics presented below. It's identical to that of mini-project 2 , with some additional constructs. We write $\langle\mathcal{E}, e\rangle \Downarrow v$ to indicate that the expression $e$ evaluates to the value $v$ in the dynamic environment $\mathcal{E}$. We use the following notation for environments.

| Notation | Description |
| :---: | :--- |
| $\varnothing$ | empty environment |
| $\mathcal{E}[x \mapsto v]$ | $\mathcal{E}$ with $x$ mapped to $v$ |
| $\mathcal{E}(x)$ | the value of $x$ in $\mathcal{E}$ |

We take a value to be:
$\triangleright$ unit, denoted as •
$\triangleright$ a Boolean value (an element of the set $\mathbb{B}$ ) denoted as true and false
$\triangleright$ an integer (an element of the set $\mathbb{Z}$ ) denoted as $1,-234,12$, etc.
$\triangleright$ a floating-point number (an element of the set $\mathbb{R}$ ), denote as $1.2,-3.14$, etc.
$\triangleright$ a pairs of values, denoted, e.g., as $(u, v)$.
$\triangleright$ a list of values, denoted, e.g., as $v_{1}:: v_{2}:: \cdots:: v_{k}::[]$. We write $l @ r$ for list concatenation.
$\triangleright$ an option value, denoted None or Some $(v)$
$\triangleright$ a closure, denoted as $\{\mathcal{E}, s \mapsto \lambda x . e \emptyset$, where $\mathcal{E}$ is an environment, $s$ is a name (represented as a string), and $\lambda x$.e is a function. We write $\backslash \mathcal{E}, \cdot \mapsto \lambda x$.e $\emptyset$ for a closure without a name.

The function eval_expr, given an expression $e$ (expr), should return $v$ (value) in the case that $\langle\varnothing, e\rangle \Downarrow v$ is derivable according to the given semantics. There are three cases in which this function may raise an exception.
$\triangleright$ DivByZero, the second argument of a division operator was 0 (this includes integer modulus).
$\triangleright$ AssertFail, an assertion within our language (not an OCaml assert) failed.
$\triangleright$ RecWithoutArg, the value of a recursive let-expression evaluates to a named closure (as opposed to an unnamed closure) ${ }^{2}$
$\triangleright$ CompareFunVals, a polymorphic comparison operator (e.g., = or <) was applied to closures.

## Literals

$$
\begin{gathered}
\overline{\langle\mathcal{E},()\rangle \Downarrow \bullet}(\text { unitEval }) \quad \overline{\langle\mathcal{E}, \text { true }\rangle \Downarrow \text { true }} \text { (trueEval) } \quad \overline{\langle\mathcal{E}, \text { false }\rangle \Downarrow \text { false }} \text { (falseEval) } \\
\frac{\mathrm{n} \text { is an integer literal }}{\langle\mathcal{E}, n\rangle \Downarrow n}(\text { intEval }) \quad \frac{\mathrm{n} \text { is an floating-point literal }}{\langle\mathcal{E}, n\rangle \Downarrow n} \text { (floatEval) }
\end{gathered}
$$

## Options

$$
\begin{gathered}
\overline{\langle\mathcal{E}, \text { None }\rangle \Downarrow \text { None }} \text { (evalNone) } \frac{\langle\mathcal{E}, e\rangle \Downarrow v}{\langle\mathcal{E}, \text { Some } e\rangle \Downarrow \operatorname{Some}(v)} \text { (evalSome) } \\
\frac{\langle\mathcal{E}, e\rangle \Downarrow \operatorname{Some}(v) \quad\left\langle\mathcal{E}[x \mapsto v], e_{1}\right\rangle \Downarrow v_{1}}{\left.\langle\mathcal{E}, \text { match } e \text { with }| \text { Some } x->e_{1} \mid \text { None }->e_{2}\right\rangle \Downarrow v_{1}} \text { (evalMatchOptSome) } \\
\frac{\langle\mathcal{E}, e\rangle \Downarrow \text { None } \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\left.\langle\mathcal{E}, \text { match } e \text { with }| \text { Some } x->e_{1} \mid \text { None }->e_{2}\right\rangle \Downarrow v_{2}} \text { (evalMatchOptNone) }
\end{gathered}
$$

## Lists

$$
\begin{gathered}
\frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\langle\mathcal{E},[]\rangle \Downarrow[]} \text { (evalNil) } \quad \frac{\left\langle e_{1}:: e_{2}\right\rangle \Downarrow v_{1}:: v_{2}}{}(\text { evalCons) } \\
\frac{\langle\mathcal{E}, e\rangle \Downarrow v_{h}:: v_{t} \quad\left\langle\mathcal{E}\left[h \mapsto v_{h}\right]\left[t \mapsto v_{t}\right], e_{1}\right\rangle \Downarrow v_{1}}{\langle\mathcal{E}, \text { match } e \text { with }| h:: t->e_{1}\left|[]->e_{2}\right\rangle \Downarrow v_{1}} \text { (evalMatchListCons) } \\
\frac{\langle\mathcal{E}, e\rangle \Downarrow[] \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\langle\mathcal{E}, \text { match } e \text { with }| h:: t->e_{1}\left|[]->e_{2}\right\rangle \Downarrow v_{2}} \text { (evalMatchListNil) }
\end{gathered}
$$

[^1]
## Pairs

$$
\begin{gathered}
\frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\left\langle\mathcal{E}, e_{1}, e_{2}\right\rangle \Downarrow\left(v_{1}, v_{2}\right)} \text { (evalPair) } \\
\frac{\langle\mathcal{E}, e\rangle \Downarrow\left(v_{1}, v_{2}\right) \quad\left\langle\mathcal{E}\left[x \mapsto v_{1}\right]\left[y \mapsto v_{2}\right], e^{\prime}\right\rangle \Downarrow v^{\prime}}{\left\langle\mathcal{E}, \text { match } e \text { with } \mid x, y->e^{\prime}\right\rangle \Downarrow v^{\prime}} \text { (evalMatchPair) }
\end{gathered}
$$

## Variables, Annotations, Assertions

$$
\frac{x \text { is a variable }}{\langle\mathcal{E}, x\rangle \Downarrow \mathcal{E}(x)} \text { (varEval) } \quad \frac{\langle\mathcal{E}, e\rangle \Downarrow v}{\langle\mathcal{E},(e: \tau)\rangle \Downarrow v} \text { (evalAnnot) } \quad \frac{\langle\mathcal{E}, e\rangle \Downarrow t r u e}{\langle\mathcal{E}, \text { assert } e\rangle \Downarrow \bullet} \text { (evalAssert) }
$$

## Operators

$$
\begin{aligned}
& \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\left\langle\mathcal{E}, e_{1}+e_{2}\right\rangle \Downarrow v_{1}+v_{2}} \text { (addEval) } \quad \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1}\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\left\langle\mathcal{E}, e_{1}-e_{2}\right\rangle \Downarrow v_{1}-v_{2}} \text { (subEval) } \\
& \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1}\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\left\langle\mathcal{E}, e_{1} * e_{2}\right\rangle \Downarrow v_{1} \times v_{2}} \text { (mulEval) } \quad \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2} \quad v_{2} \neq 0}{\left\langle\mathcal{E}, e_{1} / e_{2}\right\rangle \Downarrow v_{1} / v_{2}} \text { (divEval) } \\
& \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2} \quad v_{2} \neq 0}{\left\langle\mathcal{E}, e_{1} \bmod e_{2}\right\rangle \Downarrow v_{1} \bmod v_{2}}(\operatorname{modEval}) \\
& \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\left\langle\mathcal{E}, e_{1}+. e_{2}\right\rangle \Downarrow v_{1}+v_{2}} \text { (addFEval) } \quad \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1}\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\left\langle\mathcal{E}, e_{1}-. e_{2}\right\rangle \Downarrow v_{1}-v_{2}} \text { (subFEval) } \\
& \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\left\langle\mathcal{E}, e_{1} * . e_{2}\right\rangle \Downarrow v_{1} \times v_{2}} \text { (mulFEval) } \quad \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\left\langle\mathcal{E}, e_{1} / \cdot e_{2}\right\rangle \Downarrow v_{1} / v_{2}} \text { (divFEval) }
\end{aligned}
$$

Note that there is no division-by-zero error in the case of floating-point numbers.

$$
\frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\left\langle\mathcal{E}, e_{1} * * e_{2}\right\rangle \Downarrow v_{1}^{v_{2}}} \text { (powFEval) }
$$

To save room, all of the following rules do not include the side condition that $v_{1}$ and $v_{2}$ cannot be closures.

$$
\begin{aligned}
& \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2} \quad v_{1}<v_{2}}{\left\langle\mathcal{E}, e_{1}<e_{2}\right\rangle \Downarrow \text { true }} \text { (ItTrue) } \quad \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\left\langle\mathcal{E}, e_{1}<e_{2}\right\rangle \Downarrow \text { false }} v_{1} \geq v_{2} ~(I t F a l s e) \\
& \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2} \quad v_{1} \leq v_{2}}{\left\langle\mathcal{E}, e_{1}<=e_{2}\right\rangle \Downarrow \text { true }} \text { (IteTrue) } \quad \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\left\langle\mathcal{E}, e_{1}<=e_{2}\right\rangle \Downarrow \text { false }} \quad v_{1} \text { (IteFalse) } \\
& \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2} \quad v_{1}>v_{2}}{\left.\left\langle\mathcal{E}, e_{1}\right\rangle e_{2}\right\rangle \Downarrow \text { true }} \text { (gtTrue) } \quad \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2} \quad v_{1} \leq v_{2}}{\left.\left\langle\mathcal{E}, e_{1}\right\rangle e_{2}\right\rangle \Downarrow \text { false }} \text { (gtFalse) }
\end{aligned}
$$

$$
\begin{aligned}
& \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2} \quad v_{1} \geq v_{2}}{\left.\left\langle\mathcal{E}, e_{1}\right\rangle=e_{2}\right\rangle \Downarrow \text { true }} \text { (gteTrue) } \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2} \quad v_{1}<v_{2}}{\left.\left\langle\mathcal{E}, e_{1}\right\rangle=e_{2}\right\rangle \Downarrow \text { false }} \text { (gteFalse) } \\
& \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2} \quad v_{1}=v_{2}}{\left\langle\mathcal{E}, e_{1}=e_{2}\right\rangle \Downarrow t r u e} \text { (eqTrue) } \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2} \quad v_{1} \neq v_{2}}{\left\langle\mathcal{E}, e_{1}=e_{2}\right\rangle \Downarrow \text { false }} \text { (eqFalse) } \\
& \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2} \quad v_{1} \neq v_{2}}{\left\langle\mathcal{E}, e_{1}<>e_{2}\right\rangle \Downarrow \text { true }} \text { (neqTrue) } \quad \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2} \quad v_{1}=v_{2}}{\left\langle\mathcal{E}, e_{1}<>e_{2}\right\rangle \Downarrow \text { false }} \text { (neqFalse) } \\
& \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow \text { false }}{\left\langle\mathcal{E}, e_{1} \& \& e_{2}\right\rangle \Downarrow f a l s e} \text { (andFalse) } \quad \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow \text { true }\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\left\langle\mathcal{E}, e_{1} \& \& e_{2}\right\rangle \Downarrow v_{2}} \text { (andTrue) } \\
& \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow \text { true }}{\left\langle\mathcal{E}, e_{1}\right|\left|e_{2}\right\rangle \Downarrow \text { true }} \text { (orTrue) } \quad \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow \text { false } \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\left\langle\mathcal{E}, e_{1}\right|\left|e_{2}\right\rangle \Downarrow v_{2}} \text { (orFalse) } \\
& \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2}}{\left\langle\mathcal{E}, e_{1} @ e_{2}\right\rangle \Downarrow v_{1} @ v_{2}} \text { (concatEval) }
\end{aligned}
$$

## Conditionals

$$
\frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow \text { true } \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v}{\left\langle\mathcal{E}, \text { if } e_{1} \text { then } e_{2} \text { else } e_{3}\right\rangle \Downarrow v} \text { (ifTrue) } \quad \frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow \text { false } \quad\left\langle\mathcal{E}, e_{3}\right\rangle \Downarrow v}{\left\langle\mathcal{E}, \text { if } e_{1} \text { then } e_{2} \text { else } e_{3}\right\rangle \Downarrow v} \text { (ifFalse) }
$$

## Functions

$$
\begin{aligned}
& \overline{\langle\mathcal{E}, \text { fun } x->e\rangle \Downarrow(\mathcal{E}, \cdot \mapsto \lambda x . e\rangle} \text { (funEval) } \\
& \overline{\langle\mathcal{E}, f \text { un }(x: \tau)->e\rangle \Downarrow(\mathcal{E}, \cdot \mapsto \lambda x . e\rangle} \text { (funEvalAnnot) } \\
& \frac{\left.\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow \backslash \mathcal{E}^{\prime}, \cdot \mapsto \lambda x . e\right\rangle \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2} \quad\left\langle\mathcal{E}^{\prime}\left[x \mapsto v_{2}\right], e\right\rangle \Downarrow v}{\left\langle\mathcal{E}, e_{1} e_{2}\right\rangle \Downarrow v} \text { (appEval) } \\
& \frac{\left.\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow 0 \mathcal{E}^{\prime}, s \mapsto \lambda x . e\right\rangle \quad\left\langle\mathcal{E}, e_{2}\right\rangle \Downarrow v_{2} \quad\left\langle\mathcal{E}^{\prime}\left[s \mapsto \backslash \mathcal{E}^{\prime}, s \mapsto \lambda x . e D\right]\left[x \mapsto v_{2}\right], e\right\rangle \Downarrow v}{\left\langle\mathcal{E}, e_{1} e_{2}\right\rangle \Downarrow v} \text { (appRecEval) }
\end{aligned}
$$

## Let-Expressions

$$
\begin{gathered}
\frac{\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow v_{1} \quad\left\langle\mathcal{E}\left[x \mapsto v_{1}\right], e_{2}\right\rangle \Downarrow v_{2}}{\left\langle\mathcal{E}, \operatorname{let} x: \tau=e_{1} \text { in } e_{2}\right\rangle \Downarrow v_{2}} \text { (letEval) } \\
\frac{\left.\left\langle\mathcal{E}, e_{1}\right\rangle \Downarrow \upharpoonright \mathcal{E}^{\prime}, \cdot \mapsto \lambda x . e\right\rangle \quad\left\langle\mathcal{E}\left[f \mapsto \upharpoonleft \mathcal{E}^{\prime}, f \mapsto \lambda x . e \emptyset\right], e_{2}\right\rangle \Downarrow v_{2}}{\left\langle\mathcal{E}, \text { let } \operatorname{rec} f: \tau->\tau^{\prime}=e_{1} \text { in } e_{2}\right\rangle \Downarrow v_{2}} \text { (letRecEval) }
\end{gathered}
$$

If $e_{1}$ evaluates to a named closure in the above rule, then eval should raise a RecWithoutArg exception.

## Putting Everything Together

After you're done with the required functions, you should be able to run

```
dune exec interp03 filename
```

in order to execute code you've written in other files (replace filename with the name of the file which contains code you want to execute). Our language is subset of OCaml so you should be able to easily write programs, e.g., here is an implementation of sum_of_squares without type annotations:

```
(* sum of squares function *)
let sum_of_squares x y =
    let x_squared = x * x in
    let y_squared = y * y in
    x_squared + y_squared
let _ = assert (sum_of_squares 3(-5) = 34)
```

There are a large number of examples in the file examples.ml. If you're code is correct, you should be able to run this entire file (the inverse is not true, being able to run this file does not guarantee that your code is correct). You can pull out individual test cases as you work through the project.

## Final Remarks

$\triangleright$ There is a lot of repetition here, this is just the nature of implementing programming languages. So even though there is a lot of code to write, it should go pretty quickly. Despite this, it may be worthwhile to think about how to implement the interpreter without too much code replication.
$\triangleright$ Test along the way. Don't try to write the whole interpreter and test afterwards.
$\triangleright$ You must use exactly the same names and types as given at the top of this file. They must appear in the file interp03/lib/lib.ml. If you don't do this, we can't grade your submission. You are, of course, allowed to add your own functions and directories.
$\triangleright$ You're given a skeleton dune project to implement your interpreter. Do not change any of the given code. In particular, don't change the dune files or the utility files. When we grade your assignment we will be assume this skeleton code.
$\triangleright$ We will not immediately release examples or the autograder. You should test yourself as best as you can first.
$\triangleright$ Even though we've given a lot more starter code this time around, you still need to make sure you understand how the starter code works. This means reading code that you didn't write (which is what you'll spend most of your life doing if you go on to be a software engineer). A word of advice: don't immediately ask on Piazza "what does this code do?" Read it, try it out, and try to come up with a more specific question if you're still confused.

Good luck, happy coding.


[^0]:    ${ }^{1}$ We separated the desugaring step in mini-project 2 primarily for pedagogical purposes.

[^1]:    ${ }^{2}$ We dealt with this in the syntax in mini-project 2 .

