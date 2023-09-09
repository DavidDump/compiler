$$
\begin{align}
[Root] &\to [Statement] \text{;} \text{*} \\

[Statement] &\to \begin{cases}
    [Keyword] \\
    [Expresion] \\
\end{cases} \\

[Keyword] &\to \begin{cases}
    [Return] \\
    [Var] \\
\end{cases} \\

[Return] &\to \text{return} \space [Expresion] \text{;} \\

[Var] &\to \begin{cases}
    [DefOnly] \\
    [DefAssignment] \\
\end{cases} \\

[DefOnly] &\to \text{var} \space Ident \text{;} \\
[DefAssignment] &\to \text{var} \space Ident = [Expresion] \text{;} \\

[Expresion] &\to \begin{cases}
    intLit \text{;} \\
    [BinExp] \\
    Ident
\end{cases} \\
[BinExp] &\to intLit \space operator [BinExp] \text{;} \\

\end{align} \\

intLit \space \text{is a number between } −(2^{63}) \space \text{and} \space 2^{63} − 1 \\
operator \space \text{is one of the following: +, -, *, /} \\
Ident \space \text{is a string with the following pattern ([a-zA-Z][a-zA-Z0-9]*)}
$$