# Welcome to my SAT Solver Senior Project

To learn more about the goal of this project, please view the Proposal and Sprint documents.<br>

## Data Definitions

### Formula EBNF:

```
<Formula> ::= <var> | <aux>             // Boolean variable or auxiliary variable
            | (! Formula)               // Negation of a boolean formula
            | (& (Listof Formula))      // Conjunction of boolean formulas
            | (v (Listof Formula))      // Disjunction of boolean formulas
            | (-> Formula Formula)      // Conditional of two boolean formulas
            | (<-> Formula Formula)     // Biconditional of two boolean formulas
```
