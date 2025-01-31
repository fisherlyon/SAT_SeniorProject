# Welcome to my SAT Solver Senior Project

To learn more about the goal of this project, please view the Proposal and Sprint documents.<br>

## Data Definitions

### Formula EBNF:

```
<Formula> ::= <var> | <aux>
            | (! Formula)
            | (& (Listof Formula))
            | (v (Listof Formula))
            | (-> Formula Formula)
            | (<-> Formula Formula)
```
