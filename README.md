# Welcome to my SAT Solver Senior Project
To learn more about the goal of this project, please view the Proposal and Sprint documents.<br>

## Running

...

## Data Definitions

### Formula EBNF:

```
<Formula> ::= <var> | <aux>             // Boolean variable or auxiliary variable
            | (~ Formula)               // Negation of a boolean formula
            | (& (Listof Formula))      // Conjunction of boolean formulas
            | (v (Listof Formula))      // Disjunction of boolean formulas
            | (-> Formula Formula)      // Conditional of two boolean formulas
            | (<-> Formula Formula)     // Biconditional of two boolean formulas
```

### Racket Implementation:
```
(define-type Formula (U varF auxF notF andF orF condF bicondF))
(struct varF ([var : Symbol]) #:transparent)
(struct auxF ([aux : Symbol]) #:transparent)
(struct notF ([form : Formula]) #:transparent)
(struct andF ([forms : (Listof Formula)]) #:transparent)
(struct orF ([forms : (Listof Formula)]) #:transparent)
(struct condF ([l : Formula] [r : Formula]) #:transparent)
(struct bicondF ([l : Formula] [r : Formula]) #:transparent)
```

### Example Formula Construction
A boolean formula as seen in the wild might look something like the following.
```
(A ∧ ¬B ∧ ¬C) v (D -> (E <-> ¬F)) v G 
```
Where...
```
    ¬   = Negation
    ∧   = Conjunction
    v   = Disjunction
    ->  = Conditional
    <-> = Biconditional
```
By the stated data definitions, the above formula in Racket would look like the following:
```
(v (& A (~ B) C) (-> D (<-> E (~ F))) G)
```
Where...
```
    ~   = Negation
    &   = Conjunction
    v   = Disjunction
    ->  = Conditional
    <-> = Biconditional
```

## Tseitin Transformation (tseitin.rkt)

## Brute Force SAT Solver (bf_force_sat.rkt)

## CNF File Creation (write_cnf.rkt)
The core purpose of this program is to take an arbitrary boolean formula, convert it to CNF, and put that transformation into a file with the DIMACS .cnf format. This file format is used commonly in modern SAT solvers.<br>
For DIMACS .cnf form:
- Lines that begin with 'c' are comment lines.
- The line that begins with 'p' is the header line and is formatted like the following:
- The remaining lines are the clauses of the formula, where each line is a disjunct terminated by a zero. Positive boolean variables are represented by positive integers and the negation of a boolean variables is represnted by a negative integer. In the case of my code, the order in which the clauses are written to the file is in reverse order; where clauses that are read first in the formula will likely appear last in the file.
```
ex. (A v B) ∧ (C v ¬D) <==> (& (v A B) (v C (~ D))) <==> (& (v 1 2) (v 3 -4))

            c FILE: <filename>
            c
            p cnf 4 2
            3 -4 0
            1 2 0
