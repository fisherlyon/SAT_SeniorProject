# Welcome to my SAT Solver Senior Project
To learn more about the goal of this project, please view the Proposal and Sprint documents.<br>

## Statistics
Statistics obtained using HyperFine -- A Command-line Benchmarking Tool

## Running

...

## Data Definitions (form-def.rkt)

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
```racket
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

## Brute Force SAT Solver (bf-sat.rkt)
This SAT solver implementation checks every possible truth value assignment (TVA) starting by setting all variables in a boolean formula to false, then working up to setting all variables being true. For a formula with <i>n</i> boolean variables, there are 2<sup>n</sup> possible TVAs. So, in order to test all possible TVAs, the program uses the binary representation of integers from 0 to 2<sup>n-1</sup>, assigning each bit in the integer to a variable in the boolean formula. After an UNSAT, the current TVA integer is incremented to TVA+1, then that is tested. Once one of the TVAs returns SAT, that TVA is returned.<br>
```
Usage: ./bf-sat <in_filename> [<out_filename>]
```

## CNF File Creation (write-cnf.rkt)
The core purpose of this program is to take an arbitrary boolean formula, convert it to CNF, and put that transformation into a file with the DIMACS .cnf format. This file format is used commonly in modern SAT solvers.<br>
For DIMACS .cnf form:
- Lines that begin with 'c' are comment lines.
- The line that begins with 'p' is the header/problem line. The 'p' is followed by the problem type, which in our case is 'cnf'. After the problem type, the number of variables is stated, followed by the number of clauses.
- The remaining lines are the clauses of the formula, where each line is a disjunct terminated by a '0'. Positive boolean variables are represented by positive integers and the negation of a boolean variable is represnted by a negative integer. 
```
ex. (A v B) ∧ (C v ¬D) <==> (& (v A B) (v C (~ D))) <==> (& (v 1 2) (v 3 -4))

            c FILE: <filename>
            c
            p cnf 4 2
            1 2 0
            3 -4 0
```

## Testing SAT by Reading a CNF File (read-cnf.rkt)
...
