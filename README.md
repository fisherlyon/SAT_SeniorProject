# Welcome to My SAT Solver Senior Project
To learn more about the goal of this project, please view the Proposal and Sprint documents.<br>

## Statistics
Statistics obtained using HyperFine -- A Command-line Benchmarking Tool

## Running
In the provided code, there is a Makefile that allows the creation of executables from the command-line for the various SAT solvers and tools shown below.
The Makefile uses raco to create executables of the Racket files, so please make sure that [Racket](https://download.racket-lang.org/) is installed prior to running the Makefile.<br><br>
To make all SAT solvers and tools:
```make
make all
```
To make specific solvers/tools:
```make
make bf-sat
make sat1
make sat2
make dpll-sat
make cdcl-sat
make tseitin
make write-cnf
make make-check
```
The usage of each of the solvers/tools is shown below under their descriptions.

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

## SAT Solver Implementations

### Brute Force SAT Solver (bf-sat.rkt)
This SAT solver implementation checks every possible truth value assignment (TVA) starting by setting all variables in a boolean formula to false, then working up to setting all variables being true. For a formula with <i>n</i> boolean variables, there are 2<sup>n</sup> possible TVAs. So, in order to test all possible TVAs, the program uses the binary representation of integers from 0 to 2<sup>n-1</sup>, assigning each bit in the integer to a variable in the boolean formula. After an UNSAT, the current TVA integer is incremented to TVA+1, then that is tested. Once one of the TVAs returns SAT, that TVA is returned.<br>
```
Usage: ./bf-sat <in_filename> [<out_filename>]

 • in_filename : The file name of the input .cnf file to be tested for satisfiability
 • out_filename : Optional file name of a file where output from the satisfiability test will be written
```

### SAT-I SAT Solver (sat1.rkt)
This SAT solver implementation utilizes Depth-First-Search (DFS) and returns a complete variable instantiation (truth value assignment). It also uses the idea of conditioning. When traversing the tree, at each level, a TVA is assigned to a variable. By conditioning the knowledge base (boolean formula) on the variable given its TVA, we can reduce the size of the knowledge base until either SAT or UNSAT is returned. As previously stated, this variation returns a complete variable instantiation, meaning we have conditioned the knowledge base on every variable.<br>
```
Usage: ./sat1 [-v] <in_filename> [<out_filename>]

 • -v : Verbose truth value assignment result
 • in_filename : The file name of the input .cnf file to be tested for satisfiability
 • out_filename : Optional file name of a file where output from the satisfiability test will be written
```

### SAT-II SAT Solver (sat2.rkt)
This SAT solver implementation is just an extension of the previoiusly mentioned SAT-I. The only difference between SAT-II and SAT-I is that SAT-II returns a partial variable instantiation. This can be done due to the fact that it isn't always necessary to traverse the tree all the way down to a leaf node in order to have SAT returned.<br>
```
Usage: ./sat2 [-v] <in_filename> [<out_filename>]

 • -v : Verbose truth value assigment result
 • in_filename : The file name of the input .cnf file to be tested for satisfiability
 • out_filename : Optional file name of a file where output from the satisfiability test will be written
```

### DPLL SAT Solver (dpll-sat.rkt)
This SAT solver implementation utilizes the David-Putnam-Logemann-Loveland (DPLL) algorithm and heavily relies on the idea of unit resolution, which reduces the clausal form  of a knowledge base based on unit clauses (clauses of size one). Unit resolution returns a set of literals that were either present as unit clauses in the knowledge base or derived by unit resolution (I), and a new knowledge base which results from conditioning the knowledge base on I (Γ).<br><br>When Γ is empty, SAT is returned, and if there is a contradiction in Γ, then UNSAT is returned. For this method to progress, literals are chosen, using a heuristic, to be added to Γ as a clause for unit resolution. The heuristic used in the provided code is called "Maximum Occurrence in Minimum-sized Clauses" (MOM). When SAT is achieved, I is returned.<br>
```
Usage: ./dpll-sat [-v] <in_filename> [<out_filename>]

 • -v : Verbose truth value assigment result
 • in_filename : The file name of the input .cnf file to be tested for satisfiability
 • out_filename : Optional file name of a file where output from the satisfiability test will be written
```

### CDCL SAT Solver (cdcl-sat.rkt)
This SAT solver implementation utilizes the idea of Conflict-Driven Clause Learning (CDCL). This version also uses unit resolution, but it is different than the one used in DPLL as it also takes in a decision sequence (D) and a set of learned clauses (Γ). The general idea of this algorithm is that many decisions--literals to be added as clauses to the knowledge base for unit resolution--are made and once a conflict/contradiction is reached, we analyze it.<br><br>This is done by constucting an implication graph to see what decisions and implications ultimately led up to the contradiction. From analyzing the implication graph, we are able to derive learned clauses that we can add to Γ that are implied by the knowledge base. In the provided code, an asserting learned clause is chosen based on a heuristic called the first UIP. A UIP (unique implication point) in the implication graph is a dominator from the decision made at the highest level to the contradiction. A dominator is a node in the graph that is passed through in all possible paths from the source node (decision node made at the highest decision level) to a target node (contradiction). The first UIP is the UIP that is the closest to the contradiction.<br><br>Once the asserting clause is derived by creating cuts in the implication graph, we back-track to the decision level where the contradiction originated. When a literal is chosen to be a decision, it must be the case that neither itself or its negation is implied by unit resolution. This process runs until either unit resolution doesn't detect a contradiction and there aren't any more literals to be chosen, in which SAT is returned along with I, or if unit resolution detects a contradiction and the decision sequence D is empty, in which UNSAT is returned.<br>
```
Usage: ./cdcl-sat [-v] <in_filename> [<out_filename>]

 • -v : Verbose truth value assigment result
 • in_filename : The file name of the input .cnf file to be tested for satisfiability
 • out_filename : Optional file name of a file where output from the satisfiability test will be written
```

## CNF Tools

### Tseitin Transformation (tseitin.rkt)
This program--based on the [Tseitin Transformation Algorithm](https://en.wikipedia.org/wiki/Tseytin_transformation)--converts a boolean formula into an equisatisfiable boolean formula in conjunctive normal form (CNF), with formula size growing linearly. Another method of CNF conversion--the naive approach--is to use De Morgan's law and the distributive property to convert the given boolean formula to CNF. However, this can result in an exponential increase formula size.
```
Usage: ./tseitin -c \"<boolean_formula>\" [<out_filename>]
Usage: ./tseitin -f <boolean_formula_filename> [<out_filename>]

 • -c : Run ./tseitin with command-line argument boolean formula
 • -f : Run ./tseitin with in-file boolean formula
 • boolean_formula : The boolean formula to be converted (must be in quotes)
 • boolean_formula_filename : The name of the file that holds the boolean formula to be converted
 • out_filename : Optional file name of a file where output from the satisfiability test will be written
 • NOTE : The given formula must be provided in the form described above under "Data Definitions"
```

### CNF File Creation (write-cnf.rkt)
The core purpose of this program is to take an arbitrary boolean formula, convert it to CNF, and put that transformation into a file with the DIMACS .cnf format. This file format is used commonly in modern SAT solvers.<br>
For DIMACS .cnf form:
- Lines that begin with 'c' are comment lines.
    - .cnf files created using this program will contain a comment line of the variables used--this is used for verbose results
- The line that begins with 'p' is the header/problem line. The 'p' is followed by the problem type, which in our case is 'cnf'. After the problem type, the number of variables is stated, followed by the number of clauses.
- The remaining lines are the clauses of the formula, where each line is a disjunct terminated by a '0'. Positive boolean variables are represented by positive integers and the negation of a boolean variable is represnted by a negative integer. 
```
ex. (& (v A B) (v C (~ D)))
 => (& (v 1 2) (v 3 -4))
 =>
            c FILE: <filename>
            c VARS: A B C D
            c
            p cnf 4 2
            1 2 0
            3 -4 0
```
```
Usage: ./write-cnf -c <filename> "<boolean_formula>"
Usage: ./write-cnf -f <filename> <boolean_formula_filename>

 • -c : Run ./write-cnf with command-line argument boolean formula
 • -f : Run ./write-cnf with in-file boolean formula
 • filename : The file name of the .cnf file to be written to
 • boolean_formula : The boolean formula to be converted (must be in quotes)
 • boolean_formula_filename : The name of the file that holds the boolean formula to be converted
 • NOTE : The given formula must be provided in the form described above under "Data Definitions"
```

### Make Check (make-check.rkt)
This program was made as a tool for myself so that I could test the SAT implementations with a known, working SAT sovler implementation. When testing satisfiability using one of the SAT solvers above, the user has the option to write the output to an output file. That output file is used, in conjunction with a copy of the originally tested .cnf file to make a "SAT-Check" file. The SAT-Check file is also a .cnf file, but at the end, there is the appended, given SAT output. By appending the SAT output to the original .cnf file, it adds a series of unit clauses to the original knowledge base that correspond to the TVA of the SAT output. When running this new .cnf file through a known, working SAT solver, it ensures that the given SAT output is valid, and also further ensures that my SAT implementations are correct!
```
Usage: ./make-check <SAT_OUT> <check_filename>

 • SAT_OUT : The file name of the output from a satisfiability test
 • check_filename : The file name of the .cnf file that was tested on
 • NOTE : It is recommended that 'check_filename' is a copy of the original .cnf file because text appending occurs to the file
```

## Resources
The following links lead to resources that I used during the duration of this project.<br>
- [Automated Reasoning by UCLA Automated Reasoning Group](https://www.youtube.com/playlist?list=PLlDG_zCuBub5AyHuxnw8vfgx7Wd-P-4XN)<br>
- [How to find the learned clause from a UIP cut on Computer Science Stack Exchange](https://cs.stackexchange.com/questions/163098/how-to-find-the-learned-clause-from-a-uip-cut)<br>
- [Module 5 - Tseitin transformation by EKU - Logical Foundations of Computer Science](https://www.youtube.com/watch?v=fd9gjzZE1-4)<br>
- [CNF Files](https://people.sc.fsu.edu/%7Ejburkardt/data/cnf/cnf.html)<br>
- [Public CNF Benchmark Files](https://github.com/prokls/cnf-files-download)<br>
- [MiniSAT SAT Solver](https://github.com/niklasso/minisat)<br>
- [MiniSAT User Guide: How to use the MiniSAT SAT Solver](https://dwheeler.com/essays/minisat-user-guide.html)<br>

