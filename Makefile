all: sat1 sat2 dpll-sat cdcl-sat make-check 

sat1: src/sat1.rkt
	raco exe -o sat1 src/sat1.rkt

sat2: src/sat2.rkt
	raco exe -o sat2 src/sat2.rkt

dpll-sat: src/dpll-sat.rkt
	raco exe -o dpll-sat src/dpll-sat.rkt

cdcl-sat: src/cdcl-sat.rkt
	raco exe -o cdcl-sat src/cdcl-sat.rkt

make-check: src/make-check.rkt
	raco exe -o make-check src/make-check.rkt

clean: 
	rm -rf sat1 sat2 dpll-sat cdcl-sat make-check
