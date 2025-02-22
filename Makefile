all: sat1 sat2 dpll-sat

sat1: src/sat1.rkt
	raco exe -o sat1 src/sat1.rkt

sat2: src/sat2.rkt
	raco exe -o sat2 src/sat2.rkt

dpll-sat: src/dpll-sat.rkt
	raco exe -o dpll-sat src/dpll-sat.rkt

clean: 
	rm -rf sat1 sat2 dpll-sat
