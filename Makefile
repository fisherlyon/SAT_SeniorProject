all: write_cnf read_cnf

write_cnf: write_cnf.rkt
	raco exe -o write_cnf write_cnf.rkt

read_cnf: read_cnf.rkt
	raco exe -o bf_sat read_cnf.rkt

clean: 
	rm -rf write_cnf bf_sat
