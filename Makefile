all : write_cnf

write_cnf : write_cnf.rkt
	raco exe -o write_cnf write_cnf.rkt

clean: 
	rm -rf write_cnf
