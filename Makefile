RKT_SRCS := bf-sat sat1 sat2 dpll-sat cdcl-sat write-cnf make-check

all: $(RKT_SRCS)

$(RKT_SRCS): %: src/%.rkt
	raco exe -o $@ $<

clean:
	rm -rf $(RKT_SRCS)

