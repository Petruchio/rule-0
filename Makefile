SCHEMATA = rule_0 security security_lookup meta meta_lookup update
TARGETS  = "rule_0|security|security_lookup|meta|meta_lookup|update"
SQL=dump/rule-0.sql
VER="0.1"

.PHONY: dump dump-each install help push

help:
	@ cat doc/usage

dump:
	pg_dump --no-owner -d rule_0 --schema=$(TARGETS) > ./dump/rule-0.sql

dump-each:
	@ echo "Dumping Rule 0 entities to $(SQL)"
	for name in $(SCHEMATA); do \
		./bin/dump-schema -s $$name -d rule_0 > ./dump/$$name.sql; \
	done
	@ echo "Done."

install:
	@if [ -z "$(db)" ]; then \
		echo "Error: Variable 'db' is not set. Please run 'make db=<database> install'"; \
		exit 1; \
	fi
	@ echo "Installing Rule 0 to $(db)"
	@ psql -v ON_ERROR_STOP=on --single-transaction -f $(SQL) $(db)
	@ echo "Done."
