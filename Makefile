SCHEMATA="(rule_0|security|security_lookup|meta|meta_lookup|update|public)"
SQL=dump/rule-0.sql
VER="0.1"

.PHONY: dump install help

help:
	@echo "*********************************************************************"
	@echo "*                     Rule 0 for PostgreSQL                         *"
	@echo "*      This will install Rule 0 (v.$(VER)) to a PostgreSQL database.   *"
	@echo "*********************************************************************"
	@echo "To install:"
	@echo "             make db=<database> install"
	@echo "You must specify the database to which you wish to install."
	@echo "For safety, the default database is not assumed."

dump:
	@ echo "Dumping Rule 0 entities to $(SQL)"
	@ pg_dump --no-owner --schema=$(SCHEMATA) > $(SQL)
	@ echo "Done."

install:
	@if [ -z "$(db)" ]; then \
		echo "Error: Variable 'db' is not set. Please run 'make db=<database> install'"; \
		exit 1; \
	fi
	@ echo "Installing Rule 0 to $(db)"
	@ psql -v ON_ERROR_STOP=on --single-transaction -f $(SQL) $(db)
	@ echo "Done."
