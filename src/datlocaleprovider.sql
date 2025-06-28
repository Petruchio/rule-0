BEGIN;

DROP TABLE IF EXISTS meta_lookup.datlocprovider;

CREATE TABLE
	meta_lookup.datlocprovider (
		datlocprovider  CHAR PRIMARY KEY,
		locale_provider TEXT UNIQUE NOT NULL
);

COPY meta_lookup.datlocprovider FROM stdin;
b	builtin
c	libc
i	icu
\.

COMMIT;
