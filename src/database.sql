DROP VIEW IF EXISTS
	meta.database;

CREATE VIEW
	meta.database
AS

WITH
	pg_tsp
AS (
	SELECT
		oid     AS dattablespace,
		spcname AS tablespace_name
	FROM
		pg_catalog.pg_tablespace
)
SELECT
	oid                           AS database_id,
	datname                       AS database_name,
	datdba::regrole               AS owner,
	pg_encoding_to_char(encoding) AS encoding,
	locale_provider               AS locale_provider,
	datistemplate                 AS is_template,
	tablespace_name               AS default_tablespace,
	datcollate                    AS lc_collate,
	datctype                      AS lc_ctype,
	datlocale                     AS collation_provider_locale,
	daticurules                   AS icu_collation_rules,
	datcollversion                AS collation_version
FROM
	pg_database
NATURAL JOIN
	meta_lookup.datlocprovider
NATURAL JOIN
	pg_tsp
ORDER BY
	database_name
;

/*

Should add these elsewhere:

	Security:
		datacl
		datallowconn
		dathasloginevt
		datconnlimit

	Maintenance:
		datfrozenxid
		datminmxid

*/
