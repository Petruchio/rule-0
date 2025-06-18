BEGIN;

DROP VIEW IF EXISTS meta.analyze_stats;

CREATE OR REPLACE VIEW meta.analyze_stats AS
WITH base AS (
	SELECT
		c.relnamespace::regnamespace AS schema_name,
		c.oid::regclass              AS relation_name,
		pg_stat_get_mod_since_analyze(c.oid)::big_natural_number AS modified_rows_estimate,
		pg_stat_get_last_analyze_time(c.oid)                     AS last_analyze,
		pg_stat_get_last_autoanalyze_time(c.oid)                 AS last_autoanalyze,
		pg_stat_get_analyze_count(c.oid)::big_natural_number     AS analyze_count,
		pg_stat_get_autoanalyze_count(c.oid)::big_natural_number AS autoanalyze_count
	FROM
		pg_class c
	WHERE
		c.relkind = ANY (ARRAY['r'::"char", 't'::"char", 'm'::"char", 'p'::"char"])
)
SELECT
	schema_name,
	relation_name,
	modified_rows_estimate AS stale_row_estimate,
	date_trunc('second', now() - last_analyze + interval '0.5 second') AS age,
	analyze_count,
	date_trunc('second', last_analyze     + interval '0.5 second')     AS last_analyze,
	autoanalyze_count,
	date_trunc('second', last_autoanalyze + interval '0.5 second')     AS last_autoanalyze
FROM base
ORDER BY schema_name, relation_name
;

COMMIT;
