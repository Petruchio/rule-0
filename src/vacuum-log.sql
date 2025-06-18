BEGIN;

DROP VIEW meta.vacuum_stats;

CREATE OR REPLACE VIEW meta.vacuum_stats AS
WITH base AS (
SELECT
	c.relnamespace::regnamespace AS schema_name,
	c.oid::regclass              AS relation_name,
	pg_stat_get_ins_since_vacuum(c.oid)::big_natural_number AS n_ins_since_vacuum,
	pg_stat_get_last_vacuum_time(c.oid) AS last_vacuum,
	pg_stat_get_last_autovacuum_time(c.oid) AS last_autovacuum,
	pg_stat_get_vacuum_count(c.oid)::big_natural_number AS vacuum_count,
	pg_stat_get_autovacuum_count(c.oid)::big_natural_number AS autovacuum_count
FROM pg_class c
WHERE c.relkind = ANY (ARRAY['r'::"char", 't'::"char", 'm'::"char", 'p'::"char"])
)
	SELECT
		schema_name,
		relation_name,
		n_ins_since_vacuum AS stale_row_estimate,
		date_trunc('second', now() - last_vacuum + interval '0.5 second') AS age,
		vacuum_count,
		date_trunc('second', last_vacuum     + interval '0.5 second')     AS last_vacuum,
		autovacuum_count,
		date_trunc('second', last_autovacuum + interval '0.5 second')     AS last_autovacuum
	FROM base
	ORDER BY schema_name, relation_name
;

COMMIT;
