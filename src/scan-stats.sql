BEGIN;

DROP VIEW IF EXISTS statistics.scan_stats;
DROP VIEW IF EXISTS statistics.index_scans;
DROP VIEW IF EXISTS statistics.sequential_scans;

CREATE OR REPLACE VIEW statistics.index_scans AS
WITH base AS (
	SELECT
		c.relnamespace::regnamespace       AS schema_name,
		c.oid::regclass                    AS relation_name,
		sum(pg_stat_get_numscans(i.indexrelid))::bigint AS index_scan_count,
		max(pg_stat_get_lastscan(i.indexrelid))         AS last_index_scan,
		sum(
			pg_stat_get_tuples_fetched(i.indexrelid)
		)::bigint +
		pg_stat_get_tuples_fetched(c.oid)               AS index_tuples_read
	FROM pg_class c
	JOIN pg_index AS i ON c.oid = i.indrelid
	WHERE c.relkind = ANY (ARRAY['r'::"char", 't'::"char", 'm'::"char", 'p'::"char"])
	GROUP BY c.oid, c.relnamespace, c.relname
)
	SELECT
		schema_name,
		relation_name,
		index_scan_count,
		date_trunc('second', now() - last_index_scan + interval '0.5 second') AS last_index_scan_age,
		date_trunc('second',         last_index_scan + interval '0.5 second') AS last_index_scan,
		index_tuples_read
	FROM base
	ORDER BY schema_name, relation_name
;

CREATE OR REPLACE VIEW statistics.sequential_scans AS
WITH base AS (
	SELECT
		c.relnamespace::regnamespace       AS schema_name,
		c.oid::regclass                    AS relation_name,
		pg_stat_get_numscans(c.oid)        AS seq_scan_count,
		pg_stat_get_lastscan(c.oid)        AS last_seq_scan,
		pg_stat_get_tuples_returned(c.oid) AS seq_tuples_read
	FROM pg_class c
	WHERE c.relkind = ANY (ARRAY['r'::"char", 't'::"char", 'm'::"char", 'p'::"char"])
	GROUP BY c.oid, c.relnamespace, c.relname
)
	SELECT
		schema_name,
		relation_name,
		seq_scan_count,
		date_trunc('second', now() - last_seq_scan + interval '0.5 second') AS last_seq_scan_age,
		date_trunc('second',         last_seq_scan + interval '0.5 second') AS last_seq_scan,
		seq_tuples_read
	FROM base
	ORDER BY schema_name, relation_name
;

COMMIT;
