BEGIN;

DROP VIEW IF EXISTS statistics.tuple_stats;

CREATE OR REPLACE VIEW statistics.tuple_stats AS
SELECT
	c.relnamespace::regnamespace              AS schema_name,
	c.oid::regclass                           AS relation_name,
	pg_stat_get_tuples_inserted(c.oid)        AS tuples_inserted,
	pg_stat_get_tuples_updated(c.oid)         AS tuples_updated,
	pg_stat_get_tuples_deleted(c.oid)         AS tuples_deleted,
	pg_stat_get_tuples_hot_updated(c.oid)     AS tuples_hot_updated,
	pg_stat_get_tuples_newpage_updated(c.oid) AS tuples_newpage_updated,
	pg_stat_get_live_tuples(c.oid)            AS live_tuples,
	pg_stat_get_dead_tuples(c.oid)            AS dead_tuples
FROM pg_class c
WHERE c.relkind = ANY (ARRAY['r'::"char", 't'::"char", 'm'::"char", 'p'::"char"])
;

COMMIT;
