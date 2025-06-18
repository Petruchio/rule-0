
CREATE OR REPLACE VIEW pg_catalog.pg_stat_all_tables AS
 SELECT c.oid AS relid,
    n.nspname AS schemaname,
    c.relname,
    pg_stat_get_numscans(c.oid) AS seq_scan,
    pg_stat_get_lastscan(c.oid) AS last_seq_scan,
    pg_stat_get_tuples_returned(c.oid) AS seq_tup_read,
    sum(pg_stat_get_numscans(i.indexrelid))::bigint AS idx_scan,
    max(pg_stat_get_lastscan(i.indexrelid)) AS last_idx_scan,
    sum(pg_stat_get_tuples_fetched(i.indexrelid))::bigint + pg_stat_get_tuples_fetched(c.oid) AS idx_tup_fetch,
    pg_stat_get_tuples_inserted(c.oid) AS n_tup_ins,
    pg_stat_get_tuples_updated(c.oid) AS n_tup_upd,
    pg_stat_get_tuples_deleted(c.oid) AS n_tup_del,
    pg_stat_get_tuples_hot_updated(c.oid) AS n_tup_hot_upd,
    pg_stat_get_tuples_newpage_updated(c.oid) AS n_tup_newpage_upd,
    pg_stat_get_live_tuples(c.oid) AS n_live_tup,
    pg_stat_get_dead_tuples(c.oid) AS n_dead_tup,
    pg_stat_get_mod_since_analyze(c.oid) AS n_mod_since_analyze,
    pg_stat_get_ins_since_vacuum(c.oid) AS n_ins_since_vacuum,
    pg_stat_get_last_vacuum_time(c.oid) AS last_vacuum,
    pg_stat_get_last_autovacuum_time(c.oid) AS last_autovacuum,
    pg_stat_get_last_analyze_time(c.oid) AS last_analyze,
    pg_stat_get_last_autoanalyze_time(c.oid) AS last_autoanalyze,
    pg_stat_get_vacuum_count(c.oid) AS vacuum_count,
    pg_stat_get_autovacuum_count(c.oid) AS autovacuum_count,
    pg_stat_get_analyze_count(c.oid) AS analyze_count,
    pg_stat_get_autoanalyze_count(c.oid) AS autoanalyze_count
   FROM pg_class c
     LEFT JOIN pg_index i ON c.oid = i.indrelid
     LEFT JOIN pg_namespace n ON n.oid = c.relnamespace
  WHERE c.relkind = ANY (ARRAY['r'::"char", 't'::"char", 'm'::"char", 'p'::"char"])
  GROUP BY c.oid, n.nspname, c.relname
