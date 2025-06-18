BEGIN;

DROP VIEW IF EXISTS
	meta.tables_experiment;
CREATE OR REPLACE VIEW
	meta.tables_experiment
AS

WITH target AS (
	SELECT
		relation_type                AS table_type,
		c.relnamespace::regnamespace AS schema_name,
		c.oid::regclass              AS table_name,
		pg_get_userbyid(c.relowner)  AS owner,
		relnatts                     AS field_count,
		relation_persistence         AS persistence,
		NULLIF(c.reltuples, -1)      AS row_estimate

	FROM         pg_class                   AS c
	NATURAL JOIN meta_lookup.relkind        AS rk
	NATURAL JOIN meta_lookup.relpersistence AS rp
	WHERE c.relkind::text = ANY(ARRAY['r', 'p', 'f', 't'])
)
SELECT
	t.*
FROM
	target AS t

;
COMMIT;
