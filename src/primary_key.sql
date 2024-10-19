CREATE VIEW
	meta.primary_key
AS

SELECT
	NULLIF(conparentid, 0)             AS parent_id,
	c.oid                              AS primary_key_id,
	conname                            AS primary_key_name,
	nspname                            AS schema_name,
	cl.relname                         AS table_name,
	i.relname                          AS index_name,
	condeferred                        AS deferred,
	condeferrable                      AS deferrable,
	conislocal                         AS local,
	connoinherit                       AS inheritable,
	coninhcount                        AS ancestor_count,
	ARRAY_AGG(attname ORDER BY attnum) AS primary_key_columns

FROM
	pg_constraint c
JOIN
	pg_namespace  s
ON
	connamespace = s.oid
JOIN
	pg_class      cl
ON
	conrelid     = cl.oid
JOIN
	pg_class      i
ON
	conindid     = i.oid
JOIN
	pg_attribute  a
ON
	conrelid = attrelid
AND
	attnum = ANY(conkey)
WHERE
	contype = 'p'
GROUP BY
	c.oid,
	nspname,
	cl.relname,
	i.relname,
	conname,
	condeferred,
	condeferrable,
	conislocal,
	connoinherit,
	coninhcount,
	conkey
;
