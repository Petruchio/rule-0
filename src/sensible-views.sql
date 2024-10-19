DROP VIEW              meta.view_dependencies;
CREATE OR REPLACE VIEW meta.view_dependencies AS

WITH

relkind AS (
	SELECT
		relkind::"char",
		relation_type
	FROM (
		VALUES
			('r', 'Ordinary Table'),
			('i', 'Index'),
			('S', 'Sequence'),
			('t', 'TOAST table'),
			('v', 'View'),
			('m', 'Materialized View'),
			('c', 'Composite Type'),
			('f', 'Foreign Table'),
			('p', 'Partitioned Table'),
			('I', 'Partitioned Index')
	)
	AS tbl (
		relkind,
		relation_type
	)
)
,

schemata AS (
	SELECT
		oid     AS relnamespace,
		nspname AS schema_name
	FROM
		pg_namespace
),

relations as (
	SELECT
		oid           AS view_oid,
		schema_name   AS schema_name,
		relname       AS view_name,
		relation_type AS relation_type
	FROM         pg_class
	NATURAL JOIN schemata
	NATURAL JOIN relkind
),

deps AS (
	SELECT
		ev_class AS dependent_oid,
		refobjid AS source_oid
	FROM pg_depend
	JOIN pg_rewrite
	ON   pg_depend.objid = pg_rewrite.oid
)
,

dependencies AS (
	SELECT DISTINCT
		a.schema_name   AS dependent_schema,
		a.view_name     AS dependent_view,
		a.relation_type AS dependent_view_type,
		b.schema_name   AS source_schema,
		b.view_name     AS source_view,
		b.relation_type AS source_relation_type
	FROM deps      d
	JOIN relations a
	ON   d.dependent_oid = a.view_oid
	JOIN relations b
	ON   d.source_oid    = b.view_oid
)

SELECT
	*
FROM
	dependencies
WHERE
	dependent_schema != source_schema
OR
	dependent_view   != source_view
ORDER BY
	dependent_schema,
	dependent_view,
	source_schema,
	source_view
;
