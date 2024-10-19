WITH RECURSIVE

schemata AS (
	SELECT
		oid     AS view_schema_oid,
		nspname AS schema_name
	FROM pg_namespace
)
,

views AS (
	SELECT
		oid          AS view_oid,
		relname      AS view_name,
		relnamespace AS view_schema_oid
	FROM   pg_class
)
,

depend AS (
	SELECT
		ev_class AS view_oid,
		refobjid AS source_view_oid
	FROM   pg_depend
	JOIN   pg_rewrite
	ON     pg_depend.objid = pg_rewrite.oid
)
,

view_deps AS (
	SELECT DISTINCT
		dependent_ns.schema_name AS dependent_schema,
		dependent_view.view_name AS dependent_view,
		source_ns.schema_name    AS source_schema,
		source_table.view_name   AS source_table
	FROM
		depend
	NATURAL JOIN
		views AS dependent_view
	JOIN
		views AS source_table
	ON
		depend.source_view_oid = source_table.view_oid
	JOIN
		schemata dependent_ns
	ON
		dependent_ns.view_schema_oid = dependent_view.view_schema_oid
	JOIN
		schemata source_ns
	ON
		source_ns.view_schema_oid = source_table.view_schema_oid
	WHERE NOT (
		dependent_ns.schema_name = source_ns.schema_name
		AND
		dependent_view.view_name = source_table.view_name
	)

	UNION

	SELECT DISTINCT
		dependent_ns.schema_name AS dependent_schema,
		dependent_view.view_name AS dependent_view,
		source_ns.schema_name    AS source_schema,
		source_table.view_name   AS source_table
	FROM
		depend
	NATURAL JOIN
		views AS dependent_view
	JOIN
		views as source_table
	ON
		depend.source_view_oid = source_table.view_oid
	JOIN
		schemata dependent_ns
	ON
		dependent_ns.view_schema_oid = dependent_view.view_schema_oid
	JOIN
		schemata source_ns
	ON
		source_ns.view_schema_oid = source_table.view_schema_oid
	INNER JOIN
		view_deps vd
	ON
		vd.dependent_schema = source_ns.schema_name
	AND vd.dependent_view = source_table.view_name
	AND NOT (
		dependent_ns.schema_name = vd.dependent_schema
		AND
		dependent_view.view_name = vd.dependent_view
	)
)

SELECT
	*
FROM
	view_deps
ORDER BY
	source_schema,
	source_table
;
