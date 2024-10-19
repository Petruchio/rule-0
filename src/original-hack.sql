WITH RECURSIVE

base_deps AS (
	SELECT DISTINCT
		dependent_ns.nspname   AS dependent_schema,
		dependent_view.relname AS dependent_view,
		source_ns.nspname      AS source_schema,
		source_table.relname   AS source_table

	FROM pg_depend
	JOIN pg_rewrite                 ON pg_depend.objid     = pg_rewrite.oid
	JOIN pg_class as dependent_view ON pg_rewrite.ev_class = dependent_view.oid
	JOIN pg_class as source_table   ON pg_depend.refobjid  = source_table.oid
	JOIN pg_namespace dependent_ns  ON dependent_ns.oid    = dependent_view.relnamespace
	JOIN pg_namespace source_ns     ON source_ns.oid       = source_table.relnamespace

	WHERE dependent_ns.nspname   != source_ns.nspname
	OR    dependent_view.relname != source_table.relname
),

view_deps AS (

SELECT * FROM base_deps
UNION
SELECT * FROM base_deps bd
INNER JOIN    view_deps vd
	ON  vd.dependent_schema = bd.source_schema
	AND vd.dependent_view   = bd.dependent_view

	WHERE bd.source_schema != vg.dependent_schema
	OR    bd.source_view   != vg.dependent_view

	AND NOT (
		dependent_ns.nspname   = vd.dependent_schema
		AND
		dependent_view.relname = vd.dependent_view
	)
)

SELECT *
FROM view_deps
ORDER BY source_schema, source_table;
