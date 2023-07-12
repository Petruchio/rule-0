CREATE OR REPLACE FUNCTION
	rule_0.object_type(
		object_schema NAME,
		object_name   NAME DEFAULT NULL
	)
RETURNS text
AS $$

WITH

args AS (
	SELECT CASE object_name
		WHEN NULL THEN rule_0.find_in_path(object_schema)
		ELSE
			(
				object_schema,
				object_name,
				object_schema || '.' || object_name
			)::rule_0.qualified_name
		END CASE
),

types AS (
	SELECT *
	FROM (
		VALUES
			('r'::CHAR, 'Table'),
			('i'::CHAR, 'Index'),
			('s'::CHAR, 'Sequence'),
			('v'::CHAR, 'View'),
			('c'::CHAR, 'Composite Type'),
			('t'::CHAR, 'Toast Table'),
			('m'::CHAR, 'Materialized View'),
			('f'::CHAR, 'Foreign Table')
	)
	AS tbl (relkind, object_type)
),

class_stuff AS (
	SELECT
		relname       AS relation,
		relnamespace  AS oid,
		relkind::CHAR AS relkind
	FROM
		pg_class
),

together AS (
	SELECT
		nspname AS schema_name,
		relation,
		object_type
	FROM         class_stuff
	NATURAL JOIN types
	NATURAL JOIN pg_namespace
)

SELECT * FROM args

/*
SELECT object_type
FROM   together, args
WHERE  schema_name = schema_name
AND    relation    = object_name
*/
;

$$
LANGUAGE SQL;
