CREATE OR REPLACE FUNCTION
	rule_0.find_in_path(
		target_name name
	)
RETURNS rule_0.qualified_name
AS $$

WITH

the_path AS (
	SELECT ordinality, schema_name
	FROM
		STRING_TO_TABLE(
			( SELECT CURRENT_SETTING('search_path') ),
			', '
		)
		WITH ORDINALITY x(schema_name, ordinality)
	WHERE
		schema_name != ''
),

the_stuff AS (
	SELECT
		nspname AS schema_name,
		relname AS object_name
	FROM pg_class
	JOIN pg_namespace ns
	ON   relnamespace = ns.oid
)

SELECT DISTINCT ON (object_name)
	(
		schema_name,
		object_name,
		schema_name || '.' || object_name
	)::rule_0.qualified_name
FROM
	the_path
NATURAL JOIN
	the_stuff
WHERE
	object_name = target_name
ORDER BY
	object_name, ordinality ASC
;

$$ LANGUAGE SQL;
