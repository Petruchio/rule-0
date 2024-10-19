CREATE OR REPLACE FUNCTION
	rule_0.object_type(sch NAME, obj NAME DEFAULT NULL)
RETURNS
	TEXT
LANGUAGE
	PLPGSQL
AS
$$

DECLARE
	ret TEXT;

BEGIN

	IF obj IS NULL
	THEN
		obj := sch;
		sch := (rule_0.find_in_path(obj)).schema_name;
	END IF;

	WITH
	the_reltypes(relkind, relation_type) AS (
		VALUES
			('i'::"char", 'Index'),
			('S'::"char", 'Sequence'),
			('t'::"char", 'Toast Table'),
			('v'::"char", 'View'),
			('m'::"char", 'Materialized View'),
			('c'::"char", 'Composite Type'),
			('f'::"char", 'Foreign Table'),
			('p'::"char", 'Partitioned Table'),
			('I'::"char", 'Partitioned Index'),
			('r'::"char", 'Table')
	),

	lookup AS (
		SELECT
			nspname       AS schema_name,
			relname       AS relation_name,
			relation_type
		FROM
			pg_class
		NATURAL JOIN
			the_reltypes
		JOIN
			pg_namespace nsp
		ON
			relnamespace = nsp.oid
		)

		SELECT relation_type
		INTO   ret
		FROM   lookup
		WHERE  schema_name = sch
		AND    relation_name = obj
	;

	RETURN ret;

END;
$$
;


-- Tests:
SELECT rule_0.object_type('bol_t2');
SELECT rule_0.object_type('ace_t2_normalize', 'bol_t2');
SELECT rule_0.object_type('ace_file');
SELECT rule_0.object_type('privilege_type');
SELECT rule_0.object_type('ace_file_pkey');
SELECT rule_0.object_type('cargo_t2', NULL);
