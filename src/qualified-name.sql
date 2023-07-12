BEGIN;

-- CREATE SCHEMA rule_0;

-- DROP   TYPE rule_0.qualified_name;

/*
CREATE TYPE rule_0.qualified_name AS (
	schema_name    NAME,
	object_name    NAME,
	qualified_name TEXT
);
*/

CREATE OR REPLACE FUNCTION
	rule_0.get_qualified_name(
		object_schema NAME,
		object_name   NAME  DEFAULT NULL
	)
RETURNS rule_0.qualified_name
AS $$

	DECLARE
		ret rule_0.qualified_name;

	BEGIN

		IF object_schema LIKE '%.%.%' THEN
			RAISE EXCEPTION 'Illegal schema name (too many periods): %', object_schema;
		END IF;

		IF object_name IS NOT NULL THEN
			ret.schema_name    := object_schema;
			ret.object_name       := object_name;
			ret.qualified_name := object_schema || '.' || object_name;
			RETURN ret;
		END IF;

		IF object_schema LIKE '%.%' THEN
			ret.schema_name    := REGEXP_REPLACE(object_schema, '\..*', '');
			ret.object_name       := REGEXP_REPLACE(object_schema, '.*\.', '');
			ret.qualified_name := object_schema;
			RETURN ret;
		END IF;

		WITH
		sp AS (
			SELECT
				ordinality,
				schema_name AS schemaname
			FROM
				rule_0.search_path
		)
		SELECT DISTINCT ON (matviewname)
			schemaname
		INTO
			ret.schema_name
		FROM
			sp
		NATURAL JOIN
			pg_matviews
		WHERE
			matviewname = object_schema
		ORDER BY
			matviewname,
			ordinality;

		IF ret.schema_name IS NULL THEN
			RAISE EXCEPTION 'Cannot find the relation "%" in search_path.', object_schema;
		END IF;

		ret.object_name       := object_schema;
		ret.qualified_name := ret.schema_name || '.' || ret.object_name;
		RETURN ret;

	END;

$$ LANGUAGE plpgsql;

COMMIT;
