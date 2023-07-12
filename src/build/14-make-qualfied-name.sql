CREATE OR REPLACE FUNCTION
	rule_0.qualified_name(
		object_schema TEXT,
		object_name   TEXT
	)
RETURNS rule_0.qualified_name
AS $$

	DECLARE
		ret rule_0.qualified_name;

	BEGIN

			RAISE NOTICE
				'Called this.';

		IF object_schema IS NULL THEN
			RAISE EXCEPTION
				'Schema cannot be null.';
		END IF;

		IF object_name IS NULL THEN
			RAISE EXCEPTION
				'Name cannot be null.';
		END IF;

		/* Should we be ascertaining that the entities exist? */

		ret.schema_name    := object_schema;
		ret.object_name    := object_name;
		ret.qualified_name := object_schema || '.' || object_name;
		RETURN ret;

	END;

$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION
	rule_0.qualified_name(
		object_name TEXT
	)
RETURNS rule_0.qualified_name
AS $$

	DECLARE
		ret rule_0.qualified_name;

	BEGIN

		IF object_name IS NULL THEN
			RAISE EXCEPTION
				'Name cannot be null.';
		END IF;

		IF object_name LIKE '%.%.%' THEN
			RAISE EXCEPTION
				'Illegal name (too many periods): %',
				object_name;
		END IF;

		IF object_name LIKE '%.%' THEN
			ret.schema_name    := REGEXP_REPLACE(object_name, '\..*', '');
			ret.object_name    := REGEXP_REPLACE(object_name, '.*\.', '');
			ret.qualified_name := object_name;
			RETURN ret;
		END IF;

		object_name := schema_name;

		SELECT rule_0.find_in_path(object_name)
		INTO   ret;

		IF ret.schema_name IS NULL THEN
			RAISE EXCEPTION
				'Cannot find the object "%" in search_path.',
				object_name;
		END IF;

		RETURN ret;

	END;

$$ LANGUAGE plpgsql;
