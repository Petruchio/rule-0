CREATE OR REPLACE FUNCTION
	rule_0.get_qualified_name(
		object_schema NAME,
		object_name   NAME  DEFAULT NULL
	)
RETURNS rule_0.qualified_name
AS $$

	DECLARE
		ret   rule_0.qualified_name;

	BEGIN

		IF object_schema LIKE '%.%.%' THEN
			RAISE EXCEPTION
				'Illegal schema name (too many periods): %',
				object_schema;
		END IF;

		IF object_name IS NOT NULL THEN
			ret.schema_name    := object_schema;
			ret.object_name    := object_name;
			ret.qualified_name := object_schema || '.' || object_name;
			RETURN ret;
		END IF;

		IF object_schema LIKE '%.%' THEN
			ret.schema_name    := SUBSTRING(object_schema FROM '^[^\.]+');
			ret.object_name    := SUBSTRING(object_schema FROM '[^\.]+$');
			ret.qualified_name := object_schema;
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
