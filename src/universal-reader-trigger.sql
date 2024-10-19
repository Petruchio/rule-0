CREATE OR REPLACE FUNCTION rule_0.grant_select_on_new_schema() RETURNS event_trigger AS $$
DECLARE
	obj record;
BEGIN
	FOR obj IN
		SELECT nspname
		FROM pg_namespace
		WHERE nspname NOT IN ('pg_catalog', 'information_schema')
		LOOP
			EXECUTE 'ALTER DEFAULT PRIVILEGES IN SCHEMA ' || quote_ident(obj.nspname) || ' GRANT SELECT ON TABLES TO universal_reader;';
			FOR obj IN
				SELECT table_schema, table_name
				FROM information_schema.tables
				WHERE table_type IN ('BASE TABLE', 'VIEW')
				AND table_schema = obj.nspname
				LOOP
					EXECUTE 'GRANT SELECT ON ' || quote_ident(obj.table_schema) || '.' || quote_ident(obj.table_name) || ' TO universal_reader;';
			END LOOP;
	END LOOP;
END;
$$ LANGUAGE plpgsql;

CREATE EVENT TRIGGER on_schema_create
ON ddl_command_end
WHEN TAG IN ('CREATE SCHEMA')
EXECUTE FUNCTION grant_select_on_new_schema();
