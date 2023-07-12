-- To do:  return correct number of rows affected. */

BEGIN;

CREATE OR REPLACE VIEW
	rule_0.search_path
AS

SELECT ordinality, schema_name
FROM
	STRING_TO_TABLE(
		( SELECT CURRENT_SETTING('search_path') ),
		', '
	)
	WITH ORDINALITY x(schema_name, ordinality)
	WHERE
		schema_name != ''
	;


/* Validate schema?
 * Watch for zero-length schemata
 * Watch for commas */

CREATE OR REPLACE FUNCTION
	rule_0.push_to_search_path(
		schema_name NAME
	)
RETURNS TEXT AS $$
	SELECT
		SET_CONFIG(
			'search_path',
			CONCAT_WS(
				', ',
				NULLIF(
					CURRENT_SETTING('search_path'),
					''
				),
				schema_name
			),
			false
		)
	FROM
		pg_settings
	WHERE
		name = 'search_path';
$$ LANGUAGE sql;

-- Not presently handling UPDATE on ordinality.

CREATE OR REPLACE FUNCTION
	rule_0.push_to_search_path_trigger()
RETURNS TRIGGER AS $$
	BEGIN
	PERFORM rule_0.push_to_search_path(NEW.schema_name::NAME);
	RETURN NULL;
	END;
$$
LANGUAGE plpgsql;


CREATE TRIGGER search_path_insert
	INSTEAD OF INSERT ON
		rule_0.search_path
	FOR EACH ROW
		EXECUTE FUNCTION rule_0.push_to_search_path_trigger();

-- Allows comma hijinx.  Should validate.

CREATE OR REPLACE FUNCTION
	rule_0.update_search_path(
		old_name NAME,
		new_name NAME
	)
RETURNS TEXT AS $$
	WITH
	old_schemata AS (
		SELECT
			STRING_TO_TABLE(
				( SELECT CURRENT_SETTING('search_path') ),
				', '
			) AS schema_name
	),
	new_schemata AS (
		SELECT
			COALESCE(
				NULLIF(schema_name, old_name),
				new_name
			) AS schema_name
		FROM
			old_schemata
	)
	SELECT
		SET_CONFIG(
			'search_path',
			STRING_AGG(schema_name, ', '),
			false
		) AS search_path
		FROM
			new_schemata;
$$ LANGUAGE sql;

-- Doesn't handle updates to ordinality

CREATE OR REPLACE FUNCTION
	rule_0.update_search_path_trigger()
RETURNS TRIGGER AS $$
	BEGIN
	PERFORM rule_0.update_search_path(OLD.schema_name, NEW.schema_name);
	RETURN NULL;
	END;
$$
LANGUAGE plpgsql;


CREATE TRIGGER search_path_update
	INSTEAD OF UPDATE ON
		rule_0.search_path
	FOR EACH ROW
		EXECUTE FUNCTION rule_0.update_search_path_trigger();


-- Allows comma hijinx

CREATE OR REPLACE FUNCTION
	rule_0.delete_from_search_path(
		doomed_schema NAME
	)
RETURNS TEXT AS $$
	WITH
	old_schemata AS (
		SELECT
			STRING_TO_TABLE(
				( SELECT CURRENT_SETTING('search_path') ),
				', '
			) AS schema_name
	),
	new_schemata AS (
		SELECT
			schema_name
		FROM
			old_schemata
		WHERE
			schema_name != doomed_schema
	)
	SELECT
		SET_CONFIG(
			'search_path',
			COALESCE(STRING_AGG(schema_name, ', '), ''),
			false
		) AS search_path
		FROM
			new_schemata;
$$ LANGUAGE sql;

CREATE OR REPLACE FUNCTION
	rule_0.delete_from_search_path_trigger()
RETURNS TRIGGER AS $$
	BEGIN
	PERFORM rule_0.delete_from_search_path(
		OLD.schema_name
	);
	RETURN NULL;
	END;
$$
LANGUAGE plpgsql;


CREATE TRIGGER search_path_delete
	INSTEAD OF DELETE ON
		rule_0.search_path
	FOR EACH ROW
		EXECUTE FUNCTION rule_0.delete_from_search_path_trigger();


CREATE OR REPLACE PROCEDURE
	rule_0.save_search_path()
AS $$

	DECLARE
		sql TEXT;

	BEGIN

	SELECT FORMAT(
		'ALTER ROLE %s IN DATABASE %s SET search_path TO %s;',
		current_user,
		current_database(),
		setting
	)
	FROM pg_settings
	WHERE name = 'search_path'
	INTO   sql;

	EXECUTE sql;

	END;
$$
LANGUAGE plpgsql;


COMMIT;
