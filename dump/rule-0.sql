--
-- PostgreSQL database dump
--

-- Dumped from database version 17devel
-- Dumped by pg_dump version 17devel

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: meta; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA meta;


--
-- Name: SCHEMA meta; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA meta IS 'PostgreSQL metadata, formatted for human beings.';


--
-- Name: meta_lookup; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA meta_lookup;


--
-- Name: SCHEMA meta_lookup; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA meta_lookup IS 'Lookup tables to resolve codes and IDs in PostgreSQL metadata.';


--
-- Name: rule_0; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA rule_0;


--
-- Name: SCHEMA rule_0; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA rule_0 IS 'Entities related to the Rule 0 project for controlling PostgreSQL relationally.';


--
-- Name: security; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA security;


--
-- Name: SCHEMA security; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA security IS 'PostgreSQL security information.';


--
-- Name: security_lookup; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA security_lookup;


--
-- Name: SCHEMA security_lookup; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA security_lookup IS 'Lookup tables to resolve codes and IDs in PostgreSQL security data.';


--
-- Name: update; Type: SCHEMA; Schema: -; Owner: -
--

CREATE SCHEMA update;


--
-- Name: SCHEMA update; Type: COMMENT; Schema: -; Owner: -
--

COMMENT ON SCHEMA update IS 'Information on the status and dependencies of materialized views.';


--
-- Name: big_counting_number; Type: DOMAIN; Schema: rule_0; Owner: -
--

CREATE DOMAIN rule_0.big_counting_number AS bigint
	CONSTRAINT big_counting_number_check CHECK ((VALUE > 0));


--
-- Name: big_natural_number; Type: DOMAIN; Schema: rule_0; Owner: -
--

CREATE DOMAIN rule_0.big_natural_number AS bigint
	CONSTRAINT big_natural_number_check CHECK ((VALUE >= 0));


--
-- Name: counting_number; Type: DOMAIN; Schema: rule_0; Owner: -
--

CREATE DOMAIN rule_0.counting_number AS integer
	CONSTRAINT counting_number_check CHECK ((VALUE > 0));


--
-- Name: digit; Type: DOMAIN; Schema: rule_0; Owner: -
--

CREATE DOMAIN rule_0.digit AS smallint
	CONSTRAINT digit_check CHECK (((VALUE >= 0) AND (VALUE < 10)));


--
-- Name: natural_number; Type: DOMAIN; Schema: rule_0; Owner: -
--

CREATE DOMAIN rule_0.natural_number AS integer
	CONSTRAINT natural_number_check CHECK ((VALUE >= 0));


--
-- Name: qualified_name; Type: TYPE; Schema: rule_0; Owner: -
--

CREATE TYPE rule_0.qualified_name AS (
	schema_name name,
	object_name name,
	qualified_name text
);


--
-- Name: small_counting_number; Type: DOMAIN; Schema: rule_0; Owner: -
--

CREATE DOMAIN rule_0.small_counting_number AS smallint
	CONSTRAINT small_counting_number_check CHECK ((VALUE > 0));


--
-- Name: small_natural_number; Type: DOMAIN; Schema: rule_0; Owner: -
--

CREATE DOMAIN rule_0.small_natural_number AS smallint
	CONSTRAINT small_natural_number_check CHECK ((VALUE >= 0));


--
-- Name: create_view_key(regclass, name, name[]); Type: PROCEDURE; Schema: meta; Owner: -
--

CREATE PROCEDURE meta.create_view_key(IN view_name regclass, IN key_name name, VARIADIC fields name[])
    LANGUAGE plpgsql
    AS $$
DECLARE
	field NAME;
BEGIN
	INSERT INTO meta.view_keys (view_name, key_name)
	VALUES (view_name, key_name);
	FOREACH field IN ARRAY fields
	LOOP
		INSERT INTO meta.view_key_fields (view_name, key_name, field_name)
		VALUES (view_name, key_name, field);
	END LOOP;
END;
$$;


--
-- Name: explain_to_json(text); Type: FUNCTION; Schema: meta; Owner: -
--

CREATE FUNCTION meta.explain_to_json(query text) RETURNS json
    LANGUAGE plpgsql
    AS $$

DECLARE
	tmp   TEXT;
	ret   TEXT;

BEGIN

	tmp := REGEXP_REPLACE(query, ';.*', '');
	EXECUTE FORMAT('EXPLAIN (FORMAT JSON) %s', tmp) INTO ret;
	RETURN ret::JSON;

END
$$;


--
-- Name: field_exists(regclass, name); Type: FUNCTION; Schema: meta; Owner: -
--

CREATE FUNCTION meta.field_exists(relation regclass, field_name name) RETURNS boolean
    LANGUAGE sql
    AS $$
	SELECT COUNT(*) > 0
	FROM  pg_attribute
	WHERE attrelid = relation
	AND   attname  = field_name;
$$;


--
-- Name: field_must_exist(regclass, name); Type: PROCEDURE; Schema: meta; Owner: -
--

CREATE PROCEDURE meta.field_must_exist(IN relation regclass, IN field_name name)
    LANGUAGE plpgsql
    AS $$
BEGIN
	IF meta.field_exists(relation, field_name)
	THEN
		RETURN;
	END IF;
	RAISE EXCEPTION
		'The field % does not exist in the relation %.%',
		field_name,
		get_schema(relation),
		get_relation(relation);
END;
$$;


--
-- Name: get_trigger_when_clause(oid); Type: FUNCTION; Schema: meta; Owner: -
--

CREATE FUNCTION meta.get_trigger_when_clause(trigger_id oid) RETURNS text
    LANGUAGE sql
    AS $$
	/*
		NB: This is a short-term solution.
		    It is definitely not reliable in all cases.
		    Replace this ASAP.
	*/
	WITH def AS (
		SELECT PG_GET_TRIGGERDEF(trigger_id) AS tdef
	)
	SELECT
		REGEXP_REPLACE(
			tdef,
			'.* WHEN \((.*)\) EXECUTE FUNCTION .*',
			'\1'
		)
	FROM  def
	WHERE tdef ~ 'WHEN \(';
$$;


--
-- Name: must_be_a(regclass, text); Type: PROCEDURE; Schema: meta; Owner: -
--

CREATE PROCEDURE meta.must_be_a(IN relation regclass, IN target_type text)
    LANGUAGE plpgsql
    AS $$
DECLARE
	reltype TEXT;
BEGIN
	SELECT meta.relation_type(relation) INTO reltype;
	IF LOWER(reltype) != LOWER(target_type) THEN
		RAISE EXCEPTION
			'Expected % to be a %, but it is a %.',
			relation,
			target_type,
			reltype;
	END IF;
END;
$$;


--
-- Name: relation_type(regclass); Type: FUNCTION; Schema: meta; Owner: -
--

CREATE FUNCTION meta.relation_type(relation regclass) RETURNS text
    LANGUAGE sql
    AS $$
	SELECT
		relation_type
	FROM
		pg_class
	NATURAL JOIN
		meta.relation_type
	WHERE
		oid = relation;
$$;


--
-- Name: truncate_tables(regclass[]); Type: PROCEDURE; Schema: meta; Owner: -
--

CREATE PROCEDURE meta.truncate_tables(VARIADIC tables regclass[])
    LANGUAGE plpgsql
    AS $$
DECLARE
    table_name regclass;
BEGIN
    FOREACH table_name IN ARRAY tables LOOP
        EXECUTE format('TRUNCATE TABLE %s;', table_name);
    END LOOP;
END;
$$;


--
-- Name: cast_options(anycompatible); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.cast_options(datum anycompatible) RETURNS TABLE(type_name name)
    LANGUAGE plpgsql
    AS $$

DECLARE
	type_name     NAME;
	valid_types   NAME[];

BEGIN

	FOR type_name IN
		SELECT typname
		FROM   pg_type
		WHERE  typcategory !~ 'P|X|Z'
		LOOP
			BEGIN
				EXECUTE FORMAT('SELECT CAST(''%s'' AS %s);', datum, type_name);
				valid_types := ARRAY_APPEND(valid_types, type_name);
			EXCEPTION WHEN OTHERS THEN CONTINUE;
			END;
		END LOOP;

	RETURN QUERY SELECT unnest(valid_types) AS typ order by typ;

END
$$;


--
-- Name: clean_name(text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.clean_name(the_name text) RETURNS text
    LANGUAGE sql
    AS $_$
	SELECT
		trim(
			BOTH FROM
			upper (
				regexp_replace(
					translate(
						the_name,
						',.-()&+:/''"?;#{}`][|@!?><%=$\^',
						''
					)
					,
					' {2,}'::text,
					' '::text,
					'g'::text
				)
			)
		)
$_$;


--
-- Name: clean_whitespace(text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.clean_whitespace(the_text text) RETURNS text
    LANGUAGE sql
    AS $$
	SELECT
		TRIM(
			REGEXP_REPLACE(
				the_text,
				' {2,}',
				' ',
				'g'
			)
		)
$$;


--
-- Name: column_cast_options(name, name, name); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.column_cast_options(schema_name name, relation_name name, column_name name DEFAULT NULL::name) RETURNS TABLE(type_name name)
    LANGUAGE plpgsql
    AS $_$

DECLARE
	tbl rule_0.qualified_name;
	sql TEXT;

BEGIN

	IF column_name IS NULL THEN
		column_name   := relation_name;
		relation_name := schema_name;
		schema_name   := NULL;
		IF relation_name ~ '\.' THEN
			tbl := rule_0.get_qualified_name(relation_name);
		ELSE
			RAISE NOTICE 'Looking for:  %', relation_name;
			tbl := rule_0.find_in_path(relation_name);
		END IF;
	ELSE
		tbl := rule_0.get_qualified_name(schema_name, relation_name);
	END IF;

	IF tbl IS NULL THEN
		RAISE EXCEPTION 'Could not find the table:  %', tbl;
	END IF;

	sql := FORMAT($ooo$
			WITH

			distinct_val_count(val_count, val) AS (
				SELECT   COUNT(*), %s
				FROM     %s
				WHERE    %s IS NOT NULL
				GROUP BY %s
			),

			target_number(valid_count) AS (
				SELECT COUNT(*) FROM distinct_val_count
			),

			candidate_types(type_name) AS (
				SELECT cast_options(val)
				FROM   distinct_val_count
			),

			type_counts AS (
				SELECT
					COUNT(*) AS valid_count,
					type_name
				FROM
					candidate_types
				GROUP BY
					type_name
			)

			SELECT       type_name AS candidate_types
			FROM         type_counts
			NATURAL JOIN target_number
			ORDER BY     type_name
			;
			$ooo$,
			column_name,
			tbl.qualified_name,
			column_name,
			column_name
		);

	RETURN QUERY EXECUTE(sql);

END
$_$;


--
-- Name: copy_by_key(regclass, regclass, name); Type: PROCEDURE; Schema: rule_0; Owner: -
--

CREATE PROCEDURE rule_0.copy_by_key(IN source_relation regclass, IN target_table regclass, IN key_field name)
    LANGUAGE plpgsql
    AS $_$

DECLARE
	sql         TEXT;
	key_to_copy TEXT;

BEGIN

	sql := $sql$
		SELECT DISTINCT s.%s::TEXT
		FROM            %s AS s
		LEFT JOIN       %s    AS t
		USING           (%s)
		WHERE           t.%s IS NULL
		ORDER BY        %s
	$sql$;

		FOR key_to_copy
		IN EXECUTE FORMAT(sql, key_field, source_relation, target_table, key_field, key_field, key_field)
		LOOP
			RAISE NOTICE '%', key_to_copy;
		END LOOP;

END
$_$;


--
-- Name: copy_to_csv(regclass); Type: PROCEDURE; Schema: rule_0; Owner: -
--

CREATE PROCEDURE rule_0.copy_to_csv(IN relation regclass)
    LANGUAGE plpgsql
    AS $$

DECLARE
	target_dir  TEXT;
	target_file TEXT;

BEGIN

	SELECT value
	FROM   config.setting
	WHERE  setting = 'export_dir'
	INTO   target_dir;

	SELECT
		FORMAT(
			'%s.%s-%s.csv',
			get_schema(relation),
			relation,
			NOW()::DATE
		)
	INTO target_file;

	EXECUTE
		FORMAT(
			'COPY (SELECT * FROM %s) TO ''%s/%s'' DELIMITER '','' CSV HEADER;',
			relation,
			target_dir,
			target_file
		);

END
$$;


--
-- Name: copy_view(regclass, text); Type: PROCEDURE; Schema: rule_0; Owner: -
--

CREATE PROCEDURE rule_0.copy_view(IN view_name regclass, IN new_name text)
    LANGUAGE plpgsql
    AS $$

DECLARE
	view_definition TEXT;
	view_schema     TEXT;
	create_view     TEXT;

BEGIN
	view_schema     := get_schema(view_name);
	view_definition := pg_get_viewdef(view_name);

	IF view_definition IS NULL THEN
		RAISE EXCEPTION 'Cannot find source view: %', view_name;
	END IF;

	create_view := FORMAT('
CREATE OR REPLACE VIEW
	%s
AS
%s
',
	new_name,
	view_definition
);

	EXECUTE create_view;
END;
$$;


--
-- Name: create_materialized_view_schema_from_view_schema(name, name); Type: PROCEDURE; Schema: rule_0; Owner: -
--

CREATE PROCEDURE rule_0.create_materialized_view_schema_from_view_schema(IN source_schema name, IN destination_schema name DEFAULT NULL::name)
    LANGUAGE plpgsql
    AS $$

DECLARE
	source_view TEXT;
	target      TEXT;
	view_def    TEXT;
	t1          TIMESTAMPTZ;

BEGIN

	IF destination_schema IS NULL
	THEN
		destination_schema = source_schema || '_matview';
		RAISE NOTICE 'No destination schema specified. Using %', destination_schema;
	END IF;

		RAISE NOTICE '
Creating a schema full of materialized views.
Each view in the source schema will have
a materialzied view in the destination schema.';
		RAISE NOTICE 'Source schema:      %', source_schema;
		RAISE NOTICE 'Destination schema: %', destination_schema;

	EXECUTE( 'CREATE SCHEMA IF NOT EXISTS ' || destination_schema  );

	-- Create second version for create or replace mat views

	FOR
		source_view
	IN
		SELECT
			table_name
		FROM
			information_schema.views
		WHERE
			table_schema = source_schema
	LOOP

		target := destination_schema || '.' || source_view || '_matview';

		SELECT clock_timestamp() INTO t1;

		RAISE NOTICE
			'Creating materialized view: %',
			target;

		EXECUTE('DROP MATERIALIZED VIEW IF EXISTS ' || target);

		view_def := FORMAT(
			'CREATE MATERIALIZED VIEW %s AS SELECT v.* FROM %s.%s v WITH NO DATA;',
			target,
			source_schema,
			source_view
		);

		RAISE NOTICE '%', view_def;
		EXECUTE(view_def);
		COMMIT;

		RAISE NOTICE
			'Finished with %. Elapsed time: %s',
			target,
			t1 - clock_timestamp()
			;


	END LOOP;

END;
$$;


--
-- Name: delete_from_search_path(name); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.delete_from_search_path(doomed_schema name) RETURNS text
    LANGUAGE sql
    AS $$
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
$$;


--
-- Name: delete_from_search_path_trigger(); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.delete_from_search_path_trigger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
	BEGIN
	PERFORM rule_0.delete_from_search_path(
		OLD.schema_name
	);
	RETURN OLD;
	END;
$$;


--
-- Name: dupview(text, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.dupview(oldname text, newname text) RETURNS bigint
    LANGUAGE plpgsql
    AS $$

declare 

recreate_view text;

drop_view text;

viewdef text;

begin

  viewdef:= pg_get_viewdef(''||oldname||'');

  recreate_view := 'create or replace view '||newname||' as '||viewdef;

  execute recreate_view;

  drop_view := 'drop view '||oldname||';';

  execute drop_view;

  return 0;

end;

$$;


--
-- Name: exec(text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.exec(query text) RETURNS SETOF record
    LANGUAGE plpgsql
    AS $_$
BEGIN
	RETURN QUERY EXECUTE $1;
END
$_$;


--
-- Name: explain(text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.explain(query text) RETURNS text
    LANGUAGE plpgsql
    AS $$

DECLARE
	jsn  JSON;
	ret  TEXT;
	line TEXT;

BEGIN

	ret := '';

	FOR line IN EXECUTE FORMAT('EXPLAIN (FORMAT JSON) %s', query)
	LOOP
		ret := ret || line;
	END LOOP;

	jsn := ret::JSON;

	RAISE NOTICE '=====================';
	RAISE NOTICE '%',  json_typeof(jsn);
	RAISE NOTICE '=====================';
	-- RAISE NOTICE '%', jsn->0->'Plan'->'Node Type';

	return '';

END
$$;


--
-- Name: explain_as_table(jsonb, integer); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.explain_as_table(json_explain jsonb, parent_id_arg integer DEFAULT NULL::integer) RETURNS TABLE(node_id integer, parent_id integer, node_type text, parent_relationship text, parallel_aware boolean, async_capable boolean, join_type text, startup_cost double precision, total_cost double precision, plan_rows integer, plan_width integer, inner_unique boolean, hash_cond text, subplan_name text)
    LANGUAGE plpgsql
    AS $$

DECLARE
	node     JSONB;
	child_id INT;

BEGIN
	node_id             := COALESCE((SELECT MAX(node_id) FROM explain_as_table), 0) + 1;
	node                := json_explain->'Plan';
	parent_id           := parent_id_arg;
	node_type           := node->>'Node Type';
	parent_relationship := node->>'Parent Relationship';
	parallel_aware      := (node->>'Parallel Aware')::BOOLEAN;
	async_capable       := (node->>'Async Capable')::BOOLEAN;
	join_type           := node->>'Join Type';
	startup_cost        := (node->>'Startup Cost')::DOUBLE PRECISION;
	total_cost          := (node->>'Total Cost')::DOUBLE PRECISION;
	plan_rows           := (node->>'Plan Rows')::INT;
	plan_width          := (node->>'Plan Width')::INT;
	inner_unique        := (node->>'Inner Unique')::BOOLEAN;
	hash_cond           := node->>'Hash Cond';
	subplan_name        := node->>'Subplan Name';

	RETURN NEXT;

	FOR child_id IN SELECT generate_series(0, jsonb_array_length(json_explain->'Plans') - 1)
		LOOP
			RETURN QUERY
			SELECT explain_as_table(json_explain->'Plans'->child_id, node_id);
	END LOOP;

	RETURN;

END;
$$;


--
-- Name: explain_as_table(text, json, integer); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.explain_as_table(query text, json_explain json DEFAULT NULL::json, parent_id_arg integer DEFAULT NULL::integer) RETURNS TABLE(node_id integer, parent_id integer, node_type text, parent_relationship text, parallel_aware boolean, async_capable boolean, join_type text, startup_cost double precision, total_cost double precision, plan_rows integer, plan_width integer, inner_unique boolean, hash_cond text, subplan_name text)
    LANGUAGE plpgsql
    AS $$

DECLARE
	node     JSONB;
	child_id INT;

BEGIN

	IF query IS NOT NULL THEN
		SELECT explain_to_json(query) INTO json_explain;
	END IF;

	node_id             := COALESCE((SELECT MAX(node_id) FROM explain_as_table), 0) + 1;
	node                := json_explain->'Plan';
	parent_id           := parent_id_arg;
	node_type           := node->>'Node Type';
	parent_relationship := node->>'Parent Relationship';
	parallel_aware      := (node->>'Parallel Aware')::BOOLEAN;
	async_capable       := (node->>'Async Capable')::BOOLEAN;
	join_type           := node->>'Join Type';
	startup_cost        := (node->>'Startup Cost')::DOUBLE PRECISION;
	total_cost          := (node->>'Total Cost')::DOUBLE PRECISION;
	plan_rows           := (node->>'Plan Rows')::INT;
	plan_width          := (node->>'Plan Width')::INT;
	inner_unique        := (node->>'Inner Unique')::BOOLEAN;
	hash_cond           := node->>'Hash Cond';
	subplan_name        := node->>'Subplan Name';

	RETURN NEXT;

	FOR child_id IN SELECT generate_series(0, jsonb_array_length(json_explain->'Plans') - 1)
		LOOP
			RETURN QUERY
			SELECT explain_as_table(NULL, json_explain->'Plans'->child_id, node_id);
	END LOOP;

	RETURN;

END;
$$;


--
-- Name: explain_to_json(text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.explain_to_json(query text) RETURNS json
    LANGUAGE plpgsql
    AS $$

DECLARE
	tmp   TEXT;
	ret   TEXT;

BEGIN

	tmp := REGEXP_REPLACE(query, ';.*', '');
	EXECUTE FORMAT('EXPLAIN (FORMAT JSON) %s', tmp) INTO ret;
	RETURN ret::JSON;

END
$$;


--
-- Name: explain_view(text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.explain_view(view_name text) RETURNS text
    LANGUAGE plpgsql
    AS $$

DECLARE
	view_def  TEXT;
	ret       TEXT;

BEGIN

	SELECT
		view_definition
	FROM
		information_schema.views
	WHERE
		table_name = view_name
	INTO
		view_def;

	return explain(view_def);

END;
$$;


--
-- Name: find_candidate_type(anycompatible); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.find_candidate_type(datum anycompatible) RETURNS name[]
    LANGUAGE plpgsql
    AS $$

DECLARE
	type_name   NAME;
	valid_types NAME[];

BEGIN

	FOR type_name IN
		SELECT typname
		FROM   pg_type
		LOOP
			BEGIN
				RAISE NOTICE 'Checking out %.', type_name;
				EXECUTE FORMAT('SELECT CAST(%s AS %s)',
					datum,
					type_name
				);
				/*
			EXCEPTION WHEN OTHERS
				THEN
					RAISE NOTICE 'CALLED.';
					CONTINUE;
				*/
			END;

			PERFORM ARRAY_APPEND(valid_types, type_name);
		END LOOP;
	
	RETURN valid_types;

END
$$;


--
-- Name: find_free_name(text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.find_free_name(input_name text) RETURNS name
    LANGUAGE plpgsql
    AS $$

DECLARE
	ret            TEXT;
	schema_name    TEXT;
	base_name      TEXT;
	candidate_name TEXT;
	counter        INT := 0;
BEGIN

	IF      (input_name LIKE '%.%.%')
	THEN
		RAISE EXCEPTION 'Input name should not contain more than one period'; -- Improve message
	ELSIF (input_name LIKE '%.%')
	THEN
		schema_name := SPLIT_PART(input_name, '.', 1);
		base_name   := SPLIT_PART(input_name, '.', 2);
	ELSE
		base_name   := input_name;
		schema_name := CURRENT_SCHEMA;
	END IF;

	candidate_name := base_name;

	WHILE EXISTS (
		SELECT 1
		FROM   pg_class     AS c
		JOIN   pg_namespace AS nsp
		ON     relnamespace = nsp.oid
		WHERE  nspname      = schema_name
		AND    relname      = candidate_name
	)
		LOOP
			counter        := counter + 1;
			candidate_name := base_name || '_' || counter;
		END LOOP;

	RETURN candidate_name::NAME;

END;

$$;


--
-- Name: find_free_temp_table_name(text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.find_free_temp_table_name(input_name text) RETURNS name
    LANGUAGE plpgsql
    AS $$

DECLARE
	schema_oid     OID;
	candidate_name TEXT;
	counter        INT := 0;
BEGIN

	IF      (input_name LIKE '%.%')
	THEN
		RAISE EXCEPTION 'Cannot qualify the temporary table by schema.'
		USING HINT    = 'Get rid of the . in the name.';
	END IF;

	schema_oid := pg_my_temp_schema();

	IF schema_oid = 0
	THEN
		RETURN input_name::NAME;
	END IF;

	candidate_name := input_name;

	WHILE EXISTS (
		SELECT 1
		FROM   pg_class     AS c
		WHERE  relnamespace = schema_oid
		AND    relname      = candidate_name
	)
		LOOP
			counter        := counter + 1;
			candidate_name := input_name || '_' || counter;
		END LOOP;

	RETURN candidate_name::NAME;

END;

$$;


--
-- Name: find_in_path(name, name); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.find_in_path(input_schema name, input_name name DEFAULT NULL::name) RETURNS rule_0.qualified_name
    LANGUAGE sql
    AS $$

WITH


fixed_targets AS (
	SELECT

		CASE
			WHEN input_name IS NULL
				THEN NULL
			ELSE
				input_schema
		END                     AS target_schema,

		COALESCE(
			input_name,
			input_schema
		)                       AS target_name

),


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
	the_path      tp
NATURAL JOIN
	the_stuff     ts
JOIN
	fixed_targets ft
ON
	ft.target_name   = ts.object_name
AND (
	ft.target_schema = ts.schema_name
	OR
	ft.target_schema IS NULL
)
;

$$;


--
-- Name: generate_record_views(); Type: PROCEDURE; Schema: rule_0; Owner: -
--

CREATE PROCEDURE rule_0.generate_record_views()
    LANGUAGE plpgsql
    AS $$

DECLARE
	view_name   TEXT;
	view_schema TEXT;
	view_def    TEXT;

BEGIN

	FOR
		view_schema,
		view_name,
		view_def
	IN
		SELECT
			*
		FROM
			define_record_views()
	LOOP
		RAISE NOTICE
			'Creating view: %.%.',
			view_schema,
			view_name;
		EXECUTE(view_def);
	END LOOP;

END;
$$;


--
-- Name: generate_soft_casts(); Type: PROCEDURE; Schema: rule_0; Owner: -
--

CREATE PROCEDURE rule_0.generate_soft_casts()
    LANGUAGE plpgsql
    AS $_$

DECLARE
	template    TEXT;
	type_schema TEXT;
	type_name   TEXT;
	qual_type   TEXT;
	type_count  INTEGER;

BEGIN


---------------- BEGIN TEMPLATE --------------
	template := $level_1$

CREATE OR REPLACE FUNCTION rule_0.to_%s(
	datum ANYCOMPATIBLE
)
RETURNS  %s
LANGUAGE PLPGSQL
AS $soft_cast$

BEGIN

	BEGIN
		RETURN datum::%s;
	EXCEPTION WHEN INVALID_TEXT_REPRESENTATION
		THEN RETURN NULL;
	END;

END
$soft_cast$;

$level_1$;
----------------  END TEMPLATE --------------

	type_count := 0;

	FOR type_name, type_schema IN
		SELECT DISTINCT
			typname,
			nspname
		FROM   pg_type
		JOIN   pg_namespace nsp
		ON     typnamespace = nsp.oid
		WHERE  typname     !~ '^_'
		AND    typname     !~ '^pg_'
		AND    typcategory != 'Z'
		AND    typcategory != 'X'
		AND    typcategory != 'P'

	LOOP
		qual_type = type_schema || '.' || type_name;
		--RAISE NOTICE '%', FORMAT(template, type_name, qual_type, qual_type);
		--RAISE NOTICE '-----------------------------------';
		EXECUTE FORMAT(template, type_name, qual_type, qual_type);
		type_count = type_count + 1;
	END LOOP;

	RAISE NOTICE 'Generated soft casts for % types.', type_count;

END
$_$;


--
-- Name: get_ancestor_materialized_views(rule_0.qualified_name); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_ancestor_materialized_views(schema_name rule_0.qualified_name) RETURNS TABLE(source_schema name, source_materialized_view name)
    LANGUAGE plpgsql
    AS $$

BEGIN
	RETURN QUERY
	SELECT *
	FROM rule_0.get_ancestor_materialized_views(
		schema_name.schema_name,
		schema_name.object_name
	)
	;
END;
$$;


--
-- Name: get_ancestor_materialized_views(name, name); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_ancestor_materialized_views(schema_name name, relation_name name DEFAULT NULL::name) RETURNS TABLE(source_schema_name name, source_materialized_view name)
    LANGUAGE plpgsql
    AS $$

BEGIN

	RETURN QUERY
		SELECT
			source_schema,
			source_relation AS source_materialized_view
		FROM
			get_ancestor_relations(schema_name, relation_name)
		WHERE
			source_relation_type = 'Materialized View';

END;
$$;


--
-- Name: get_ancestor_relations(rule_0.qualified_name); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_ancestor_relations(schema_name rule_0.qualified_name) RETURNS TABLE(source_schema name, source_relation name, source_relation_type text)
    LANGUAGE plpgsql
    AS $$

BEGIN
	RETURN QUERY
	SELECT *
	FROM rule_0.get_ancestor_relations(
		schema_name.schema_name,
		schema_name.object_name
	)
	;
END;
$$;


--
-- Name: get_ancestor_relations(name, name); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_ancestor_relations(schema_name name, relation_name name DEFAULT NULL::name) RETURNS TABLE(source_schema name, source_relation name, source_relation_type text)
    LANGUAGE plpgsql
    AS $$

BEGIN
	IF relation_name IS NULL
	THEN
		relation_name := schema_name;
		schema_name   := (rule_0.find_in_path(relation_name)).schema_name;
	END IF;

RETURN QUERY
WITH RECURSIVE dependent_relations AS (
	SELECT
		d.source_schema,
		d.source_relation,
		d.source_relation_type
	FROM  view_dependencies d
	WHERE d.dependent_schema = schema_name
	AND   d.dependent_view   = relation_name

	UNION ALL

	SELECT
		vd.source_schema,
		vd.source_relation,
		vd.source_relation_type
	FROM view_dependencies vd
	INNER JOIN dependent_relations dr
	ON  vd.dependent_schema    = dr.source_schema
	AND vd.dependent_view      = dr.source_relation
	AND vd.dependent_view_type = dr.source_relation_type
)
SELECT DISTINCT * FROM dependent_relations;
END;
$$;


--
-- Name: get_composite_object_fields(anyelement); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_composite_object_fields(object anyelement) RETURNS TABLE(field name)
    LANGUAGE sql
    AS $$
	SELECT
		get_composite_type_fields(
			pg_typeof(object)::TEXT
		);
$$;


--
-- Name: get_composite_object_fields(text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_composite_object_fields(oname text) RETURNS TABLE(field name)
    LANGUAGE sql
    AS $$
	SELECT
		get_composite_type_fields(
			pg_typeof(oname)::TEXT
		);
$$;


--
-- Name: get_composite_type_fields(text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_composite_type_fields(tname text) RETURNS TABLE(field name)
    LANGUAGE sql
    AS $$
	SELECT
		attname  AS field
	FROM     pg_type
	JOIN     pg_attribute
	ON       typrelid     = attrelid
	JOIN     pg_namespace nsp
	ON       typnamespace = nsp.oid
	JOIN     pg_class c
	ON       c.oid        = attrelid
	WHERE    typcategory  = 'C'
	AND      attnum       > 0
	AND      relname      = tname
	ORDER BY attnum;
$$;


--
-- Name: get_qualified_name(name, name); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_qualified_name(object_schema name, object_name name DEFAULT NULL::name) RETURNS rule_0.qualified_name
    LANGUAGE plpgsql
    AS $_$

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

$_$;


--
-- Name: get_relation(regclass); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_relation(rel regclass) RETURNS name
    LANGUAGE plpgsql
    AS $$
DECLARE
	rel_name TEXT;
	BEGIN
		SELECT c.relname
		INTO   rel_name
		FROM   pg_class c
		JOIN   pg_namespace n
		ON     c.relnamespace = n.oid
		WHERE  c.oid          = rel;

		RETURN rel_name;
END;
$$;


--
-- Name: get_schema(regclass); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_schema(tbl regclass) RETURNS text
    LANGUAGE plpgsql
    AS $$
DECLARE
	schema_name TEXT;
BEGIN
	SELECT n.nspname
	INTO   schema_name
	FROM   pg_class c
	JOIN   pg_namespace n
	ON     c.relnamespace = n.oid
	WHERE  c.oid          = tbl;

	RETURN schema_name;
END;
$$;


--
-- Name: get_schema(regcollation); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_schema(regcollation) RETURNS regnamespace
    LANGUAGE sql
    AS $_$
	SELECT collnamespace::REGNAMESPACE FROM pg_collation WHERE oid = $1;
$_$;


--
-- Name: get_schema(regconfig); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_schema(regconfig) RETURNS regnamespace
    LANGUAGE sql
    AS $_$
	SELECT cfgnamespace::REGNAMESPACE FROM pg_ts_config  WHERE oid = $1;
$_$;


--
-- Name: get_schema(regdictionary); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_schema(regdictionary) RETURNS regnamespace
    LANGUAGE sql
    AS $_$
	SELECT dictnamespace::REGNAMESPACE FROM pg_ts_dict   WHERE oid = $1;
$_$;


--
-- Name: get_schema(regnamespace); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_schema(regnamespace) RETURNS regnamespace
    LANGUAGE sql
    AS $_$
-- Schemata don't belong to schemata?  How to handle this?  I say it's idempotent.
	SELECT $1;
$_$;


--
-- Name: get_schema(regoper); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_schema(regoper) RETURNS regnamespace
    LANGUAGE sql
    AS $_$
	SELECT  oprnamespace::REGNAMESPACE FROM pg_operator  WHERE oid = $1;
$_$;


--
-- Name: get_schema(regoperator); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_schema(regoperator) RETURNS regnamespace
    LANGUAGE sql
    AS $_$
	SELECT  oprnamespace::REGNAMESPACE FROM pg_operator  WHERE oid = $1;
$_$;


--
-- Name: get_schema(regproc); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_schema(regproc) RETURNS regnamespace
    LANGUAGE sql
    AS $_$
	SELECT  pronamespace::REGNAMESPACE FROM pg_proc      WHERE oid = $1;
$_$;


--
-- Name: get_schema(regprocedure); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_schema(regprocedure) RETURNS regnamespace
    LANGUAGE sql
    AS $_$
	SELECT  pronamespace::REGNAMESPACE FROM pg_proc      WHERE oid = $1;
$_$;


--
-- Name: get_schema(regrole); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_schema(regrole) RETURNS regnamespace
    LANGUAGE sql
    AS $$
-- Roles don't belong to schemata.  Consider raising a warning on this.
	SELECT NULL::REGNAMESPACE;
$$;


--
-- Name: get_schema(regtype); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.get_schema(regtype) RETURNS regnamespace
    LANGUAGE sql
    AS $_$
	SELECT  typnamespace::REGNAMESPACE FROM pg_type      WHERE oid = $1;
$_$;


--
-- Name: grant_select_on_new_schema(); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.grant_select_on_new_schema() RETURNS event_trigger
    LANGUAGE plpgsql
    AS $$
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
$$;


--
-- Name: load_unzipped_text(text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.load_unzipped_text(zip_filename text) RETURNS void
    LANGUAGE plpgsql
    AS $$
DECLARE
	temp_filename text := '/tmp/unzipped_file.txt';
	text_data text;
BEGIN
	-- Extract the ZIP file to a temporary file
	-- EXECUTE format('unzip -p %s > %s', zip_filename, temp_filename);
	EXECUTE format('touch %s', '/tmp/foobar');

	-- Read the unzipped content from the temporary file
	text_data := pg_read_binary_file(temp_filename);

	-- Insert the unzipped text data into the table
	INSERT INTO unzipped_text (text_content) VALUES (text_data);

	-- Clean up the temporary file
	EXECUTE format('rm %s', temp_filename);
END;
$$;


--
-- Name: materialized_view_refresh_sequence(); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.materialized_view_refresh_sequence() RETURNS TABLE(ordinality rule_0.counting_number, schema_name name, materialized_view_name name)
    LANGUAGE sql
    AS $$

WITH RECURSIVE

topological_sort AS (
	SELECT
		s.source_schema        AS schema_name,
		s.source_relation      AS relation_name,
		s.source_relation_type AS relation_type,
		1                      AS level
	FROM
		view_dependencies    AS s
	WHERE (
		s.source_schema,
		s.source_relation
	)
	NOT IN (
		SELECT
			d.dependent_schema,
			d.dependent_view
		FROM
			view_dependencies AS d
	)

	UNION ALL

	SELECT
		t.dependent_schema    AS schema_name,
		t.dependent_view      AS relation_name,
		t.dependent_view_type AS relation_type,
		ts.level + 1
	FROM
		view_dependencies t
	INNER JOIN
		topological_sort ts
	ON  t.source_schema        = ts.schema_name
	AND t.source_relation      = ts.relation_name
	AND t.source_relation_type = ts.relation_type
),

uniq AS (
	SELECT DISTINCT ON (schema_name, relation_name)
		ts.schema_name,
		ts.relation_name,
		ts.relation_type,
		ROW_NUMBER() OVER () AS sparse_ordinality
	FROM topological_sort ts
	ORDER BY
		ts.schema_name,
		ts.relation_name
),

clean AS (
	SELECT
		ROW_NUMBER() OVER (ORDER BY sparse_ordinality) AS ordinality,
		u.schema_name,
		u.relation_name
	FROM
		uniq u
	WHERE
		u.relation_type = 'Materialized View'
)
SELECT * FROM clean ORDER BY ordinality;

$$;


--
-- Name: materialized_view_refresh_sequence(name, name); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.materialized_view_refresh_sequence(schema_name_arg name, mat_view name DEFAULT NULL::name) RETURNS TABLE(ordinality rule_0.counting_number, schema_name name, materialized_view_name name)
    LANGUAGE plpgsql
    AS $$

BEGIN

	IF mat_view IS NULL
	THEN
		mat_view := schema_name_arg;
		schema_name_arg := (rule_0.find_in_path(mat_view)).schema_name;
	END IF;

	RETURN QUERY

	WITH
	seq AS (
		SELECT
			rs.ordinality             AS ord,
			rs.schema_name            AS sch,
			rs.materialized_view_name AS mat
		FROM rule_0.materialized_view_refresh_sequence() rs
	),
	ancestors AS (
		SELECT
			ROW_NUMBER() OVER (ORDER BY ord) AS ordinalitee,
			source_schema_name               AS ret_schema_name,
			source_materialized_view         AS ret_matview
		FROM
			rule_0.get_ancestor_materialized_views(
				schema_name_arg,
				mat_view
			)
		JOIN seq AS s
		ON   source_schema_name       = sch
		AND  source_materialized_view = mat
	)
	,
	self AS (
		SELECT
			(MAX(ordinalitee) + 1)::counting_number AS ordinalitee,
			schema_name_arg                         AS ret_schema_name,
			mat_view                                AS ret_matview
		FROM
			ancestors
		LIMIT
			1
	)
	SELECT
		ordinalitee::rule_0.counting_number,
		ret_schema_name,
		ret_matview
	FROM ancestors a
	UNION
	SELECT
		ordinalitee,
		ret_schema_name,
		ret_matview
	FROM self
	ORDER BY ordinalitee;

END;
$$;


--
-- Name: move_view(regclass, text); Type: PROCEDURE; Schema: rule_0; Owner: -
--

CREATE PROCEDURE rule_0.move_view(IN view_name regclass, IN new_name text)
    LANGUAGE plpgsql
    AS $$

BEGIN
	CALL rule_0.copy_view(view_name, new_name);
	EXECUTE 'DROP VIEW ' ||  view_name;
END;
$$;


--
-- Name: named_field_must_exist_tg(); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.named_field_must_exist_tg() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
	CALL meta.field_must_exist(NEW.view_name, NEW.field_name);
	RETURN NEW;
END;
$$;


--
-- Name: object_type(name, name); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.object_type(sch name, obj name DEFAULT NULL::name) RETURNS text
    LANGUAGE plpgsql
    AS $$

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
$$;


--
-- Name: or_default(integer); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.or_default(integer) RETURNS text
    LANGUAGE plpgsql
    AS $_$
BEGIN
	IF($1 IS NULL)
		THEN RETURN 'DEFAULT';
	END IF;

	RETURN $1;
END;
$_$;


--
-- Name: or_default(text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.or_default(str text) RETURNS text
    LANGUAGE plpgsql
    AS $$
BEGIN
	IF(str IS NULL)
		THEN RETURN 'DEFAULT';
	END IF;

	RETURN quote_literal(str);
END;
$$;


--
-- Name: parse_execution_plan(jsonb, rule_0.natural_number); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.parse_execution_plan(plan_json jsonb, parent_id rule_0.natural_number DEFAULT 1) RETURNS TABLE(node rule_0.natural_number, parent rule_0.natural_number, node_type text, startup_cost numeric, total_cost numeric)
    LANGUAGE plpgsql
    AS $$

DECLARE
	subplan JSONB;
	key     TEXT;
	value   JSONB;
	row     RECORD;

BEGIN

	IF jsonb_typeof(plan_json) = 'array' THEN
		FOR subplan IN SELECT * FROM jsonb_array_elements(plan_json)
		LOOP
			RETURN QUERY SELECT * FROM parse_execution_plan(subplan, parent_id);
		END LOOP;
		RETURN;
	END IF;

	IF plan_json->'Plan' IS NOT NULL THEN
		RETURN QUERY SELECT * FROM parse_execution_plan(plan_json->'Plan', 1);
		RETURN;
	END IF;

	RETURN QUERY
		SELECT
			1::natural_number,
			parent_id,
			TRANSLATE((plan_json->'Node Type')::TEXT, '"', ''),
			(plan_json->'Startup Cost')::NUMERIC,
			(plan_json->'Total Cost'  )::NUMERIC
	;

	IF plan_json->'Plans' IS NULL THEN
		RETURN;
	END IF;

	FOR row IN
		SELECT
			(pep.parent + ROW_NUMBER() OVER ())::natural_number AS node,
			pep.node AS parent,
			pep.node_type,
			pep.startup_cost,
			pep.total_cost
		FROM parse_execution_plan(plan_json->'Plans', parent_id) AS pep
	LOOP
		node         := row.node;
		parent       := row.parent;
		node_type    := row.node_type;
		startup_cost := row.startup_cost;
		total_cost   := row.total_cost;
		RETURN NEXT;
	END LOOP;

-- PREVIOUS OKAY STATE

END;
$$;


--
-- Name: push_to_search_path(name); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.push_to_search_path(schema_name name) RETURNS text
    LANGUAGE sql
    AS $$
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
$$;


--
-- Name: push_to_search_path_trigger(); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.push_to_search_path_trigger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
	BEGIN
	PERFORM rule_0.push_to_search_path(NEW.schema_name::NAME);
	RETURN NEW;
	END;
$$;


--
-- Name: qualified_name(text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.qualified_name(object_name text) RETURNS rule_0.qualified_name
    LANGUAGE plpgsql
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

$$;


--
-- Name: qualified_name(text, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.qualified_name(object_schema text, object_name text) RETURNS rule_0.qualified_name
    LANGUAGE plpgsql
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

$$;


--
-- Name: random_string(); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.random_string() RETURNS text
    LANGUAGE plpgsql
    AS $$
DECLARE
	alphabet TEXT := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
	random_string_length INTEGER := 5 + floor(random() * 6); -- Random length between 5 and 10
	random_string TEXT := '';
	i INTEGER;
BEGIN
	FOR i IN 1..random_string_length LOOP
		random_string := random_string || substr(alphabet, floor(random() * length(alphabet) + 1)::integer, 1);
	END LOOP;

	RETURN random_string;
END;
$$;


--
-- Name: refresh_materialized_view_tree(name, name); Type: PROCEDURE; Schema: rule_0; Owner: -
--

CREATE PROCEDURE rule_0.refresh_materialized_view_tree(IN matsch name, IN matview name DEFAULT NULL::name)
    LANGUAGE plpgsql
    AS $$

DECLARE
	next_matview NAME;
	next_schema  NAME;

BEGIN
	IF
		matview IS NULL
	THEN
		matview := matsch;
		matsch  := (rule_0.find_in_path(matview)).schema_name;
	END IF;

	IF
		rule_0.object_type(matview) != 'Materialized View'
	THEN
		RAISE EXCEPTION '% is not a materialized view.', matview;
	END IF;

	FOR next_schema, next_matview
	IN
		SELECT
			schema_name,
			materialized_view_name
	FROM
		rule_0.materialized_view_refresh_sequence(matview)
	ORDER BY
		ordinality
	LOOP
		RAISE NOTICE 'Refreshing %.%', next_schema, next_matview;
		EXECUTE FORMAT(
			'REFRESH MATERIALIZED VIEW %s.%s',
			next_schema,
			next_matview
		);
	END LOOP;
END;

$$;


--
-- Name: refresh_materialized_views(text); Type: PROCEDURE; Schema: rule_0; Owner: -
--

CREATE PROCEDURE rule_0.refresh_materialized_views(IN p_schema text)
    LANGUAGE plpgsql
    AS $$
DECLARE
	view_count   INT;
	current_view INT := 0;
	view_name    TEXT;
	start_time   TIMESTAMPTZ;
	end_time     TIMESTAMPTZ;
BEGIN
	SELECT COUNT(*)
	INTO   view_count
	FROM   pg_catalog.pg_matviews
	WHERE  schemaname = p_schema;

	IF view_count = 0 THEN
		RAISE NOTICE 'No materialized views found in schema %.', p_schema;
		RETURN;
	END IF;

	RAISE NOTICE 'Refreshing % materialized views in schema %.', view_count, p_schema;

	FOR view_name IN
		SELECT matviewname
		FROM   pg_catalog.pg_matviews
		WHERE  schemaname = p_schema
	LOOP
		current_view := current_view + 1;
		RAISE NOTICE 'Refreshing % (%/%)', view_name, current_view, view_count;
		start_time := clock_timestamp();
		EXECUTE format('REFRESH MATERIALIZED VIEW %I.%I', p_schema, view_name);
		COMMIT;
		end_time := clock_timestamp();
		RAISE NOTICE 'Refresh of % complete.',    view_name;
	END LOOP;

	RAISE NOTICE 'Refresh of all materialized views in schema % complete.', p_schema;
END;
$$;


--
-- Name: reg_does_not_match_text(regclass, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(regclass, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT != $2;
$_$;


--
-- Name: reg_does_not_match_text(regcollation, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(regcollation, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT != $2;
$_$;


--
-- Name: reg_does_not_match_text(regconfig, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(regconfig, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT != $2;
$_$;


--
-- Name: reg_does_not_match_text(regdictionary, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(regdictionary, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT != $2;
$_$;


--
-- Name: reg_does_not_match_text(regnamespace, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(regnamespace, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT != $2;
$_$;


--
-- Name: reg_does_not_match_text(regoper, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(regoper, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT != $2;
$_$;


--
-- Name: reg_does_not_match_text(regoperator, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(regoperator, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT != $2;
$_$;


--
-- Name: reg_does_not_match_text(regproc, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(regproc, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT != $2;
$_$;


--
-- Name: reg_does_not_match_text(regprocedure, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(regprocedure, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT != $2;
$_$;


--
-- Name: reg_does_not_match_text(regrole, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(regrole, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT != $2;
$_$;


--
-- Name: reg_does_not_match_text(regtype, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(regtype, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT != $2;
$_$;


--
-- Name: reg_does_not_match_text(text, regclass); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regclass) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT != $1;
$_$;


--
-- Name: reg_does_not_match_text(text, regcollation); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regcollation) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT != $1;
$_$;


--
-- Name: reg_does_not_match_text(text, regconfig); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regconfig) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT != $1;
$_$;


--
-- Name: reg_does_not_match_text(text, regdictionary); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regdictionary) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT != $1;
$_$;


--
-- Name: reg_does_not_match_text(text, regnamespace); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regnamespace) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT != $1;
$_$;


--
-- Name: reg_does_not_match_text(text, regoper); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regoper) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT != $1;
$_$;


--
-- Name: reg_does_not_match_text(text, regoperator); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regoperator) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT != $1;
$_$;


--
-- Name: reg_does_not_match_text(text, regproc); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regproc) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT != $1;
$_$;


--
-- Name: reg_does_not_match_text(text, regprocedure); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regprocedure) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT != $1;
$_$;


--
-- Name: reg_does_not_match_text(text, regrole); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regrole) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT != $1;
$_$;


--
-- Name: reg_does_not_match_text(text, regtype); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regtype) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT != $1;
$_$;


--
-- Name: reg_matches_text(regclass, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(regclass, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT = $2;
$_$;


--
-- Name: reg_matches_text(regcollation, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(regcollation, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT = $2;
$_$;


--
-- Name: reg_matches_text(regconfig, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(regconfig, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT = $2;
$_$;


--
-- Name: reg_matches_text(regdictionary, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(regdictionary, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT = $2;
$_$;


--
-- Name: reg_matches_text(regnamespace, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(regnamespace, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT = $2;
$_$;


--
-- Name: reg_matches_text(regoper, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(regoper, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT = $2;
$_$;


--
-- Name: reg_matches_text(regoperator, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(regoperator, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT = $2;
$_$;


--
-- Name: reg_matches_text(regproc, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(regproc, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT = $2;
$_$;


--
-- Name: reg_matches_text(regprocedure, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(regprocedure, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT = $2;
$_$;


--
-- Name: reg_matches_text(regrole, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(regrole, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT = $2;
$_$;


--
-- Name: reg_matches_text(regtype, text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(regtype, text) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $1::TEXT = $2;
$_$;


--
-- Name: reg_matches_text(text, regclass); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(text, regclass) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: reg_matches_text(text, regcollation); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(text, regcollation) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: reg_matches_text(text, regconfig); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(text, regconfig) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: reg_matches_text(text, regdictionary); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(text, regdictionary) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: reg_matches_text(text, regnamespace); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(text, regnamespace) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: reg_matches_text(text, regoper); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(text, regoper) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: reg_matches_text(text, regoperator); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(text, regoperator) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: reg_matches_text(text, regproc); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(text, regproc) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: reg_matches_text(text, regprocedure); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(text, regprocedure) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: reg_matches_text(text, regrole); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(text, regrole) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: reg_matches_text(text, regtype); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.reg_matches_text(text, regtype) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: regexp_delete(text, text[]); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.regexp_delete(base_string text, VARIADIC patterns text[]) RETURNS text
    LANGUAGE plpgsql
    AS $$
DECLARE
	result_string TEXT;
	pattern_text  TEXT;
BEGIN
	result_string := base_string;

	FOR pattern_text IN SELECT unnest(patterns)
	LOOP
		IF pattern_text IS NOT NULL THEN
			result_string := regexp_replace(result_string, pattern_text, '', 'g');
		END IF;
	END LOOP;

	RETURN
		result_string;

END;
$$;


--
-- Name: restart_column_sequence(regclass, name); Type: PROCEDURE; Schema: rule_0; Owner: -
--

CREATE PROCEDURE rule_0.restart_column_sequence(IN tblname regclass, IN colname name)
    LANGUAGE plpgsql
    AS $$

DECLARE
	seq NAME;
	sch NAME;

BEGIN

	SELECT
		schema_name,
		sequence_name
	FROM
		meta.column_sequences
	WHERE
		schema_name = get_schema(tblname)
	AND
		table_name  = get_relation(tblname)
	AND
		column_name = colname
	INTO
		sch,
		seq;

	EXECUTE FORMAT('
		ALTER SEQUENCE
			%s.%s
		RESTART
			1;',
		sch,
		seq
	);

END
$$;


--
-- Name: safe_cast(anycompatible, anyelement); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.safe_cast(ival anycompatible, dval anyelement) RETURNS anyelement
    LANGUAGE plpgsql
    AS $$

BEGIN
	IF
		PG_INPUT_IS_VALID(
			ival::TEXT,
			PG_TYPEOF(dval)::TEXT
		)
	THEN
		RETURN ival;
	ELSE
		RETURN dval;
	END IF;
END;
$$;


--
-- Name: save_search_path(); Type: PROCEDURE; Schema: rule_0; Owner: -
--

CREATE PROCEDURE rule_0.save_search_path()
    LANGUAGE plpgsql
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
$$;


--
-- Name: sed(text, text, text, text[]); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.sed(base_string text, pattern text, replacement text, VARIADIC patterns_and_replacements text[]) RETURNS text
    LANGUAGE plpgsql
    AS $$

DECLARE
	result_string text;
	i integer;
	pattern_text text;
	replacement_text text;

BEGIN
	result_string := regexp_replace(base_string, pattern, replacement, 'g');

	FOR i IN 1 .. cardinality(patterns_and_replacements) / 2
		LOOP
			pattern_text := patterns_and_replacements[i * 2 - 1];
			replacement_text := patterns_and_replacements[i * 2];

			IF pattern_text IS NOT NULL AND replacement_text IS NOT NULL THEN
				result_string := regexp_replace(result_string, pattern_text, replacement_text, 'g');
	END IF;
END LOOP;

RETURN result_string;
END;
$$;


--
-- Name: system_exec(text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.system_exec(command text) RETURNS TABLE(result text)
    LANGUAGE plpgsql
    AS $$

DECLARE
	copy_cmd TEXT;
	ret_rec  RECORD;
	tmp_tbl  NAME;
	tmp_nsp  OID;

BEGIN

	tmp_tbl := find_free_temp_table_name('system_exec_output');

	EXECUTE
		FORMAT ('
			CREATE TEMPORARY TABLE %s (
				return_value TEXT
			)
			',
			tmp_tbl
		);

	copy_cmd := FORMAT(
		'COPY %s(return_value)
		FROM PROGRAM ''%s'';',
		tmp_tbl,
		command
	);

	EXECUTE copy_cmd;

	RETURN QUERY
		SELECT return_value
		FROM   system_exec_output;

	DROP TABLE system_exec_output;

END
$$;


--
-- Name: table_cast_options(regnamespace, regclass); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.table_cast_options(schema_name regnamespace, tbl_name regclass) RETURNS TABLE(column_name name, type_name regtype)
    LANGUAGE sql
    AS $$

WITH

field_list(column_name, column_number) AS (
	SELECT column_name, ordinal_position
	FROM   information_schema.columns
	WHERE  table_schema::TEXT = schema_name::TEXT
	AND    table_name::TEXT   = tbl_name::TEXT
)

SELECT
	column_name,
	rule_0.column_cast_options(
		schema_name::NAME,
		tbl_name::NAME,
		column_name::NAME
	)::REGTYPE
	AS cast_option
FROM
	field_list
ORDER BY
	column_name,
	column_number,
	rule_0.column_cast_options(
		schema_name::NAME,
		tbl_name::NAME,
		column_name::NAME
	)
;

$$;


--
-- Name: tco(regnamespace, regclass); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.tco(schema_name regnamespace, tbl_name regclass) RETURNS TABLE(column_name name, type_name regtype)
    LANGUAGE sql
    AS $$

WITH

field_list(column_name, column_number) AS (
	SELECT column_name, ordinal_position
	FROM   information_schema.columns
	WHERE  table_schema::TEXT = schema_name::TEXT
	AND    table_name::TEXT   = tbl_name::TEXT
)

SELECT
	column_name,
	rule_0.column_cast_options(
		schema_name::NAME,
		tbl_name::NAME,
		column_name::NAME
	)::REGTYPE
	AS cast_option
FROM
	field_list
ORDER BY
	column_name,
	column_number,
	rule_0.column_cast_options(
		schema_name::NAME,
		tbl_name::NAME,
		column_name
	)
;

$$;


--
-- Name: text_matches_reg(text, regclass); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.text_matches_reg(text, regclass) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: text_matches_reg(text, regcollation); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.text_matches_reg(text, regcollation) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: text_matches_reg(text, regconfig); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.text_matches_reg(text, regconfig) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: text_matches_reg(text, regdictionary); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.text_matches_reg(text, regdictionary) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: text_matches_reg(text, regnamespace); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.text_matches_reg(text, regnamespace) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: text_matches_reg(text, regoper); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.text_matches_reg(text, regoper) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: text_matches_reg(text, regoperator); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.text_matches_reg(text, regoperator) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: text_matches_reg(text, regproc); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.text_matches_reg(text, regproc) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: text_matches_reg(text, regprocedure); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.text_matches_reg(text, regprocedure) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: text_matches_reg(text, regrole); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.text_matches_reg(text, regrole) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: text_matches_reg(text, regtype); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.text_matches_reg(text, regtype) RETURNS boolean
    LANGUAGE sql
    AS $_$
	SELECT $2::TEXT = $1;
$_$;


--
-- Name: try_cast_double(text); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.try_cast_double(inp text) RETURNS double precision
    LANGUAGE plpgsql IMMUTABLE
    AS $$
  BEGIN
    BEGIN
      RETURN inp::double precision;
    EXCEPTION
      WHEN OTHERS THEN RETURN NULL;
    END;
  END;
$$;


--
-- Name: update_cache_partition(regnamespace, regnamespace); Type: PROCEDURE; Schema: rule_0; Owner: -
--

CREATE PROCEDURE rule_0.update_cache_partition(IN from_schema regnamespace, IN to_schema regnamespace DEFAULT NULL::regnamespace)
    LANGUAGE plpgsql
    AS $$

DECLARE
	from_tbl REGCLASS;
	to_tbl   REGCLASS;
	sql      TEXT;
	records  rule_0.natural_number;

BEGIN

	sql = '
	WITH ins AS (
		INSERT INTO %s.%s
		SELECT      *
		FROM        %s.%s
		RETURNING   1
	)
	SELECT COUNT(*) FROM ins;
';

	IF to_schema IS NULL THEN
		to_schema := (from_schema::TEXT || '_cache')::REGNAMESPACE;
	END IF;

	FOR from_tbl IN
		SELECT viewname
		FROM   pg_catalog.pg_views
		WHERE  schemaname = from_schema::TEXT
	LOOP
		to_tbl := from_tbl::TEXT || '_cache';
		RAISE NOTICE 'COPYING FROM %.% to %.%.',
			from_schema,
			from_tbl,
			to_schema,
			to_tbl;
		EXECUTE FORMAT(
			sql,
			to_schema,
			to_tbl,
			from_schema,
			from_tbl
		)
		INTO records;

		RAISE NOTICE 'Done.  Records copied: %',
			TO_CHAR(records, 'FM9,999,999');
	END LOOP;

END
$$;


--
-- Name: update_search_path(name, name); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.update_search_path(old_name name, new_name name) RETURNS text
    LANGUAGE sql
    AS $$
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
$$;


--
-- Name: update_search_path_trigger(); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.update_search_path_trigger() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
	BEGIN
	PERFORM rule_0.update_search_path(OLD.schema_name, NEW.schema_name);
	RETURN NEW;
	END;
$$;


--
-- Name: view_has_descendants(name, name); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.view_has_descendants(schema_name name, view_name name DEFAULT NULL::name) RETURNS boolean
    LANGUAGE plpgsql
    AS $$

DECLARE

	ret       BOOLEAN;
	qual_name qualified_name;

BEGIN

	qual_name := rule_0.find_in_path(schema_name, view_name);

	IF view_name IS NULL
	THEN
		view_name   := schema_name;
		schema_name := (rule_0.find_in_path(view_name)).schema_name;
	END IF;

	IF schema_name IS NULL
	THEN
		RAISE NOTICE 'View not found: %', view_name;
	END IF;

	IF rule_0.object_type(schema_name, view_name) !~ 'View'
	THEN
		RAISE EXCEPTION
			'%.% does not seem to be a view.',
			schema_name,
			view_name;
	END IF;

	SELECT COUNT(*) > 0
	FROM meta.view_dependencies
	WHERE source_schema   = schema_name
	AND   source_relation = view_name
	INTO ret;

	RETURN ret;

END;
$$;


--
-- Name: view_name_must_be_a_view_tg(); Type: FUNCTION; Schema: rule_0; Owner: -
--

CREATE FUNCTION rule_0.view_name_must_be_a_view_tg() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
	CALL meta.must_be_a(NEW.view_name, 'View');
	RETURN NEW;
END;
$$;


--
-- Name: clear_and_copy(text, text); Type: PROCEDURE; Schema: update; Owner: -
--

CREATE PROCEDURE update.clear_and_copy(IN schema_name text, IN table_name text)
    LANGUAGE plpgsql
    AS $$

DECLARE
	source    text;
	target    text;
	age       integer;
	copy_info copy%ROWTYPE;
BEGIN

	SELECT *
	INTO   copy_info
	FROM   update.copy
	WHERE  target_schema = schema_name
	AND    target_table  = table_name;

	source = copy_info.source_schema || '.' || copy_info.source_relation;
	target =           schema_name   || '.' ||           table_name;
	age    = copy_info.max_age;

	EXECUTE 'DELETE FROM ' || target;
	EXECUTE 'INSERT INTO ' || target || ' SELECT * FROM ' || source || ' WHERE run_date >= now()::date - ' || age;

END;

$$;


--
-- Name: copy_new(text, text, text); Type: FUNCTION; Schema: update; Owner: -
--

CREATE FUNCTION update.copy_new(source text, target text, dfield text DEFAULT 'run_date'::text) RETURNS integer
    LANGUAGE plpgsql
    AS $_$

DECLARE
	latest  DATE;
	sql     TEXT;
	cond    TEXT;
	records INTEGER;
BEGIN

	sql = 'SELECT MAX(' || dfield || ') FROM ' || target;

	EXECUTE sql INTO latest;

	cond = '';

	IF(latest IS NOT NULL)
		THEN cond = 'WHERE ' || dfield || ' > ' || quote_literal(latest);
	END IF;

	sql = $sql$
		WITH ins AS (
			INSERT   INTO $sql$ || target || $sql$
			SELECT * FROM $sql$ || source || ' ' ||
			cond || $sql$
			RETURNING *
		)
		SELECT COUNT(*)
		FROM   ins;
	$sql$;

	EXECUTE sql INTO records;

	RETURN records AS record_count;

END;

$_$;


--
-- Name: delete_all_and_record(text, text, text); Type: PROCEDURE; Schema: update; Owner: -
--

CREATE PROCEDURE update.delete_all_and_record(IN target text, IN script text DEFAULT 'unknown script'::text, IN entry text DEFAULT 'Called delete_all_and_record().'::text)
    LANGUAGE plpgsql
    AS $_$

DECLARE
	sql       TEXT;
	namespace TEXT;
	tblname   TEXT;
	records   INTEGER;
BEGIN

	namespace = split_part(target, '.',  1);
	tblname   = split_part(target, '.', -1);

	sql = $sql$
		WITH del AS (
			DELETE FROM $sql$ || target || $sql$
			RETURNING *
		)
		SELECT COUNT(*)
		FROM   del;
	$sql$;

	EXECUTE sql INTO records;

	INSERT INTO log.table_log(
		script_name,
		table_schema,
		table_name,
		table_action,
		record_count,
		entry
	)
	VALUES (
		script,
		namespace,
		tblname,
		'Delete',
		records,
		entry
	);

END;

$_$;


--
-- Name: refresh_materialized_view(name, name); Type: PROCEDURE; Schema: update; Owner: -
--

CREATE PROCEDURE update.refresh_materialized_view(IN view_schema name, IN view_name name DEFAULT NULL::name)
    LANGUAGE plpgsql
    AS $$

	DECLARE
		q_name  qualified_name;
		s_time  TIME WITH TIME ZONE;
		s_date  DATE;
		e_time  TIME WITH TIME ZONE;
		e_date  DATE;

	BEGIN

		q_name := rule_0.get_qualified_name(view_schema, view_name);

		s_time = CLOCK_TIMESTAMP()::TIMETZ;
		s_date = CLOCK_TIMESTAMP()::DATE;

		RAISE NOTICE 'Refreshing %', q_name.qualified_name;
		EXECUTE 'REFRESH MATERIALIZED VIEW ' || q_name.qualified_name;

		e_time = CLOCK_TIMESTAMP()::TIMETZ;
		e_date = CLOCK_TIMESTAMP()::DATE;

		INSERT INTO log.mat_view_refresh (
			view_schema,
			view_name,
			start_date,
			start_time,
			end_date,
			end_time
		)
		VALUES (
			q_name.schema_name,
			q_name.rel_name,
			s_date,
			s_time,
			e_date,
			e_time
		);

	END;

$$;


--
-- Name: refresh_materialized_views(text[]); Type: PROCEDURE; Schema: update; Owner: -
--

CREATE PROCEDURE update.refresh_materialized_views(VARIADIC views text[])
    LANGUAGE plpgsql
    AS $$

DECLARE
view       text;
start_time timestamptz;
end_time   timestamptz;

BEGIN

FOREACH view IN ARRAY views LOOP
	SELECT clock_timestamp() into start_time;
	PERFORM pg_sleep(2);
	SELECT clock_timestamp() into end_time;

	RAISE INFO 'Start time: %', start_time;
	RAISE INFO 'Refreshed:  %', view;
	RAISE INFO 'End time:   %', end_time;
END LOOP;

END;

$$;


--
-- Name: refresh_materialized_views(text); Type: PROCEDURE; Schema: update; Owner: -
--

CREATE PROCEDURE update.refresh_materialized_views(IN v_group text DEFAULT 'Default'::text)
    LANGUAGE plpgsql
    AS $$

	DECLARE
		temprow RECORD;

	BEGIN

		FOR temprow IN
			SELECT
				view_schema,
				view_name
			FROM
				update.refresh_sequence
			WHERE
				view_group = v_group
			ORDER BY
				ordinality ASC
		LOOP
			CALL refresh_materialized_view(
				temprow.view_schema,
				temprow.view_name
			);
		END LOOP;
	END;

$$;


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = regclass,
    RIGHTARG = text
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = regcollation,
    RIGHTARG = text
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = regconfig,
    RIGHTARG = text
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = regdictionary,
    RIGHTARG = text
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = regnamespace,
    RIGHTARG = text
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = regoper,
    RIGHTARG = text
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = regoperator,
    RIGHTARG = text
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = regproc,
    RIGHTARG = text
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = regprocedure,
    RIGHTARG = text
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = regrole,
    RIGHTARG = text
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = regtype,
    RIGHTARG = text
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = text,
    RIGHTARG = regclass
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = text,
    RIGHTARG = regcollation
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = text,
    RIGHTARG = regconfig
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = text,
    RIGHTARG = regdictionary
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = text,
    RIGHTARG = regnamespace
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = text,
    RIGHTARG = regoper
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = text,
    RIGHTARG = regoperator
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = text,
    RIGHTARG = regproc
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = text,
    RIGHTARG = regprocedure
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = text,
    RIGHTARG = regrole
);


--
-- Name: <>; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.<> (
    FUNCTION = rule_0.reg_does_not_match_text,
    LEFTARG = text,
    RIGHTARG = regtype
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.reg_matches_text,
    LEFTARG = regclass,
    RIGHTARG = text
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.reg_matches_text,
    LEFTARG = regcollation,
    RIGHTARG = text
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.reg_matches_text,
    LEFTARG = regconfig,
    RIGHTARG = text
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.reg_matches_text,
    LEFTARG = regdictionary,
    RIGHTARG = text
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.reg_matches_text,
    LEFTARG = regnamespace,
    RIGHTARG = text
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.reg_matches_text,
    LEFTARG = regoper,
    RIGHTARG = text
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.reg_matches_text,
    LEFTARG = regoperator,
    RIGHTARG = text
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.reg_matches_text,
    LEFTARG = regproc,
    RIGHTARG = text
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.reg_matches_text,
    LEFTARG = regprocedure,
    RIGHTARG = text
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.reg_matches_text,
    LEFTARG = regrole,
    RIGHTARG = text
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.reg_matches_text,
    LEFTARG = regtype,
    RIGHTARG = text
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.text_matches_reg,
    LEFTARG = text,
    RIGHTARG = regclass
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.text_matches_reg,
    LEFTARG = text,
    RIGHTARG = regcollation
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.text_matches_reg,
    LEFTARG = text,
    RIGHTARG = regconfig
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.text_matches_reg,
    LEFTARG = text,
    RIGHTARG = regdictionary
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.text_matches_reg,
    LEFTARG = text,
    RIGHTARG = regnamespace
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.text_matches_reg,
    LEFTARG = text,
    RIGHTARG = regoper
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.text_matches_reg,
    LEFTARG = text,
    RIGHTARG = regoperator
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.text_matches_reg,
    LEFTARG = text,
    RIGHTARG = regproc
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.text_matches_reg,
    LEFTARG = text,
    RIGHTARG = regprocedure
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.text_matches_reg,
    LEFTARG = text,
    RIGHTARG = regrole
);


--
-- Name: =; Type: OPERATOR; Schema: rule_0; Owner: -
--

CREATE OPERATOR rule_0.= (
    FUNCTION = rule_0.text_matches_reg,
    LEFTARG = text,
    RIGHTARG = regtype
);


SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: amtype; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.amtype (
    amtype "char" NOT NULL,
    access_method_type text NOT NULL
);


--
-- Name: access_method; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.access_method AS
 SELECT am.oid AS access_method_id,
    am.amname AS access_method_name,
    nsp.nspname AS function_schema,
    func.proname AS function_name,
    amtype.access_method_type
   FROM (((pg_am am
     JOIN meta_lookup.amtype USING (amtype))
     JOIN pg_proc func ON (((am.amhandler)::oid = func.oid)))
     JOIN pg_namespace nsp ON ((func.pronamespace = nsp.oid)));


--
-- Name: backend_pid; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.backend_pid AS
 SELECT pg_backend_pid() AS pg_backend_pid;


--
-- Name: castcontext; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.castcontext (
    castcontext "char" NOT NULL,
    cast_context text NOT NULL
);


--
-- Name: castmethod; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.castmethod (
    castmethod "char" NOT NULL,
    cast_method text NOT NULL
);


--
-- Name: casts; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.casts AS
 WITH func AS (
         SELECT pg_proc.oid AS castfunc,
            pg_proc.proname AS function_name
           FROM pg_proc
        UNION
         SELECT 0,
            NULL::name AS name
        ), typ AS (
         SELECT typ.oid AS type_id,
            nsp.nspname AS type_schema,
            typ.typname AS type_name
           FROM (pg_type typ
             JOIN pg_namespace nsp ON ((typ.typnamespace = nsp.oid)))
        )
 SELECT pg_cast.oid AS cast_id,
    from_typ.type_schema AS source_type_schema,
    from_typ.type_name AS source_type,
    to_typ.type_schema AS target_type_schema,
    to_typ.type_name AS target_type,
    func.function_name,
    castcontext.cast_context,
    castmethod.cast_method
   FROM (((((pg_cast
     JOIN meta_lookup.castcontext USING (castcontext))
     JOIN meta_lookup.castmethod USING (castmethod))
     JOIN func USING (castfunc))
     JOIN typ from_typ ON ((pg_cast.castsource = from_typ.type_id)))
     JOIN typ to_typ ON ((pg_cast.casttarget = to_typ.type_id)));


--
-- Name: column_sequences; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.column_sequences AS
 SELECT (t.relnamespace)::regnamespace AS schema_name,
    (t.oid)::regclass AS table_name,
    a.attname AS column_name,
    s.relname AS sequence_name
   FROM (((pg_class t
     JOIN pg_attribute a ON ((a.attrelid = t.oid)))
     JOIN pg_depend d ON (((d.refobjid = t.oid) AND (d.refobjsubid = a.attnum))))
     JOIN pg_class s ON ((s.oid = d.objid)))
  WHERE ((d.classid = ('pg_class'::regclass)::oid) AND (d.refclassid = ('pg_class'::regclass)::oid) AND (d.deptype = ANY (ARRAY['i'::"char", 'a'::"char"])) AND (t.relkind = ANY (ARRAY['r'::"char", 'p'::"char"])) AND (s.relkind = 'S'::"char"));


--
-- Name: columns_simple; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.columns_simple AS
 SELECT (c.relnamespace)::regnamespace AS schema_name,
    (c.oid)::regclass AS table_name,
    a.attname AS column_name,
    (t.oid)::regtype AS column_type,
    col_description(c.oid, (a.attnum)::integer) AS description
   FROM ((pg_attribute a
     JOIN pg_class c ON ((a.attrelid = c.oid)))
     JOIN pg_type t ON ((a.atttypid = t.oid)))
  WHERE ((a.attnum > 0) AND (a.atttypid <> (0)::oid) AND (a.attisdropped = false));


--
-- Name: composite_type; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.composite_type AS
 SELECT (relnamespace)::regnamespace AS schema_name,
    relname AS composite_type
   FROM pg_class cl
  WHERE (relkind = 'c'::"char")
  ORDER BY (relnamespace)::text, relname;


--
-- Name: composite_type_field; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.composite_type_field AS
 WITH comp_type AS (
         SELECT cl.oid AS attrelid,
            (cl.relnamespace)::regnamespace AS schema_name,
            cl.relname AS composite_type
           FROM pg_class cl
          WHERE (cl.relkind = 'c'::"char")
        )
 SELECT ct.schema_name,
    ct.composite_type,
    att.attname AS field_name,
    att.attnum AS field_number,
    ((((nsp.nspname)::text || '.'::text) || (typ.typname)::text))::regtype AS field_type
   FROM (((comp_type ct
     JOIN pg_attribute att USING (attrelid))
     JOIN pg_type typ ON ((att.atttypid = typ.oid)))
     JOIN pg_namespace nsp ON ((typ.typnamespace = nsp.oid)))
  ORDER BY ct.schema_name, ct.composite_type, att.attnum;


--
-- Name: domains; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.domains AS
 WITH nsp AS (
         SELECT pg_namespace.oid AS typnamespace,
            pg_namespace.nspname AS schema_name
           FROM pg_namespace
        ), coll AS (
         SELECT pg_collation.oid AS typcollation,
            (pg_collation.oid)::regcollation AS collation_type
           FROM pg_collation
        UNION
         SELECT 0 AS typcollation,
            NULL::regcollation AS collation_type
        ), base_type AS (
         SELECT pg_type.oid AS typbasetype,
            (pg_type.oid)::regtype AS base_type
           FROM (pg_type
             JOIN nsp USING (typnamespace))
        ), owner AS (
         SELECT pg_roles.oid AS typowner,
            (pg_roles.oid)::regrole AS owner
           FROM pg_roles
        ), dom AS (
         SELECT (typ.typnamespace)::regnamespace AS schema_name,
            (typ.oid)::regtype AS domain_name,
            owner.owner,
            typ.typcollation,
            typ.typbasetype,
            (NOT typ.typnotnull) AS nullable,
            (NULLIF(typ.typtypmod, '-1'::integer))::rule_0.counting_number AS base_mod,
            (NULLIF(typ.typndims, 0))::rule_0.counting_number AS dimensions,
            typ.typdefault AS default_value,
            typ.typdefaultbin AS default_expression
           FROM ((pg_type typ
             JOIN nsp USING (typnamespace))
             JOIN owner USING (typowner))
          WHERE (typ.typtype = 'd'::"char")
        )
 SELECT dom.schema_name,
    dom.domain_name,
    dom.owner,
    base_type.base_type,
    dom.base_mod,
    dom.nullable,
    dom.dimensions,
    dom.default_value,
    coll.collation_type
   FROM ((dom
     JOIN coll USING (typcollation))
     JOIN base_type USING (typbasetype))
  ORDER BY (dom.schema_name)::text, (dom.domain_name)::text;


--
-- Name: enum_value; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.enum_value AS
 SELECT t.typname AS enum_name,
    e.enumsortorder AS sort_order,
    e.enumlabel AS label
   FROM (pg_enum e
     JOIN pg_type t ON ((e.enumtypid = t.oid)))
  ORDER BY t.typname, e.enumsortorder;


--
-- Name: error_class; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.error_class (
    error_class character(2) NOT NULL,
    error_class_name text NOT NULL
);


--
-- Name: error_code; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.error_code (
    error_class character(2) NOT NULL,
    error_code character(5) NOT NULL,
    condition_name text NOT NULL
);


--
-- Name: error; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.error AS
 SELECT error_class.error_class_name,
    error_code.error_code,
    error_code.condition_name
   FROM (meta_lookup.error_class
     JOIN meta_lookup.error_code USING (error_class));


--
-- Name: extensions; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.extensions AS
 SELECT ext.oid AS extension_id,
    sch.nspname AS extension_schema,
    ext.extname AS extension_name,
    ext.extversion AS version,
    rolz.rolname AS extension_owner,
    ext.extrelocatable AS relocatable
   FROM ((pg_extension ext
     JOIN pg_namespace sch ON ((ext.extnamespace = sch.oid)))
     JOIN pg_roles rolz ON ((ext.extowner = rolz.oid)));


--
-- Name: attgenerated; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.attgenerated (
    attgenerated "char" NOT NULL,
    generated text NOT NULL
);


--
-- Name: attidentity; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.attidentity (
    attidentity "char" NOT NULL,
    identity text NOT NULL
);


--
-- Name: fields; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.fields AS
 SELECT (c.relnamespace)::regnamespace AS schema_name,
    c.relname AS relation_name,
    a.attnum AS field_order,
    a.attname AS field_name,
    (a.atttypid)::regtype AS field_type,
    (a.attnotnull = false) AS nullable,
    a.atthasdef AS has_default,
    COALESCE(attgenerated.generated, 'Unknown Value'::text) AS generated,
    COALESCE(attidentity.identity, 'Unknown Value'::text) AS identity
   FROM (((pg_attribute a
     JOIN pg_class c ON ((a.attrelid = c.oid)))
     LEFT JOIN meta_lookup.attidentity USING (attidentity))
     LEFT JOIN meta_lookup.attgenerated USING (attgenerated))
  WHERE (a.attnum > 0)
  ORDER BY ((c.relnamespace)::regnamespace)::text, c.relname, a.attnum;


--
-- Name: index_fields; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.index_fields AS
 SELECT (tbl.relnamespace)::regnamespace AS schema_name,
    tbl.relname AS relation,
    idx.relname AS index_name,
    array_agg(att.attname) AS fields
   FROM (((pg_index i
     JOIN pg_class idx ON ((i.indexrelid = idx.oid)))
     JOIN pg_class tbl ON ((i.indrelid = tbl.oid)))
     JOIN pg_attribute att ON (((att.attrelid = tbl.oid) AND (att.attnum = ANY ((i.indkey)::smallint[])))))
  GROUP BY (tbl.relnamespace)::regnamespace, tbl.relname, idx.relname, idx.relkind;


--
-- Name: language_functions; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.language_functions AS
 WITH func AS (
         SELECT f.oid AS func_id,
            s.nspname AS func_schema,
            f.proname AS func_name
           FROM (pg_proc f
             JOIN pg_namespace s ON ((f.pronamespace = s.oid)))
        UNION
         SELECT 0,
            NULL::name AS name,
            NULL::name AS name
        ), resolved AS (
         SELECT l.oid AS language_id,
            l.lanname AS language_name,
            h.func_schema AS handler_schema,
            h.func_name AS handler_name,
            i.func_schema AS inline_schema,
            i.func_name AS inline_name,
            v.func_schema AS validator_schema,
            v.func_name AS validator_name
           FROM ((((pg_language l
             JOIN pg_roles r ON ((l.lanowner = r.oid)))
             JOIN func v ON ((l.lanvalidator = v.func_id)))
             JOIN func i ON ((l.laninline = i.func_id)))
             JOIN func h ON ((l.lanplcallfoid = h.func_id)))
        )
 SELECT resolved.language_id,
    resolved.language_name,
    'handler'::text AS function_type,
    resolved.handler_schema AS function_schema,
    resolved.handler_name AS function_name
   FROM resolved
  WHERE (resolved.handler_name IS NOT NULL)
UNION
 SELECT resolved.language_id,
    resolved.language_name,
    'inline'::text AS function_type,
    resolved.inline_schema AS function_schema,
    resolved.inline_name AS function_name
   FROM resolved
  WHERE (resolved.inline_name IS NOT NULL)
UNION
 SELECT resolved.language_id,
    resolved.language_name,
    'validator'::text AS function_type,
    resolved.validator_schema AS function_schema,
    resolved.validator_name AS function_name
   FROM resolved
  WHERE (resolved.validator_name IS NOT NULL)
  ORDER BY 2, 3;


--
-- Name: languages; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.languages AS
 SELECT l.oid AS language_id,
    l.lanname AS language_name,
    r.rolname AS owner,
    (NOT l.lanispl) AS internal,
    l.lanpltrusted AS trusted
   FROM (pg_language l
     JOIN pg_roles r ON ((l.lanowner = r.oid)));


--
-- Name: materialized_view_age; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.materialized_view_age AS
 WITH path AS (
         SELECT (pg_settings.setting ||
                CASE
                    WHEN ("left"(pg_settings.setting, 1) = '/'::text) THEN '/'::text
                    ELSE '\'::text
                END) AS path
           FROM pg_settings
          WHERE (pg_settings.name = 'data_directory'::text)
        ), payload AS (
         SELECT (c.relnamespace)::regnamespace AS schema_name,
            c.relname AS materialized_view,
            (pg_stat_file((path.path || pg_relation_filepath((((((c.relnamespace)::regnamespace)::text || '.'::text) || (c.relname)::text))::regclass)))).modification AS refresh_time,
            (now() - (pg_stat_file((path.path || pg_relation_filepath((((((c.relnamespace)::regnamespace)::text || '.'::text) || (c.relname)::text))::regclass)))).modification) AS age
           FROM path,
            pg_class c
          WHERE (c.relkind = 'm'::"char")
        )
 SELECT schema_name,
    materialized_view,
    refresh_time,
    age
   FROM payload
  ORDER BY refresh_time;


--
-- Name: my_temp_schema; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.my_temp_schema AS
 SELECT pg_my_temp_schema() AS pg_my_temp_schema;


--
-- Name: operator_family; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.operator_family AS
 SELECT o.oid AS operator_family_id,
    s.nspname AS operator_family_schema,
    o.opfname AS operator_family,
    a.amname AS access_method,
    r.rolname AS owner
   FROM (((pg_opfamily o
     JOIN pg_namespace s ON ((o.opfnamespace = s.oid)))
     JOIN pg_am a ON ((o.opfmethod = a.oid)))
     JOIN pg_roles r ON ((o.opfowner = r.oid)));


--
-- Name: primary_key; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.primary_key AS
 SELECT NULLIF(c.conparentid, (0)::oid) AS parent_id,
    c.oid AS primary_key_id,
    c.conname AS primary_key_name,
    (c.connamespace)::regnamespace AS schema_name,
    cl.relname AS table_name,
    i.relname AS index_name,
    c.condeferred AS deferred,
    c.condeferrable AS "deferrable",
    c.conislocal AS local,
    c.connoinherit AS inheritable,
    c.coninhcount AS ancestor_count,
    array_agg(a.attname ORDER BY a.attnum) AS primary_key_columns
   FROM (((pg_constraint c
     JOIN pg_class cl ON ((c.conrelid = cl.oid)))
     JOIN pg_class i ON ((c.conindid = i.oid)))
     JOIN pg_attribute a ON (((c.conrelid = a.attrelid) AND (a.attnum = ANY (c.conkey)))))
  WHERE (c.contype = 'p'::"char")
  GROUP BY c.oid, (c.connamespace)::regnamespace, cl.relname, i.relname, c.conname, c.condeferred, c.condeferrable, c.conislocal, c.connoinherit, c.coninhcount, c.conkey;


--
-- Name: primary_keys; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.primary_keys AS
 WITH unnested AS (
         SELECT pg_class.oid AS attrelid,
            (pg_class.relnamespace)::regnamespace AS nspname,
            pg_class.relname,
            unnest(pg_constraint.conkey) AS attnum
           FROM ((pg_constraint
             JOIN pg_class ON ((pg_constraint.conrelid = pg_class.oid)))
             JOIN pg_namespace ON ((pg_class.relnamespace = pg_namespace.oid)))
          WHERE (pg_constraint.contype = 'p'::"char")
        ), normalized AS (
         SELECT u.nspname AS schema_name,
            u.relname AS table_name,
            u.attnum AS column_rank,
            a.attname AS column_name
           FROM (unnested u
             JOIN pg_attribute a USING (attrelid, attnum))
          ORDER BY u.nspname, u.relname, u.attnum
        )
 SELECT DISTINCT schema_name,
    table_name,
    array_agg(column_name) AS primary_key
   FROM normalized
  GROUP BY schema_name, table_name
  ORDER BY schema_name, table_name;


--
-- Name: view_dependencies; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.view_dependencies AS
 WITH relkind AS (
         SELECT (tbl.relkind)::"char" AS relkind,
            tbl.relation_type
           FROM ( VALUES ('r'::text,'Ordinary Table'::text), ('i'::text,'Index'::text), ('S'::text,'Sequence'::text), ('t'::text,'TOAST table'::text), ('v'::text,'View'::text), ('m'::text,'Materialized View'::text), ('c'::text,'Composite Type'::text), ('f'::text,'Foreign Table'::text), ('p'::text,'Partitioned Table'::text), ('I'::text,'Partitioned Index'::text)) tbl(relkind, relation_type)
        ), schemata AS (
         SELECT pg_namespace.oid AS relnamespace,
            pg_namespace.nspname AS schema_name
           FROM pg_namespace
        ), relations AS (
         SELECT pg_class.oid AS view_oid,
            schemata.schema_name,
            pg_class.relname AS view_name,
            relkind.relation_type
           FROM ((pg_class
             JOIN schemata USING (relnamespace))
             JOIN relkind USING (relkind))
        ), deps AS (
         SELECT pg_rewrite.ev_class AS dependent_oid,
            pg_depend.refobjid AS source_oid
           FROM (pg_depend
             JOIN pg_rewrite ON ((pg_depend.objid = pg_rewrite.oid)))
        ), dependencies AS (
         SELECT DISTINCT a.schema_name AS dependent_schema,
            a.view_name AS dependent_view,
            a.relation_type AS dependent_view_type,
            b.schema_name AS source_schema,
            b.view_name AS source_view,
            b.relation_type AS source_relation_type
           FROM ((deps d
             JOIN relations a ON ((d.dependent_oid = a.view_oid)))
             JOIN relations b ON ((d.source_oid = b.view_oid)))
        )
 SELECT dependent_schema,
    dependent_view,
    dependent_view_type,
    source_schema,
    source_view AS source_relation,
    source_relation_type
   FROM dependencies
  WHERE ((dependent_schema <> source_schema) OR (dependent_view <> source_view))
  ORDER BY dependent_schema, dependent_view, source_schema, source_view;


--
-- Name: relation_dependency_order; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.relation_dependency_order AS
 WITH RECURSIVE topological_sort AS (
         SELECT view_dependencies.source_schema AS schema_name,
            view_dependencies.source_relation AS relation_name,
            view_dependencies.source_relation_type AS relation_type,
            1 AS level
           FROM meta.view_dependencies
          WHERE (NOT ((view_dependencies.source_schema, view_dependencies.source_relation) IN ( SELECT view_dependencies_1.dependent_schema,
                    view_dependencies_1.dependent_view
                   FROM meta.view_dependencies view_dependencies_1)))
        UNION ALL
         SELECT t.dependent_schema AS schema_name,
            t.dependent_view AS relation_name,
            t.dependent_view_type AS relation_type,
            (ts.level + 1)
           FROM (meta.view_dependencies t
             JOIN topological_sort ts ON (((t.source_schema = ts.schema_name) AND (t.source_relation = ts.relation_name) AND (t.source_relation_type = ts.relation_type))))
        ), views_with_or_being_deps AS (
         SELECT DISTINCT view_dependencies.dependent_schema AS schema_name,
            view_dependencies.dependent_view AS relation_name
           FROM meta.view_dependencies
        UNION
         SELECT DISTINCT view_dependencies.source_schema AS schema_name,
            view_dependencies.source_relation AS relation_name
           FROM meta.view_dependencies
        ), sorted AS (
         SELECT DISTINCT ON (topological_sort.schema_name, topological_sort.relation_name) row_number() OVER () AS ord,
            topological_sort.schema_name,
            topological_sort.relation_name,
            topological_sort.relation_type
           FROM (topological_sort
             JOIN views_with_or_being_deps USING (schema_name, relation_name))
          ORDER BY topological_sort.schema_name, topological_sort.relation_name, topological_sort.level, topological_sort.relation_type
        )
 SELECT row_number() OVER () AS ordinality,
    schema_name,
    relation_name,
    relation_type
   FROM sorted
  ORDER BY ord;


--
-- Name: relation_list; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.relation_list AS
 SELECT n.nspname AS "Schema",
    c.relname AS "Name",
        CASE c.relkind
            WHEN 'r'::"char" THEN 'table'::text
            WHEN 'v'::"char" THEN 'view'::text
            WHEN 'm'::"char" THEN 'materialized view'::text
            WHEN 'i'::"char" THEN 'index'::text
            WHEN 'S'::"char" THEN 'sequence'::text
            WHEN 't'::"char" THEN 'TOAST table'::text
            WHEN 'f'::"char" THEN 'foreign table'::text
            WHEN 'p'::"char" THEN 'partitioned table'::text
            WHEN 'I'::"char" THEN 'partitioned index'::text
            ELSE NULL::text
        END AS "Type",
    pg_get_userbyid(c.relowner) AS "Owner"
   FROM ((pg_class c
     LEFT JOIN pg_namespace n ON ((n.oid = c.relnamespace)))
     LEFT JOIN pg_am am ON ((am.oid = c.relam)))
  WHERE ((c.relkind = ANY (ARRAY['r'::"char", 'p'::"char", 'v'::"char", 'm'::"char", 'S'::"char", 'f'::"char", ''::"char"])) AND (n.nspname <> 'pg_catalog'::name) AND (n.nspname !~ '^pg_toast'::text) AND (n.nspname <> 'information_schema'::name) AND pg_table_is_visible(c.oid))
  ORDER BY n.nspname, c.relname;


--
-- Name: relkind; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.relkind (
    relkind "char" NOT NULL,
    relation_type text NOT NULL
);


--
-- Name: relation_size; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.relation_size AS
 WITH tbl_names AS (
         SELECT c.oid,
            (c.relnamespace)::regnamespace AS table_schema,
            n.nspname,
            c.relname AS table_name,
            relkind.relation_type
           FROM ((pg_class c
             JOIN pg_namespace n ON ((c.relnamespace = n.oid)))
             JOIN meta_lookup.relkind USING (relkind))
        )
 SELECT relation_type,
    table_schema AS schema_name,
    table_name AS relation,
    pg_total_relation_size((oid)::regclass) AS bytes,
    pg_size_pretty(pg_total_relation_size((oid)::regclass)) AS size
   FROM tbl_names t
  WHERE ((relation_type ~ 'Table|Materialized'::text) AND (relation_type !~ 'Toast'::text))
  ORDER BY (table_schema)::text, table_name;


--
-- Name: role_setting; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.role_setting AS
 WITH dbs AS (
         SELECT pg_database.oid AS setdatabase,
            pg_database.datname AS database
           FROM pg_database
        UNION
         SELECT 0,
            '[All]'::name AS name
        ), rolz AS (
         SELECT (pg_roles.oid)::regrole AS role_name,
            pg_roles.oid AS setrole
           FROM pg_roles
        ), exploded AS (
         SELECT dbs.database,
            rolz.role_name,
            unnest(pg_db_role_setting.setconfig) AS config
           FROM ((pg_db_role_setting
             JOIN dbs USING (setdatabase))
             JOIN rolz USING (setrole))
        )
 SELECT database,
    role_name,
    "substring"(config, '^[^=]+'::text) AS setting,
    regexp_replace(config, '^[^=]+='::text, ''::text) AS value
   FROM exploded;


--
-- Name: row_level_policy; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.row_level_policy AS
 SELECT schemaname AS schema_name,
    tablename AS table_name,
    policyname AS policy_name,
    initcap(permissive) AS leniency,
    cmd AS command,
    qual AS record_qualifier,
    with_check AS write_check
   FROM pg_policies;


--
-- Name: schema_size; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.schema_size AS
 SELECT schema_name,
    sum(bytes) AS bytes,
    pg_size_pretty(sum(bytes)) AS size
   FROM meta.relation_size
  GROUP BY schema_name
  ORDER BY schema_name;


--
-- Name: schemata; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.schemata AS
 SELECT nsp.oid AS schema_id,
    (nsp.oid)::regnamespace AS schema_name,
    u.usename AS owner,
    obj_description(nsp.oid) AS description
   FROM (pg_namespace nsp
     JOIN pg_user u ON ((nsp.nspowner = u.usesysid)))
  ORDER BY nsp.nspname;


--
-- Name: search_path; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.search_path AS
 SELECT ordinality,
    schema_name
   FROM string_to_table(( SELECT current_setting('search_path'::text) AS current_setting), ', '::text) WITH ORDINALITY x(schema_name, ordinality)
  WHERE (schema_name <> ''::text);


--
-- Name: session_settings; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.session_settings AS
 WITH lookup AS (
         SELECT pg_namespace.oid AS nspoid,
            pg_namespace.nspname AS temporary_schema
           FROM pg_namespace
        UNION
         SELECT 0 AS nspoid,
            NULL::name AS temporary_schema
        ), tmp_schema AS (
         SELECT 'temporary_schema'::text AS parameter,
            lookup.temporary_schema AS value
           FROM lookup
          WHERE (lookup.nspoid = pg_my_temp_schema())
        )
 SELECT tmp_schema.parameter,
    tmp_schema.value
   FROM tmp_schema
UNION
 SELECT parameters.parameter,
    parameters.value
   FROM ( VALUES ('role'::text,(CURRENT_ROLE)::text), ('session_user'::text,(SESSION_USER)::text), ('system_user'::text,SYSTEM_USER), ('catalog'::text,(CURRENT_CATALOG)::text), ('schema'::text,(CURRENT_SCHEMA)::text), ('client_ip'::text,(inet_client_addr())::text), ('client_port'::text,(inet_client_port())::text), ('server_ip'::text,(inet_server_addr())::text), ('server_port'::text,(inet_server_port())::text), ('server_version'::text,version()), ('server_start_time'::text,(pg_postmaster_start_time())::text), ('backend_pid'::text,(pg_backend_pid())::text), ('jit_available'::text,(pg_jit_available())::text)) parameters(parameter, value)
  ORDER BY 1;


--
-- Name: simple_primary_key; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.simple_primary_key AS
 SELECT schema_name,
    table_name,
    primary_key_columns
   FROM meta.primary_key
  WHERE ((schema_name)::text <> 'pg_catalog'::text)
  ORDER BY schema_name, table_name;


--
-- Name: system_foreign_keys; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.system_foreign_keys AS
 SELECT fktable AS constrained_table,
    fkcols AS constrained_columns,
    pktable AS constraining_table,
    pkcols AS constraining_columns,
    is_array,
    is_opt AS zero_allowed
   FROM pg_get_catalog_foreign_keys() pg_get_catalog_foreign_keys(fktable, fkcols, pktable, pkcols, is_array, is_opt);


--
-- Name: table_size; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.table_size AS
 SELECT schemaname AS table_schema,
    tablename AS table_name,
    pg_relation_size(((((schemaname)::text || '.'::text) || (tablename)::text))::regclass) AS bytes,
    pg_size_pretty(pg_relation_size(((((schemaname)::text || '.'::text) || (tablename)::text))::regclass)) AS size
   FROM pg_tables t
  ORDER BY schemaname, tablename;


--
-- Name: relpersistence; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.relpersistence (
    relpersistence "char" NOT NULL,
    relation_persistence text NOT NULL
);


--
-- Name: tables_experiment; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.tables_experiment AS
 WITH target AS (
         SELECT rk.relation_type AS table_type,
            (c.relnamespace)::regnamespace AS schema_name,
            (c.oid)::regclass AS table_name,
            pg_get_userbyid(c.relowner) AS owner,
            c.relnatts AS field_count,
            rp.relation_persistence AS persistence,
            NULLIF(c.reltuples, ('-1'::integer)::double precision) AS row_estimate
           FROM ((pg_class c
             JOIN meta_lookup.relkind rk USING (relkind))
             JOIN meta_lookup.relpersistence rp USING (relpersistence))
          WHERE ((c.relkind)::text = ANY (ARRAY['r'::text, 'p'::text, 'f'::text, 't'::text]))
        )
 SELECT table_type,
    schema_name,
    table_name,
    owner,
    field_count,
    persistence,
    row_estimate
   FROM target t;


--
-- Name: tables_simple; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.tables_simple AS
 SELECT (c.relnamespace)::regnamespace AS schema_name,
    (c.oid)::regclass AS table_name,
    obj_description(c.oid) AS description,
    pg_get_userbyid(c.relowner) AS owner
   FROM (pg_class c
     LEFT JOIN pg_tablespace t ON ((t.oid = c.reltablespace)))
  WHERE (c.relkind = ANY (ARRAY['r'::"char", 'p'::"char"]));


--
-- Name: VIEW tables_simple; Type: COMMENT; Schema: meta; Owner: -
--

COMMENT ON VIEW meta.tables_simple IS 'A simple representation of the tables in the database';


--
-- Name: COLUMN tables_simple.schema_name; Type: COMMENT; Schema: meta; Owner: -
--

COMMENT ON COLUMN meta.tables_simple.schema_name IS 'Name of the schema containing the table';


--
-- Name: COLUMN tables_simple.table_name; Type: COMMENT; Schema: meta; Owner: -
--

COMMENT ON COLUMN meta.tables_simple.table_name IS 'Name of the table';


--
-- Name: COLUMN tables_simple.description; Type: COMMENT; Schema: meta; Owner: -
--

COMMENT ON COLUMN meta.tables_simple.description IS 'Description of the table';


--
-- Name: COLUMN tables_simple.owner; Type: COMMENT; Schema: meta; Owner: -
--

COMMENT ON COLUMN meta.tables_simple.owner IS 'The user that owns the table';


--
-- Name: tgenabled; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.tgenabled (
    tgenabled "char" NOT NULL,
    trigger_enabled_condition text NOT NULL
);


--
-- Name: tgtype; Type: VIEW; Schema: meta_lookup; Owner: -
--

CREATE VIEW meta_lookup.tgtype AS
 WITH tgtype_values AS (
         SELECT num.num
           FROM generate_series('-32768'::integer, 32767) num(num)
        ), scope AS (
         SELECT scope_values.scope_mod,
            scope_values.scope
           FROM ( VALUES (0,'STATEMENT'::text), (1,'EACH ROW'::text)) scope_values(scope_mod, scope)
        ), timing_mask AS (
         SELECT (timing_values.mask)::bit(7) AS timing_mask,
            timing_values.timing
           FROM ( VALUES (0,'AFTER'::text), (2,'BEFORE'::text), (64,'INSTEAD'::text), (66,NULL::text)) timing_values(mask, timing)
        ), operation_mask AS (
         SELECT (operation_values.operation_mask)::bit(4) AS op_mask,
            operation_values.operation
           FROM ( VALUES (1,'INSERT'::text), (2,'DELETE'::text), (4,'UPDATE'::text), (8,'TRUNCATE'::text)) operation_values(operation_mask, operation)
        )
 SELECT tgtype_values.num AS tgtype,
    scope.scope,
    operation_mask.operation,
    timing_mask.timing,
    ((operation_mask.operation IS NOT NULL) AND (timing_mask.timing IS NOT NULL)) AS valid
   FROM (((tgtype_values
     JOIN scope ON ((abs((tgtype_values.num % 2)) = scope.scope_mod)))
     LEFT JOIN operation_mask ON (((((tgtype_values.num >> 2))::bit(4) # operation_mask.op_mask) = (0)::bit(4))))
     JOIN timing_mask ON ((((tgtype_values.num)::bit(7) & (66)::bit(7)) = timing_mask.timing_mask)))
  ORDER BY tgtype_values.num;


--
-- Name: triggers; Type: VIEW; Schema: meta; Owner: -
--

CREATE VIEW meta.triggers AS
 WITH func AS (
         SELECT f.oid AS function_id,
            s.nspname AS function_schema,
            f.proname AS function_name
           FROM (pg_proc f
             JOIN pg_namespace s ON ((f.pronamespace = s.oid)))
        UNION
         SELECT 0,
            NULL::name AS name,
            NULL::name AS name
        ), payload AS (
         SELECT t.oid AS trigger_id,
            NULLIF(t.tgparentid, (0)::oid) AS parent_trigger_id,
            t.tgisinternal AS system,
            (t.tgconstraint <> (0)::oid) AS "constraint",
            t.tgname AS trigger_name,
            rule_0.get_schema((t.tgrelid)::regclass) AS relation_schema,
            rule_0.get_relation((t.tgrelid)::regclass) AS relation,
            NULLIF(ARRAY( SELECT att.attname
                   FROM (unnest((t.tgattr)::smallint[]) arr(arr)
                     JOIN pg_attribute att ON (((t.tgrelid = att.attrelid) AND (arr.arr = att.attnum))))), '{}'::name[]) AS columns,
            tgtype.timing,
            tgtype.operation,
            tgtype.scope,
            meta.get_trigger_when_clause(t.oid) AS when_condition,
            tgenabled.trigger_enabled_condition AS replication_condition,
            f.function_schema,
            f.function_name,
            t.tgnargs AS function_arg_count,
            t.tgargs AS argument_strings,
            t.tgoldtable AS old_table_name,
            t.tgnewtable AS new_table_name
           FROM (((pg_trigger t
             JOIN meta_lookup.tgtype USING (tgtype))
             JOIN meta_lookup.tgenabled USING (tgenabled))
             JOIN func f ON ((t.tgfoid = f.function_id)))
        )
 SELECT trigger_id,
    parent_trigger_id,
    system,
    "constraint" AS is_constraint,
    trigger_name,
    relation_schema,
    relation,
    columns,
    timing,
    operation,
    scope,
    when_condition,
    replication_condition,
    function_schema,
    function_name,
    function_arg_count,
    argument_strings,
    old_table_name,
    new_table_name
   FROM payload p;


--
-- Name: view_key_fields; Type: TABLE; Schema: meta; Owner: -
--

CREATE TABLE meta.view_key_fields (
    view_name regclass NOT NULL,
    key_name name NOT NULL,
    field_name name NOT NULL
);


--
-- Name: view_keys; Type: TABLE; Schema: meta; Owner: -
--

CREATE TABLE meta.view_keys (
    view_name regclass NOT NULL,
    key_name name NOT NULL
);


--
-- Name: aggfinalmodify; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.aggfinalmodify (
    aggfinalmodify "char" NOT NULL,
    aggregate_transition_state_modification_policy text CONSTRAINT aggfinalmodify_aggregate_transition_state_modification_not_null NOT NULL
);


--
-- Name: aggkind; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.aggkind (
    aggkind "char" NOT NULL,
    aggregate_kind text NOT NULL
);


--
-- Name: aggmfinalmodify; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.aggmfinalmodify (
    aggmfinalmodify "char" NOT NULL,
    aggregate_transition_state_modification_policy text CONSTRAINT aggmfinalmodify_aggregate_transition_state_modificatio_not_null NOT NULL
);


--
-- Name: amoppurpose; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.amoppurpose (
    amoppurpose "char" NOT NULL,
    access_method_operator_purpose text NOT NULL
);


--
-- Name: attalign; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.attalign (
    attalign "char" NOT NULL,
    attribute_alignment text NOT NULL
);


--
-- Name: attcompression; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.attcompression (
    attcompression "char" NOT NULL,
    attribute_compression text NOT NULL
);


--
-- Name: attstorage; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.attstorage (
    attstorage "char" NOT NULL,
    attribute_storage text NOT NULL,
    attribute_storage_notes text NOT NULL
);


--
-- Name: collprovider; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.collprovider (
    collprovider "char" NOT NULL,
    collation_provider text NOT NULL
);


--
-- Name: comment_targets; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.comment_targets (
    comment_target_type text NOT NULL
);


--
-- Name: confdeltype; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.confdeltype (
    confdeltype "char" NOT NULL,
    foreign_key_deletion_type text NOT NULL
);


--
-- Name: confmatchtype; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.confmatchtype (
    confmatchtype "char" NOT NULL,
    foreign_key_match_type text NOT NULL
);


--
-- Name: confupdtype; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.confupdtype (
    confupdtype "char" NOT NULL,
    foreign_key_update_type text NOT NULL
);


--
-- Name: contype; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.contype (
    contype "char" NOT NULL,
    constraint_type text NOT NULL
);


--
-- Name: datlocprovider; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.datlocprovider (
    datlocprovider "char" NOT NULL,
    locale_provider text NOT NULL
);


--
-- Name: defaclobjtype; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.defaclobjtype (
    defaclobjtype "char" NOT NULL,
    object_type text NOT NULL
);


--
-- Name: deptype; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.deptype (
    deptype "char" NOT NULL,
    dependency_type text NOT NULL
);


--
-- Name: ev_enabled; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.ev_enabled (
    ev_enabled "char" NOT NULL,
    enabling_event_mode text NOT NULL
);


--
-- Name: ev_type; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.ev_type (
    ev_type "char" NOT NULL,
    event_type text NOT NULL
);


--
-- Name: evtenabled; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.evtenabled (
    evtenabled "char" NOT NULL,
    enabling_event_mode text NOT NULL
);


--
-- Name: lock_conflicts; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.lock_conflicts (
    lock_level text NOT NULL,
    current_lock_mode text NOT NULL,
    conflicting_request text NOT NULL
);


--
-- Name: lock_level; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.lock_level (
    lock_level text NOT NULL
);


--
-- Name: lock_mode; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.lock_mode (
    lock_level text NOT NULL,
    lock_mode text NOT NULL,
    alternate_mode_name text
);


--
-- Name: message_level; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.message_level (
    message_level text NOT NULL
);


--
-- Name: oprkind; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.oprkind (
    oprkind "char" NOT NULL,
    operator_kind text NOT NULL
);


--
-- Name: partstrat; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.partstrat (
    partstrat "char" NOT NULL,
    partitioning_strategy text NOT NULL
);


--
-- Name: polcmd; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.polcmd (
    polcmd "char" NOT NULL,
    policy_command_type text NOT NULL
);


--
-- Name: privtype; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.privtype (
    privtype "char" NOT NULL,
    initial_privilege_creator text NOT NULL
);


--
-- Name: proargmode; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.proargmode (
    proargmode "char" NOT NULL,
    procedure_argument_mode text NOT NULL
);


--
-- Name: prokind; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.prokind (
    prokind "char" NOT NULL,
    procedure_kind text NOT NULL
);


--
-- Name: proparallel; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.proparallel (
    proparallel "char" NOT NULL,
    parallel_safety text NOT NULL
);


--
-- Name: provolatile; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.provolatile (
    provolatile "char" NOT NULL,
    volatility text NOT NULL
);


--
-- Name: relreplident; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.relreplident (
    relreplident "char" NOT NULL,
    relation_persistence text NOT NULL
);


--
-- Name: srsubstate; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.srsubstate (
    srsubstate "char" NOT NULL,
    subscribed_relation_state text NOT NULL
);


--
-- Name: substream; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.substream (
    substream "char" NOT NULL,
    substream_policy text NOT NULL
);


--
-- Name: subtwophasestate; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.subtwophasestate (
    subtwophasestate "char" NOT NULL,
    two_phase_mode_state text NOT NULL
);


--
-- Name: trigger_operation; Type: VIEW; Schema: meta_lookup; Owner: -
--

CREATE VIEW meta_lookup.trigger_operation AS
 SELECT DISTINCT operation
   FROM meta_lookup.tgtype
  WHERE (operation IS NOT NULL);


--
-- Name: trigger_scope; Type: VIEW; Schema: meta_lookup; Owner: -
--

CREATE VIEW meta_lookup.trigger_scope AS
 SELECT DISTINCT scope
   FROM meta_lookup.tgtype
  WHERE (scope IS NOT NULL);


--
-- Name: trigger_timing; Type: VIEW; Schema: meta_lookup; Owner: -
--

CREATE VIEW meta_lookup.trigger_timing AS
 SELECT DISTINCT timing
   FROM meta_lookup.tgtype
  WHERE (timing IS NOT NULL);


--
-- Name: typalign; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.typalign (
    typalign "char" NOT NULL,
    type_alignment text NOT NULL
);


--
-- Name: typcategory; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.typcategory (
    typcategory "char" NOT NULL,
    type_category name NOT NULL
);


--
-- Name: typstorage; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.typstorage (
    typstorage "char" NOT NULL,
    type_storage text NOT NULL,
    type_storage_notes text NOT NULL
);


--
-- Name: typtype; Type: TABLE; Schema: meta_lookup; Owner: -
--

CREATE TABLE meta_lookup.typtype (
    typtype "char" NOT NULL,
    type_type text NOT NULL
);


--
-- Name: column_privilege; Type: VIEW; Schema: security; Owner: -
--

CREATE VIEW security.column_privilege AS
 WITH rolz AS (
         SELECT pg_roles.rolname AS role_name,
            pg_roles.oid AS role_id
           FROM pg_roles
        UNION ALL
         SELECT 'PUBLIC'::name AS role_name,
            0 AS role_id
        ), cols AS (
         SELECT (rule_0.get_schema((pg_attribute.attrelid)::regclass))::regnamespace AS schema_name,
            rule_0.get_relation((pg_attribute.attrelid)::regclass) AS relation,
            pg_attribute.attname AS column_name,
            aclexplode(pg_attribute.attacl) AS vals
           FROM pg_attribute
          WHERE (pg_attribute.attacl IS NOT NULL)
        )
 SELECT cols.schema_name,
    cols.relation,
    cols.column_name,
    tor.role_name AS grantor,
    tee.role_name AS grantee,
    (cols.vals).privilege_type AS privilege,
    (cols.vals).is_grantable AS can_grant
   FROM ((cols
     JOIN rolz tor ON ((tor.role_id = (cols.vals).grantor)))
     JOIN rolz tee ON ((tee.role_id = (cols.vals).grantee)))
  ORDER BY cols.schema_name, cols.relation, cols.column_name, tee.role_name, (cols.vals).privilege_type;


--
-- Name: database_privileges; Type: VIEW; Schema: security; Owner: -
--

CREATE VIEW security.database_privileges AS
 WITH base AS (
         SELECT r.usename AS grantor,
            e.usename AS grantee,
            p.datname AS database,
            a.privilege_type,
            a.is_grantable AS grantable
           FROM (((pg_database p
             JOIN LATERAL ( SELECT x.grantor,
                    x.grantee,
                    x.privilege_type,
                    x.is_grantable
                   FROM aclexplode(p.datacl) x(grantor, grantee, privilege_type, is_grantable)) a ON (true))
             JOIN pg_user e ON ((a.grantee = e.usesysid)))
             JOIN pg_user r ON ((a.grantor = r.usesysid)))
        )
 SELECT grantor,
    grantee,
    database,
    privilege_type,
    grantable
   FROM base;


--
-- Name: privileges_old; Type: VIEW; Schema: security; Owner: -
--

CREATE VIEW security.privileges_old AS
 WITH privs AS (
         SELECT pg_class.relname,
            pg_class.relnamespace AS oid,
            pg_class.relkind,
            pg_class.relacl
           FROM pg_class
          WHERE (pg_class.relacl IS NOT NULL)
        ), with_schema AS (
         SELECT pg_namespace.nspname AS schema,
            privs.relname,
            privs.relkind,
            privs.relacl AS acl
           FROM (privs
             JOIN pg_namespace USING (oid))
        ), with_kind AS (
         SELECT with_schema.schema,
            with_schema.relname,
            relkind.relation_type,
            with_schema.acl
           FROM (with_schema
             JOIN meta_lookup.relkind USING (relkind))
        )
 SELECT r.usename AS grantor,
    e.usename AS grantee,
    ws.schema,
    ws.relname,
    ws.relation_type,
    a.privilege_type,
    a.grantable
   FROM (((with_kind ws
     JOIN LATERAL ( SELECT x.grantor,
            x.grantee,
            x.privilege_type,
            x.grantable
           FROM aclexplode(ws.acl) x(grantor, grantee, privilege_type, grantable)) a ON (true))
     JOIN pg_user e ON ((a.grantee = e.usesysid)))
     JOIN pg_user r ON ((a.grantor = r.usesysid)));


--
-- Name: relation_privilege; Type: VIEW; Schema: security; Owner: -
--

CREATE VIEW security.relation_privilege AS
 WITH rolz AS (
         SELECT pg_roles.rolname AS role_name,
            pg_roles.oid AS role_id
           FROM pg_roles
        UNION ALL
         SELECT 'PUBLIC'::name AS role_name,
            0 AS role_id
        ), objects AS (
         SELECT (rule_0.get_schema((pg_class.oid)::regclass))::regnamespace AS schema_name,
            pg_class.relname AS relation,
            pg_class.relkind,
            aclexplode(pg_class.relacl) AS vals
           FROM pg_class
          WHERE (pg_class.relacl IS NOT NULL)
        )
 SELECT objects.schema_name,
    objects.relation,
    relkind.relation_type,
    tor.role_name AS grantor,
    tee.role_name AS grantee,
    (objects.vals).privilege_type AS privilege,
    (objects.vals).is_grantable AS can_grant
   FROM (((objects
     JOIN meta_lookup.relkind USING (relkind))
     JOIN rolz tor ON ((tor.role_id = (objects.vals).grantor)))
     JOIN rolz tee ON ((tee.role_id = (objects.vals).grantee)))
  ORDER BY objects.schema_name, objects.relation, tee.role_name, relkind.relation_type;


--
-- Name: role_membership; Type: VIEW; Schema: security; Owner: -
--

CREATE VIEW security.role_membership AS
 SELECT oid AS role_membership_id,
    pg_get_userbyid(roleid) AS group_role,
    pg_get_userbyid(member) AS member_role,
    pg_get_userbyid(grantor) AS grantor,
    admin_option AS can_grant,
    inherit_option AS inherits,
    set_option AS can_set_role
   FROM pg_auth_members;


--
-- Name: roles; Type: VIEW; Schema: security; Owner: -
--

CREATE VIEW security.roles AS
 SELECT oid AS role_id,
    (oid)::regrole AS role_name,
    rolinherit AS role_inherits
   FROM pg_roles;


--
-- Name: row_privilege; Type: VIEW; Schema: security; Owner: -
--

CREATE VIEW security.row_privilege AS
 SELECT schemaname AS schema_name,
    tablename AS table_name,
    policyname AS policy_name,
    unnest(roles) AS role
   FROM pg_policies
  ORDER BY (unnest(roles));


--
-- Name: schema_privileges_old; Type: VIEW; Schema: security; Owner: -
--

CREATE VIEW security.schema_privileges_old AS
 WITH base AS (
         SELECT r.usename AS grantor,
            e.usename AS grantee,
            p.nspname AS schema,
            a.privilege_type,
            a.is_grantable AS grantable
           FROM (((pg_namespace p
             JOIN LATERAL ( SELECT x.grantor,
                    x.grantee,
                    x.privilege_type,
                    x.is_grantable
                   FROM aclexplode(p.nspacl) x(grantor, grantee, privilege_type, is_grantable)) a ON (true))
             JOIN pg_user e ON ((a.grantee = e.usesysid)))
             JOIN pg_user r ON ((a.grantor = r.usesysid)))
        )
 SELECT grantor,
    grantee,
    schema,
    privilege_type,
    grantable
   FROM base;


--
-- Name: system_privilege; Type: VIEW; Schema: security; Owner: -
--

CREATE VIEW security.system_privilege AS
 SELECT (pg_roles.oid)::regrole AS role_name,
    'BYPASS PERMISSIONS'::text AS system_privilege
   FROM pg_roles
  WHERE pg_roles.rolsuper
UNION
 SELECT (pg_roles.oid)::regrole AS role_name,
    'CREATE ROLE'::text AS system_privilege
   FROM pg_roles
  WHERE pg_roles.rolcreaterole
UNION
 SELECT (pg_roles.oid)::regrole AS role_name,
    'CREATE DATABASE'::text AS system_privilege
   FROM pg_roles
  WHERE pg_roles.rolcreatedb
UNION
 SELECT (pg_roles.oid)::regrole AS role_name,
    'LOG IN'::text AS system_privilege
   FROM pg_roles
  WHERE pg_roles.rolcanlogin
UNION
 SELECT (pg_roles.oid)::regrole AS role_name,
    'MANAGE REPLICATION'::text AS system_privilege
   FROM pg_roles
  WHERE pg_roles.rolreplication
UNION
 SELECT (pg_roles.oid)::regrole AS role_name,
    'BYPASS ROW-LEVEL SECURITY'::text AS system_privilege
   FROM pg_roles
  WHERE pg_roles.rolbypassrls
  ORDER BY 1, 2;


--
-- Name: table_privileges_old; Type: VIEW; Schema: security; Owner: -
--

CREATE VIEW security.table_privileges_old AS
 SELECT grantor,
    grantee,
    table_schema AS schema,
    table_name,
    privilege_type,
    is_grantable AS grantable
   FROM information_schema.role_table_grants;


--
-- Name: users; Type: VIEW; Schema: security; Owner: -
--

CREATE VIEW security.users AS
 SELECT oid AS user_id,
    rolname AS user_name,
    rolpassword AS password,
    rolvaliduntil AS password_expiry,
    NULLIF(rolconnlimit, '-1'::integer) AS connection_limit
   FROM pg_roles
  WHERE (rolcanlogin = true);


--
-- Name: privilege_type; Type: TABLE; Schema: security_lookup; Owner: -
--

CREATE TABLE security_lookup.privilege_type (
    privilege_type name NOT NULL,
    privilege_code character(1)
);


--
-- Name: system_privilege_type; Type: TABLE; Schema: security_lookup; Owner: -
--

CREATE TABLE security_lookup.system_privilege_type (
    system_privilege_type text NOT NULL
);


--
-- Name: valid_object_privileges; Type: TABLE; Schema: security_lookup; Owner: -
--

CREATE TABLE security_lookup.valid_object_privileges (
    object_type name NOT NULL,
    privilege_type name NOT NULL
);


--
-- Name: copy; Type: TABLE; Schema: update; Owner: -
--

CREATE TABLE update.copy (
    source_id integer NOT NULL,
    source_schema name NOT NULL,
    source_relation name NOT NULL,
    target_schema name NOT NULL,
    target_table name NOT NULL,
    max_age rule_0.counting_number NOT NULL
);


--
-- Name: refresh_sequence; Type: TABLE; Schema: update; Owner: -
--

CREATE TABLE update.refresh_sequence (
    refresh_id integer NOT NULL,
    view_group text DEFAULT 'Default'::text NOT NULL,
    ordinality real NOT NULL,
    view_schema name NOT NULL,
    view_name name NOT NULL
);


--
-- Name: refresh_sequence_2_refresh_id_seq; Type: SEQUENCE; Schema: update; Owner: -
--

ALTER TABLE update.refresh_sequence ALTER COLUMN refresh_id ADD GENERATED BY DEFAULT AS IDENTITY (
    SEQUENCE NAME update.refresh_sequence_2_refresh_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: source_source_id_seq; Type: SEQUENCE; Schema: update; Owner: -
--

ALTER TABLE update.copy ALTER COLUMN source_id ADD GENERATED ALWAYS AS IDENTITY (
    SEQUENCE NAME update.source_source_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1
);


--
-- Name: view_dependencies; Type: VIEW; Schema: update; Owner: -
--

CREATE VIEW update.view_dependencies AS
 SELECT dependent_ns.nspname AS dependent_schema,
    dependent_view.relname AS dependent_view,
    source_ns.nspname AS source_schema,
    source_table.relname AS source_relation,
    pg_attribute.attname AS column_name
   FROM ((((((pg_depend
     JOIN pg_rewrite ON ((pg_depend.objid = pg_rewrite.oid)))
     JOIN pg_class dependent_view ON ((pg_rewrite.ev_class = dependent_view.oid)))
     JOIN pg_class source_table ON ((pg_depend.refobjid = source_table.oid)))
     JOIN pg_attribute ON (((pg_depend.refobjid = pg_attribute.attrelid) AND (pg_depend.refobjsubid = pg_attribute.attnum))))
     JOIN pg_namespace dependent_ns ON ((dependent_ns.oid = dependent_view.relnamespace)))
     JOIN pg_namespace source_ns ON ((source_ns.oid = source_table.relnamespace)))
  WHERE ((pg_attribute.attnum > 0) AND (dependent_ns.nspname <> 'information_schema'::name) AND (dependent_ns.nspname <> 'pg_catalog'::name))
  ORDER BY dependent_ns.nspname, dependent_view.relname;


--
-- Name: view_dependency_tree; Type: VIEW; Schema: update; Owner: -
--

CREATE VIEW update.view_dependency_tree AS
 WITH deps AS (
         SELECT DISTINCT (((view_dependencies.dependent_schema)::text || '.'::text) || (view_dependencies.dependent_view)::text) AS dependent,
            (((view_dependencies.source_schema)::text || '.'::text) || (view_dependencies.source_relation)::text) AS source
           FROM update.view_dependencies
        ), deps_2 AS (
         SELECT DISTINCT a_1.dependent,
            a_1.source,
            b_1.source AS source_2
           FROM (deps a_1
             LEFT JOIN deps b_1 ON ((a_1.source = b_1.dependent)))
        ), deps_3 AS (
         SELECT deps_2.dependent,
            deps_2.source,
            deps_2.source_2,
            deps.source AS source_3
           FROM (deps_2
             LEFT JOIN deps ON ((deps_2.source_2 = deps.dependent)))
        ), deps_4 AS (
         SELECT deps_3.dependent,
            deps_3.source,
            deps_3.source_2,
            deps_3.source_3,
            deps.source AS source_4
           FROM (deps_3
             LEFT JOIN deps ON ((deps_3.source_3 = deps.dependent)))
        ), deps_5 AS (
         SELECT deps_4.dependent,
            deps_4.source,
            deps_4.source_2,
            deps_4.source_3,
            deps_4.source_4,
            deps.source AS source_5
           FROM (deps_4
             LEFT JOIN deps ON ((deps_4.source_4 = deps.dependent)))
        ), deps_6 AS (
         SELECT deps_5.dependent,
            deps_5.source,
            deps_5.source_2,
            deps_5.source_3,
            deps_5.source_4,
            deps_5.source_5,
            deps.source AS source_6
           FROM (deps_5
             LEFT JOIN deps ON ((deps_5.source_5 = deps.dependent)))
        )
 SELECT a.dependent,
    a.source,
    a.source_2,
    a.source_3,
    a.source_4,
    a.source_5,
    a.source_6
   FROM (deps_6 a
     LEFT JOIN deps_6 b ON (((a.dependent = b.source) OR (a.dependent = b.source_2) OR (a.dependent = b.source_3) OR (a.dependent = b.source_4) OR (a.dependent = b.source_5) OR (a.dependent = b.source_6) OR (a.source_2 = b.source_3) OR (a.source_2 = b.source_4) OR (a.source_2 = b.source_5) OR (a.source_2 = b.source_6) OR (a.source_3 = b.source_4) OR (a.source_3 = b.source_5) OR (a.source_3 = b.source_6) OR (a.source_4 = b.source_5) OR (a.source_4 = b.source_6) OR (a.source_5 = b.source_6))))
  WHERE (b.source IS NULL);


--
-- Data for Name: view_key_fields; Type: TABLE DATA; Schema: meta; Owner: -
--

COPY meta.view_key_fields (view_name, key_name, field_name) FROM stdin;
\.


--
-- Data for Name: view_keys; Type: TABLE DATA; Schema: meta; Owner: -
--

COPY meta.view_keys (view_name, key_name) FROM stdin;
\.


--
-- Data for Name: aggfinalmodify; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.aggfinalmodify (aggfinalmodify, aggregate_transition_state_modification_policy) FROM stdin;
r	Read-only
s	Not after the aggfinalfn
w	Writes on the value
\.


--
-- Data for Name: aggkind; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.aggkind (aggkind, aggregate_kind) FROM stdin;
n	Normal
o	Ordered set
h	Hypothetical set
\.


--
-- Data for Name: aggmfinalmodify; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.aggmfinalmodify (aggmfinalmodify, aggregate_transition_state_modification_policy) FROM stdin;
r	Read-only
s	Not after the aggmfinalfn
w	Writes on the value
\.


--
-- Data for Name: amoppurpose; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.amoppurpose (amoppurpose, access_method_operator_purpose) FROM stdin;
s	Search
o	Ordering
\.


--
-- Data for Name: amtype; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.amtype (amtype, access_method_type) FROM stdin;
t	Table or Materialized View
i	Index
\.


--
-- Data for Name: attalign; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.attalign (attalign, attribute_alignment) FROM stdin;
c	Char alignment (no alignment needed)
s	Short alignment (2 bytes on most machines)
i	Int alignment (4 bytes on most machines)
d	Double alignment (8 bytes on most machines)
\.


--
-- Data for Name: attcompression; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.attcompression (attcompression, attribute_compression) FROM stdin;
p	pglz
l	LZ4
	Current default setting
\.


--
-- Data for Name: attgenerated; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.attgenerated (attgenerated, generated) FROM stdin;
	No
s	Stored
\.


--
-- Data for Name: attidentity; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.attidentity (attidentity, identity) FROM stdin;
	No
a	Always
d	by Default
\.


--
-- Data for Name: attstorage; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.attstorage (attstorage, attribute_storage, attribute_storage_notes) FROM stdin;
p	Plain	Values must always be stored plain.
e	External	Values can be stored in a secondary "TOAST" relation.
m	Main	Values can be compressed and stored inline.
x	Extended	Values can be compressed and/or moved to a secondary relation.
\.


--
-- Data for Name: castcontext; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.castcontext (castcontext, cast_context) FROM stdin;
a	Implicitly and Explicitly
e	Explicit
i	Implicit
\.


--
-- Data for Name: castmethod; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.castmethod (castmethod, cast_method) FROM stdin;
b	Binary-Coercible
f	Specified Function
i	Input/Output Functions
\.


--
-- Data for Name: collprovider; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.collprovider (collprovider, collation_provider) FROM stdin;
d	Database default
c	libc
i	icu
\.


--
-- Data for Name: comment_targets; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.comment_targets (comment_target_type) FROM stdin;
ACCESS METHOD
AGGREGATE
CAST
COLLATION
COLUMN
CONSTRAINT
CONVERSION
DATABASE
DOMAIN
EXTENSION
EVENT TRIGGER
FOREIGN DATA WRAPPER
FOREIGN TABLE
FUNCTION
INDEX
LARGE OBJECT
MATERIALIZED VIEW
OPERATOR
OPERATOR CLASS
OPERATOR FAMILY
POLICY
PROCEDURAL LANGUAGE
PROCEDURE
PUBLICATION
ROLE
ROUTINE
RULE
SCHEMA
SEQUENCE
SERVER
STATISTICS
SUBSCRIPTION
TABLE
TABLESPACE
TEXT SEARCH CONFIGURATION
TEXT SEARCH DICTIONARY
TEXT SEARCH PARSER
TEXT SEARCH TEMPLATE
TRANSFORM FOR
TRIGGER
TYPE
VIEW
\.


--
-- Data for Name: confdeltype; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.confdeltype (confdeltype, foreign_key_deletion_type) FROM stdin;
a	No action
r	Restrict
c	Cascade
n	Set NULL
d	Set default
\.


--
-- Data for Name: confmatchtype; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.confmatchtype (confmatchtype, foreign_key_match_type) FROM stdin;
f	Full
p	Partial
s	Simple
\.


--
-- Data for Name: confupdtype; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.confupdtype (confupdtype, foreign_key_update_type) FROM stdin;
a	No action
r	Restrict
c	Cascade
n	Set NULL
d	Set default
\.


--
-- Data for Name: contype; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.contype (contype, constraint_type) FROM stdin;
c	Check
f	Foreign Key
p	Primary Key
u	Unique
t	Trigger
x	Exclusion
\.


--
-- Data for Name: datlocprovider; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.datlocprovider (datlocprovider, locale_provider) FROM stdin;
c	libc
i	icu
\.


--
-- Data for Name: defaclobjtype; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.defaclobjtype (defaclobjtype, object_type) FROM stdin;
r	Relation (table,view)
S	Sequence
f	Function
T	Type
n	Schema
\.


--
-- Data for Name: deptype; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.deptype (deptype, dependency_type) FROM stdin;
n	DEPENDENCY_NORMAL
a	DEPENDENCY_AUTO
i	DEPENDENCY_INTERNAL
P	DEPENDENCY_PARTITION_PRI
S	DEPENDENCY_PARTITION_SEC
e	DEPENDENCY_EXTENSION
x	DEPENDENCY_AUTO_EXTENSION
\.


--
-- Data for Name: error_class; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.error_class (error_class, error_class_name) FROM stdin;
00	Successful Completion
01	Warning
02	No Data (this is also a warning class per the SQL standard)
03	SQL Statement Not Yet Complete
08	Connection Exception
09	Triggered Action Exception
0A	Feature Not Supported
0B	Invalid Transaction Initiation
0F	Locator Exception
0L	Invalid Grantor
0P	Invalid Role Specification
0Z	Diagnostics Exception
20	Case Not Found
21	Cardinality Violation
22	Data Exception
23	Integrity Constraint Violation
24	Invalid Cursor State
25	Invalid Transaction State
26	Invalid SQL Statement Name
27	Triggered Data Change Violation
28	Invalid Authorization Specification
2B	Dependent Privilege Descriptors Still Exist
2D	Invalid Transaction Termination
2F	SQL Routine Exception
34	Invalid Cursor Name
38	External Routine Exception
39	External Routine Invocation Exception
3B	Savepoint Exception
3D	Invalid Catalog Name
3F	Invalid Schema Name
40	Transaction Rollback
42	Syntax Error or Access Rule Violation
44	WITH CHECK OPTION Violation
53	Insufficient Resources
54	Program Limit Exceeded
55	Object Not In Prerequisite State
57	Operator Intervention
58	System Error (errors external to PostgreSQL itself)
72	Snapshot Failure
F0	Configuration File Error
HV	Foreign Data Wrapper Error (SQL/MED)
P0	PL/pgSQL Error
XX	Internal Error
\.


--
-- Data for Name: error_code; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.error_code (error_class, error_code, condition_name) FROM stdin;
00	00000	successful_completion
01	01000	warning
01	0100C	dynamic_result_sets_returned
01	01008	implicit_zero_bit_padding
01	01003	null_value_eliminated_in_set_function
01	01007	privilege_not_granted
01	01006	privilege_not_revoked
01	01004	string_data_right_truncation
01	01P01	deprecated_feature
02	02000	no_data
02	02001	no_additional_dynamic_result_sets_returned
03	03000	sql_statement_not_yet_complete
08	08000	connection_exception
08	08003	connection_does_not_exist
08	08006	connection_failure
08	08001	sqlclient_unable_to_establish_sqlconnection
08	08004	sqlserver_rejected_establishment_of_sqlconnection
08	08007	transaction_resolution_unknown
08	08P01	protocol_violation
09	09000	triggered_action_exception
0A	0A000	feature_not_supported
0B	0B000	invalid_transaction_initiation
0F	0F000	locator_exception
0F	0F001	invalid_locator_specification
0L	0L000	invalid_grantor
0L	0LP01	invalid_grant_operation
0P	0P000	invalid_role_specification
0Z	0Z000	diagnostics_exception
0Z	0Z002	stacked_diagnostics_accessed_without_active_handler
20	20000	case_not_found
21	21000	cardinality_violation
22	22000	data_exception
22	2202E	array_subscript_error
22	22021	character_not_in_repertoire
22	22008	datetime_field_overflow
22	22012	division_by_zero
22	22005	error_in_assignment
22	2200B	escape_character_conflict
22	22022	indicator_overflow
22	22015	interval_field_overflow
22	2201E	invalid_argument_for_logarithm
22	22014	invalid_argument_for_ntile_function
22	22016	invalid_argument_for_nth_value_function
22	2201F	invalid_argument_for_power_function
22	2201G	invalid_argument_for_width_bucket_function
22	22018	invalid_character_value_for_cast
22	22007	invalid_datetime_format
22	22019	invalid_escape_character
22	2200D	invalid_escape_octet
22	22025	invalid_escape_sequence
22	22P06	nonstandard_use_of_escape_character
22	22010	invalid_indicator_parameter_value
22	22023	invalid_parameter_value
22	22013	invalid_preceding_or_following_size
22	2201B	invalid_regular_expression
22	2201W	invalid_row_count_in_limit_clause
22	2201X	invalid_row_count_in_result_offset_clause
22	2202H	invalid_tablesample_argument
22	2202G	invalid_tablesample_repeat
22	22009	invalid_time_zone_displacement_value
22	2200C	invalid_use_of_escape_character
22	2200G	most_specific_type_mismatch
22	22004	null_value_not_allowed
22	22002	null_value_no_indicator_parameter
22	22003	numeric_value_out_of_range
22	2200H	sequence_generator_limit_exceeded
22	22026	string_data_length_mismatch
22	22001	string_data_right_truncation
22	22011	substring_error
22	22027	trim_error
22	22024	unterminated_c_string
22	2200F	zero_length_character_string
22	22P01	floating_point_exception
22	22P02	invalid_text_representation
22	22P03	invalid_binary_representation
22	22P04	bad_copy_file_format
22	22P05	untranslatable_character
22	2200L	not_an_xml_document
22	2200M	invalid_xml_document
22	2200N	invalid_xml_content
22	2200S	invalid_xml_comment
22	2200T	invalid_xml_processing_instruction
22	22030	duplicate_json_object_key_value
22	22031	invalid_argument_for_sql_json_datetime_function
22	22032	invalid_json_text
22	22033	invalid_sql_json_subscript
22	22034	more_than_one_sql_json_item
22	22035	no_sql_json_item
22	22036	non_numeric_sql_json_item
22	22037	non_unique_keys_in_a_json_object
22	22038	singleton_sql_json_item_required
22	22039	sql_json_array_not_found
22	2203A	sql_json_member_not_found
22	2203B	sql_json_number_not_found
22	2203C	sql_json_object_not_found
22	2203D	too_many_json_array_elements
22	2203E	too_many_json_object_members
22	2203F	sql_json_scalar_required
22	2203G	sql_json_item_cannot_be_cast_to_target_type
23	23000	integrity_constraint_violation
23	23001	restrict_violation
23	23502	not_null_violation
23	23503	foreign_key_violation
23	23505	unique_violation
23	23514	check_violation
23	23P01	exclusion_violation
24	24000	invalid_cursor_state
25	25000	invalid_transaction_state
P0	P0000	plpgsql_error
25	25001	active_sql_transaction
25	25002	branch_transaction_already_active
25	25008	held_cursor_requires_same_isolation_level
25	25003	inappropriate_access_mode_for_branch_transaction
25	25004	inappropriate_isolation_level_for_branch_transaction
25	25005	no_active_sql_transaction_for_branch_transaction
25	25006	read_only_sql_transaction
25	25007	schema_and_data_statement_mixing_not_supported
25	25P01	no_active_sql_transaction
25	25P02	in_failed_sql_transaction
25	25P03	idle_in_transaction_session_timeout
26	26000	invalid_sql_statement_name
27	27000	triggered_data_change_violation
28	28000	invalid_authorization_specification
28	28P01	invalid_password
2B	2B000	dependent_privilege_descriptors_still_exist
2B	2BP01	dependent_objects_still_exist
2D	2D000	invalid_transaction_termination
2F	2F000	sql_routine_exception
2F	2F005	function_executed_no_return_statement
2F	2F002	modifying_sql_data_not_permitted
2F	2F003	prohibited_sql_statement_attempted
2F	2F004	reading_sql_data_not_permitted
34	34000	invalid_cursor_name
38	38000	external_routine_exception
38	38001	containing_sql_not_permitted
38	38002	modifying_sql_data_not_permitted
38	38003	prohibited_sql_statement_attempted
38	38004	reading_sql_data_not_permitted
39	39000	external_routine_invocation_exception
39	39001	invalid_sqlstate_returned
39	39004	null_value_not_allowed
39	39P01	trigger_protocol_violated
39	39P02	srf_protocol_violated
39	39P03	event_trigger_protocol_violated
3B	3B000	savepoint_exception
3B	3B001	invalid_savepoint_specification
3D	3D000	invalid_catalog_name
3F	3F000	invalid_schema_name
40	40000	transaction_rollback
40	40002	transaction_integrity_constraint_violation
40	40001	serialization_failure
40	40003	statement_completion_unknown
40	40P01	deadlock_detected
42	42000	syntax_error_or_access_rule_violation
42	42601	syntax_error
42	42501	insufficient_privilege
42	42846	cannot_coerce
42	42803	grouping_error
42	42P20	windowing_error
42	42P19	invalid_recursion
42	42830	invalid_foreign_key
42	42602	invalid_name
42	42622	name_too_long
42	42939	reserved_name
42	42804	datatype_mismatch
42	42P18	indeterminate_datatype
42	42P21	collation_mismatch
42	42P22	indeterminate_collation
42	42809	wrong_object_type
42	428C9	generated_always
42	42703	undefined_column
42	42883	undefined_function
42	42P01	undefined_table
42	42P02	undefined_parameter
42	42704	undefined_object
42	42701	duplicate_column
42	42P03	duplicate_cursor
42	42P04	duplicate_database
42	42723	duplicate_function
42	42P05	duplicate_prepared_statement
42	42P06	duplicate_schema
42	42P07	duplicate_table
42	42712	duplicate_alias
42	42710	duplicate_object
42	42702	ambiguous_column
42	42725	ambiguous_function
42	42P08	ambiguous_parameter
42	42P09	ambiguous_alias
42	42P10	invalid_column_reference
42	42611	invalid_column_definition
42	42P11	invalid_cursor_definition
42	42P12	invalid_database_definition
42	42P13	invalid_function_definition
42	42P14	invalid_prepared_statement_definition
42	42P15	invalid_schema_definition
42	42P16	invalid_table_definition
42	42P17	invalid_object_definition
44	44000	with_check_option_violation
53	53000	insufficient_resources
53	53100	disk_full
53	53200	out_of_memory
53	53300	too_many_connections
53	53400	configuration_limit_exceeded
54	54000	program_limit_exceeded
54	54001	statement_too_complex
54	54011	too_many_columns
54	54023	too_many_arguments
55	55000	object_not_in_prerequisite_state
55	55006	object_in_use
55	55P02	cant_change_runtime_param
55	55P03	lock_not_available
55	55P04	unsafe_new_enum_value_usage
57	57000	operator_intervention
57	57014	query_canceled
57	57P01	admin_shutdown
57	57P02	crash_shutdown
57	57P03	cannot_connect_now
57	57P04	database_dropped
57	57P05	idle_session_timeout
58	58000	system_error
58	58030	io_error
58	58P01	undefined_file
58	58P02	duplicate_file
72	72000	snapshot_too_old
F0	F0000	config_file_error
F0	F0001	lock_file_exists
HV	HV000	fdw_error
HV	HV005	fdw_column_name_not_found
HV	HV002	fdw_dynamic_parameter_value_needed
HV	HV010	fdw_function_sequence_error
HV	HV021	fdw_inconsistent_descriptor_information
HV	HV024	fdw_invalid_attribute_value
HV	HV007	fdw_invalid_column_name
HV	HV008	fdw_invalid_column_number
HV	HV004	fdw_invalid_data_type
HV	HV006	fdw_invalid_data_type_descriptors
HV	HV091	fdw_invalid_descriptor_field_identifier
HV	HV00B	fdw_invalid_handle
HV	HV00C	fdw_invalid_option_index
HV	HV00D	fdw_invalid_option_name
HV	HV090	fdw_invalid_string_length_or_buffer_length
HV	HV00A	fdw_invalid_string_format
HV	HV009	fdw_invalid_use_of_null_pointer
HV	HV014	fdw_too_many_handles
HV	HV001	fdw_out_of_memory
HV	HV00P	fdw_no_schemas
HV	HV00J	fdw_option_name_not_found
HV	HV00K	fdw_reply_handle
HV	HV00Q	fdw_schema_not_found
HV	HV00R	fdw_table_not_found
HV	HV00L	fdw_unable_to_create_execution
HV	HV00M	fdw_unable_to_create_reply
HV	HV00N	fdw_unable_to_establish_connection
P0	P0001	raise_exception
P0	P0002	no_data_found
P0	P0003	too_many_rows
P0	P0004	assert_failure
XX	XX000	internal_error
XX	XX001	data_corrupted
XX	XX002	index_corrupted
\.


--
-- Data for Name: ev_enabled; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.ev_enabled (ev_enabled, enabling_event_mode) FROM stdin;
O	Origin and local modes
D	Disabled
R	Replica mode
A	Always
\.


--
-- Data for Name: ev_type; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.ev_type (ev_type, event_type) FROM stdin;
1	SELECT
2	UPDATE
3	INSERT
4	DELETE
\.


--
-- Data for Name: evtenabled; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.evtenabled (evtenabled, enabling_event_mode) FROM stdin;
O	Origin and local modes
D	Disabled
R	Replica mode
A	Always
\.


--
-- Data for Name: lock_conflicts; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.lock_conflicts (lock_level, current_lock_mode, conflicting_request) FROM stdin;
Table	ACCESS EXCLUSIVE	ACCESS SHARE
Table	EXCLUSIVE	ROW SHARE
Table	ACCESS EXCLUSIVE	ROW SHARE
Table	SHARE	ROW EXCLUSIVE
Table	SHARE ROW EXCLUSIVE	ROW EXCLUSIVE
Table	EXCLUSIVE	ROW EXCLUSIVE
Table	ACCESS EXCLUSIVE	ROW EXCLUSIVE
Table	SHARE UPDATE EXCLUSIVE	SHARE UPDATE EXCLUSIVE
Table	SHARE	SHARE UPDATE EXCLUSIVE
Table	SHARE ROW EXCLUSIVE	SHARE UPDATE EXCLUSIVE
Table	EXCLUSIVE	SHARE UPDATE EXCLUSIVE
Table	ACCESS EXCLUSIVE	SHARE UPDATE EXCLUSIVE
Table	ROW EXCLUSIVE	SHARE
Table	SHARE UPDATE EXCLUSIVE	SHARE
Table	SHARE ROW EXCLUSIVE	SHARE
Table	EXCLUSIVE	SHARE
Table	ACCESS EXCLUSIVE	SHARE
Table	ROW EXCLUSIVE	SHARE ROW EXCLUSIVE
Table	SHARE UPDATE EXCLUSIVE	SHARE ROW EXCLUSIVE
Table	SHARE	SHARE ROW EXCLUSIVE
Table	SHARE ROW EXCLUSIVE	SHARE ROW EXCLUSIVE
Table	EXCLUSIVE	SHARE ROW EXCLUSIVE
Table	ACCESS EXCLUSIVE	SHARE ROW EXCLUSIVE
Table	ROW SHARE	EXCLUSIVE
Table	ROW EXCLUSIVE	EXCLUSIVE
Table	SHARE UPDATE EXCLUSIVE	EXCLUSIVE
Table	SHARE	EXCLUSIVE
Table	SHARE ROW EXCLUSIVE	EXCLUSIVE
Table	EXCLUSIVE	EXCLUSIVE
Table	ACCESS EXCLUSIVE	EXCLUSIVE
Table	ACCESS SHARE	ACCESS EXCLUSIVE
Table	ROW SHARE	ACCESS EXCLUSIVE
Table	ROW EXCLUSIVE	ACCESS EXCLUSIVE
Table	SHARE UPDATE EXCLUSIVE	ACCESS EXCLUSIVE
Table	SHARE	ACCESS EXCLUSIVE
Table	SHARE ROW EXCLUSIVE	ACCESS EXCLUSIVE
Table	EXCLUSIVE	ACCESS EXCLUSIVE
Table	ACCESS EXCLUSIVE	ACCESS EXCLUSIVE
Row	FOR UPDATE	FOR KEY SHARE
Row	FOR NO KEY UPDATE	FOR SHARE
Row	FOR UPDATE	FOR SHARE
Row	FOR SHARE	FOR NO KEY UPDATE
Row	FOR NO KEY UPDATE	FOR NO KEY UPDATE
Row	FOR UPDATE	FOR NO KEY UPDATE
Row	FOR KEY SHARE	FOR UPDATE
Row	FOR SHARE	FOR UPDATE
Row	FOR NO KEY UPDATE	FOR UPDATE
Row	FOR UPDATE	FOR UPDATE
\.


--
-- Data for Name: lock_level; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.lock_level (lock_level) FROM stdin;
Table
Row
Page
\.


--
-- Data for Name: lock_mode; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.lock_mode (lock_level, lock_mode, alternate_mode_name) FROM stdin;
Table	ACCESS SHARE	AccessShareLock
Table	ROW SHARE	RowShareLock
Table	ROW EXCLUSIVE	RowExclusiveLock
Table	SHARE UPDATE EXCLUSIVE	ShareUpdateExclusiveLock
Table	SHARE	ShareLock
Table	SHARE ROW EXCLUSIVE	ShareRowExclusiveLock
Table	EXCLUSIVE	ExclusiveLock
Table	ACCESS EXCLUSIVE	AccessExclusiveLock
Row	FOR UPDATE	\N
Row	FOR NO KEY UPDATE	\N
Row	FOR SHARE	\N
Row	FOR KEY SHARE	\N
\.


--
-- Data for Name: message_level; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.message_level (message_level) FROM stdin;
DEBUG
LOG
INFO
NOTICE
WARNING
EXCEPTION
\.


--
-- Data for Name: oprkind; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.oprkind (oprkind, operator_kind) FROM stdin;
b	Infix
l	Left
\.


--
-- Data for Name: partstrat; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.partstrat (partstrat, partitioning_strategy) FROM stdin;
h	Hash
l	List
r	Range
\.


--
-- Data for Name: polcmd; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.polcmd (polcmd, policy_command_type) FROM stdin;
r	SELECT
a	INSERT
w	UPDATE
d	DELETE
*	All
\.


--
-- Data for Name: privtype; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.privtype (privtype, initial_privilege_creator) FROM stdin;
i	initdb
e	CREATE EXTENSION
\.


--
-- Data for Name: proargmode; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.proargmode (proargmode, procedure_argument_mode) FROM stdin;
i	IN
o	OUT
b	INOUT
v	VARIADIC
t	TABLE
\.


--
-- Data for Name: prokind; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.prokind (prokind, procedure_kind) FROM stdin;
f	Function
p	Procedure
a	Aggregate Function
w	Window Function
\.


--
-- Data for Name: proparallel; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.proparallel (proparallel, parallel_safety) FROM stdin;
s	Safe
r	Restricted
u	Unsafe
\.


--
-- Data for Name: provolatile; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.provolatile (provolatile, volatility) FROM stdin;
i	Immutable
s	Stable
v	Volatile
\.


--
-- Data for Name: relkind; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.relkind (relkind, relation_type) FROM stdin;
i	Index
S	Sequence
t	Toast Table
v	View
m	Materialized View
c	Composite Type
f	Foreign Table
p	Partitioned Table
I	Partitioned Index
r	Table
\.


--
-- Data for Name: relpersistence; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.relpersistence (relpersistence, relation_persistence) FROM stdin;
p	Permanent
u	Unlogged
t	Temporary
\.


--
-- Data for Name: relreplident; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.relreplident (relreplident, relation_persistence) FROM stdin;
d	Default (primary key, if any)
n	Nothing
f	All columns
i	Index with "indisreplident" set
\.


--
-- Data for Name: srsubstate; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.srsubstate (srsubstate, subscribed_relation_state) FROM stdin;
i	Initialize
d	Data is being copied
f	Finished table copy
s	Synchronized
r	Ready (normal replication)
\.


--
-- Data for Name: substream; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.substream (substream, substream_policy) FROM stdin;
f	Disallow streaming of in-progress transactions.
t	Spill the changes of in-progress transactions to disk and apply at once after the transaction is committed on the publisher and received by the subscriber.
p	Apply changes directly using a parallel apply worker if available.
\.


--
-- Data for Name: subtwophasestate; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.subtwophasestate (subtwophasestate, two_phase_mode_state) FROM stdin;
d	Disabled
p	Pending enablement
a	Enabled
\.


--
-- Data for Name: tgenabled; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.tgenabled (tgenabled, trigger_enabled_condition) FROM stdin;
D	Disabled
A	Always
R	Replica Mode Only
O	Origin or Local Mode Only
\.


--
-- Data for Name: typalign; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.typalign (typalign, type_alignment) FROM stdin;
c	Char alignment (no alignment needed)
s	Short alignment (2 bytes on most machines)
i	Int alignment (4 bytes on most machines)
d	Double alignment (8 bytes on most machines)
\.


--
-- Data for Name: typcategory; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.typcategory (typcategory, type_category) FROM stdin;
A	Array
B	Boolean
C	Composite
D	Date/Time
E	Enum
G	Geometric
I	Network
N	Numeric
P	Pseudo-type
R	Range
S	String
T	Timespan
U	User-defined
V	Bit-string
X	Unknown
Z	Internal-use
\.


--
-- Data for Name: typstorage; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.typstorage (typstorage, type_storage, type_storage_notes) FROM stdin;
p	Plain	Values must always be stored plain.
e	External	Values can be stored in a secondary "TOAST" relation.
m	Main	Values can be compressed and stored inline.
x	Extended	Values can be compressed and/or moved to a secondary relation.
\.


--
-- Data for Name: typtype; Type: TABLE DATA; Schema: meta_lookup; Owner: -
--

COPY meta_lookup.typtype (typtype, type_type) FROM stdin;
b	Base
c	Composite
d	Domain
e	Enum
p	Pseudo
r	Range
m	Multirange
\.


--
-- Data for Name: privilege_type; Type: TABLE DATA; Schema: security_lookup; Owner: -
--

COPY security_lookup.privilege_type (privilege_type, privilege_code) FROM stdin;
\.


--
-- Data for Name: system_privilege_type; Type: TABLE DATA; Schema: security_lookup; Owner: -
--

COPY security_lookup.system_privilege_type (system_privilege_type) FROM stdin;
\.


--
-- Data for Name: valid_object_privileges; Type: TABLE DATA; Schema: security_lookup; Owner: -
--

COPY security_lookup.valid_object_privileges (object_type, privilege_type) FROM stdin;
\.


--
-- Data for Name: copy; Type: TABLE DATA; Schema: update; Owner: -
--

COPY update.copy (source_id, source_schema, source_relation, target_schema, target_table, max_age) FROM stdin;
\.


--
-- Data for Name: refresh_sequence; Type: TABLE DATA; Schema: update; Owner: -
--

COPY update.refresh_sequence (refresh_id, view_group, ordinality, view_schema, view_name) FROM stdin;
\.


--
-- Name: refresh_sequence_2_refresh_id_seq; Type: SEQUENCE SET; Schema: update; Owner: -
--

SELECT pg_catalog.setval('update.refresh_sequence_2_refresh_id_seq', 1, false);


--
-- Name: source_source_id_seq; Type: SEQUENCE SET; Schema: update; Owner: -
--

SELECT pg_catalog.setval('update.source_source_id_seq', 1, false);


--
-- Name: view_key_fields view_key_fields_pkey; Type: CONSTRAINT; Schema: meta; Owner: -
--

ALTER TABLE ONLY meta.view_key_fields
    ADD CONSTRAINT view_key_fields_pkey PRIMARY KEY (view_name, key_name, field_name);


--
-- Name: view_keys view_keys_pkey; Type: CONSTRAINT; Schema: meta; Owner: -
--

ALTER TABLE ONLY meta.view_keys
    ADD CONSTRAINT view_keys_pkey PRIMARY KEY (view_name, key_name);


--
-- Name: amoppurpose access_method_operator_purpose_uniq; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.amoppurpose
    ADD CONSTRAINT access_method_operator_purpose_uniq UNIQUE (access_method_operator_purpose);


--
-- Name: aggfinalmodify aggfinalmodify_lookup_field_uniq; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.aggfinalmodify
    ADD CONSTRAINT aggfinalmodify_lookup_field_uniq UNIQUE (aggregate_transition_state_modification_policy);


--
-- Name: aggfinalmodify aggfinalmodify_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.aggfinalmodify
    ADD CONSTRAINT aggfinalmodify_pkey PRIMARY KEY (aggfinalmodify);


--
-- Name: aggkind aggkind_aggregate_kind_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.aggkind
    ADD CONSTRAINT aggkind_aggregate_kind_key UNIQUE (aggregate_kind);


--
-- Name: aggkind aggkind_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.aggkind
    ADD CONSTRAINT aggkind_pkey PRIMARY KEY (aggkind);


--
-- Name: aggmfinalmodify aggmfinalmodify_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.aggmfinalmodify
    ADD CONSTRAINT aggmfinalmodify_pkey PRIMARY KEY (aggmfinalmodify);


--
-- Name: aggmfinalmodify aggmfinalmodify_rows_uniq; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.aggmfinalmodify
    ADD CONSTRAINT aggmfinalmodify_rows_uniq UNIQUE (aggregate_transition_state_modification_policy);


--
-- Name: amoppurpose amoppurpose_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.amoppurpose
    ADD CONSTRAINT amoppurpose_pkey PRIMARY KEY (amoppurpose);


--
-- Name: amtype amtype_access_method_type_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.amtype
    ADD CONSTRAINT amtype_access_method_type_key UNIQUE (access_method_type);


--
-- Name: amtype amtype_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.amtype
    ADD CONSTRAINT amtype_pkey PRIMARY KEY (amtype);


--
-- Name: attalign attalign_attribute_alignment_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.attalign
    ADD CONSTRAINT attalign_attribute_alignment_key UNIQUE (attribute_alignment);


--
-- Name: attalign attalign_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.attalign
    ADD CONSTRAINT attalign_pkey PRIMARY KEY (attalign);


--
-- Name: attcompression attcompression_attribute_compression_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.attcompression
    ADD CONSTRAINT attcompression_attribute_compression_key UNIQUE (attribute_compression);


--
-- Name: attcompression attcompression_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.attcompression
    ADD CONSTRAINT attcompression_pkey PRIMARY KEY (attcompression);


--
-- Name: attgenerated attgenerated_generated_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.attgenerated
    ADD CONSTRAINT attgenerated_generated_key UNIQUE (generated);


--
-- Name: attgenerated attgenerated_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.attgenerated
    ADD CONSTRAINT attgenerated_pkey PRIMARY KEY (attgenerated);


--
-- Name: attidentity attidentity_identity_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.attidentity
    ADD CONSTRAINT attidentity_identity_key UNIQUE (identity);


--
-- Name: attidentity attidentity_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.attidentity
    ADD CONSTRAINT attidentity_pkey PRIMARY KEY (attidentity);


--
-- Name: attstorage attstorage_attribute_storage_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.attstorage
    ADD CONSTRAINT attstorage_attribute_storage_key UNIQUE (attribute_storage);


--
-- Name: attstorage attstorage_attribute_storage_notes_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.attstorage
    ADD CONSTRAINT attstorage_attribute_storage_notes_key UNIQUE (attribute_storage_notes);


--
-- Name: attstorage attstorage_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.attstorage
    ADD CONSTRAINT attstorage_pkey PRIMARY KEY (attstorage);


--
-- Name: castcontext castcontext_cast_context_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.castcontext
    ADD CONSTRAINT castcontext_cast_context_key UNIQUE (cast_context);


--
-- Name: castcontext castcontext_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.castcontext
    ADD CONSTRAINT castcontext_pkey PRIMARY KEY (castcontext);


--
-- Name: castmethod castmethod_cast_method_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.castmethod
    ADD CONSTRAINT castmethod_cast_method_key UNIQUE (cast_method);


--
-- Name: castmethod castmethod_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.castmethod
    ADD CONSTRAINT castmethod_pkey PRIMARY KEY (castmethod);


--
-- Name: collprovider collprovider_collation_provider_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.collprovider
    ADD CONSTRAINT collprovider_collation_provider_key UNIQUE (collation_provider);


--
-- Name: collprovider collprovider_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.collprovider
    ADD CONSTRAINT collprovider_pkey PRIMARY KEY (collprovider);


--
-- Name: comment_targets comment_targets_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.comment_targets
    ADD CONSTRAINT comment_targets_pkey PRIMARY KEY (comment_target_type);


--
-- Name: confdeltype confdeltype_foreign_key_deletion_type_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.confdeltype
    ADD CONSTRAINT confdeltype_foreign_key_deletion_type_key UNIQUE (foreign_key_deletion_type);


--
-- Name: confdeltype confdeltype_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.confdeltype
    ADD CONSTRAINT confdeltype_pkey PRIMARY KEY (confdeltype);


--
-- Name: confmatchtype confmatchtype_foreign_key_match_type_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.confmatchtype
    ADD CONSTRAINT confmatchtype_foreign_key_match_type_key UNIQUE (foreign_key_match_type);


--
-- Name: confmatchtype confmatchtype_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.confmatchtype
    ADD CONSTRAINT confmatchtype_pkey PRIMARY KEY (confmatchtype);


--
-- Name: confupdtype confupdtype_foreign_key_update_type_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.confupdtype
    ADD CONSTRAINT confupdtype_foreign_key_update_type_key UNIQUE (foreign_key_update_type);


--
-- Name: confupdtype confupdtype_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.confupdtype
    ADD CONSTRAINT confupdtype_pkey PRIMARY KEY (confupdtype);


--
-- Name: contype constraint_type_constraint_type_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.contype
    ADD CONSTRAINT constraint_type_constraint_type_key UNIQUE (constraint_type);


--
-- Name: contype constraint_type_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.contype
    ADD CONSTRAINT constraint_type_pkey PRIMARY KEY (contype);


--
-- Name: datlocprovider datlocprovider_locale_provider_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.datlocprovider
    ADD CONSTRAINT datlocprovider_locale_provider_key UNIQUE (locale_provider);


--
-- Name: datlocprovider datlocprovider_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.datlocprovider
    ADD CONSTRAINT datlocprovider_pkey PRIMARY KEY (datlocprovider);


--
-- Name: defaclobjtype defaclobjtype_object_type_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.defaclobjtype
    ADD CONSTRAINT defaclobjtype_object_type_key UNIQUE (object_type);


--
-- Name: defaclobjtype defaclobjtype_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.defaclobjtype
    ADD CONSTRAINT defaclobjtype_pkey PRIMARY KEY (defaclobjtype);


--
-- Name: deptype deptype_dependency_type_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.deptype
    ADD CONSTRAINT deptype_dependency_type_key UNIQUE (dependency_type);


--
-- Name: deptype deptype_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.deptype
    ADD CONSTRAINT deptype_pkey PRIMARY KEY (deptype);


--
-- Name: error_class error_class_error_class_name_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.error_class
    ADD CONSTRAINT error_class_error_class_name_key UNIQUE (error_class_name);


--
-- Name: error_class error_class_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.error_class
    ADD CONSTRAINT error_class_pkey PRIMARY KEY (error_class);


--
-- Name: error_code error_code_error_code_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.error_code
    ADD CONSTRAINT error_code_error_code_key UNIQUE (error_code);


--
-- Name: error_code error_code_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.error_code
    ADD CONSTRAINT error_code_pkey PRIMARY KEY (error_class, error_code);


--
-- Name: ev_enabled ev_enabled_enabling_event_mode_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.ev_enabled
    ADD CONSTRAINT ev_enabled_enabling_event_mode_key UNIQUE (enabling_event_mode);


--
-- Name: ev_enabled ev_enabled_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.ev_enabled
    ADD CONSTRAINT ev_enabled_pkey PRIMARY KEY (ev_enabled);


--
-- Name: ev_type ev_type_event_type_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.ev_type
    ADD CONSTRAINT ev_type_event_type_key UNIQUE (event_type);


--
-- Name: ev_type ev_type_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.ev_type
    ADD CONSTRAINT ev_type_pkey PRIMARY KEY (ev_type);


--
-- Name: evtenabled evtenabled_enabling_event_mode_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.evtenabled
    ADD CONSTRAINT evtenabled_enabling_event_mode_key UNIQUE (enabling_event_mode);


--
-- Name: evtenabled evtenabled_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.evtenabled
    ADD CONSTRAINT evtenabled_pkey PRIMARY KEY (evtenabled);


--
-- Name: lock_conflicts lock_conflicts_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.lock_conflicts
    ADD CONSTRAINT lock_conflicts_pkey PRIMARY KEY (lock_level, current_lock_mode, conflicting_request);


--
-- Name: lock_level lock_level_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.lock_level
    ADD CONSTRAINT lock_level_pkey PRIMARY KEY (lock_level);


--
-- Name: lock_mode lock_mode_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.lock_mode
    ADD CONSTRAINT lock_mode_pkey PRIMARY KEY (lock_level, lock_mode);


--
-- Name: message_level message_level_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.message_level
    ADD CONSTRAINT message_level_pkey PRIMARY KEY (message_level);


--
-- Name: oprkind oprkind_operator_kind_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.oprkind
    ADD CONSTRAINT oprkind_operator_kind_key UNIQUE (operator_kind);


--
-- Name: oprkind oprkind_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.oprkind
    ADD CONSTRAINT oprkind_pkey PRIMARY KEY (oprkind);


--
-- Name: partstrat partstrat_partitioning_strategy_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.partstrat
    ADD CONSTRAINT partstrat_partitioning_strategy_key UNIQUE (partitioning_strategy);


--
-- Name: partstrat partstrat_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.partstrat
    ADD CONSTRAINT partstrat_pkey PRIMARY KEY (partstrat);


--
-- Name: polcmd polcmd_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.polcmd
    ADD CONSTRAINT polcmd_pkey PRIMARY KEY (polcmd);


--
-- Name: polcmd polcmd_policy_command_type_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.polcmd
    ADD CONSTRAINT polcmd_policy_command_type_key UNIQUE (policy_command_type);


--
-- Name: privtype privtype_initial_privilege_creator_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.privtype
    ADD CONSTRAINT privtype_initial_privilege_creator_key UNIQUE (initial_privilege_creator);


--
-- Name: privtype privtype_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.privtype
    ADD CONSTRAINT privtype_pkey PRIMARY KEY (privtype);


--
-- Name: proargmode proargmode_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.proargmode
    ADD CONSTRAINT proargmode_pkey PRIMARY KEY (proargmode);


--
-- Name: proargmode proargmode_procedure_argument_mode_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.proargmode
    ADD CONSTRAINT proargmode_procedure_argument_mode_key UNIQUE (procedure_argument_mode);


--
-- Name: prokind prokind_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.prokind
    ADD CONSTRAINT prokind_pkey PRIMARY KEY (prokind);


--
-- Name: prokind prokind_procedure_kind_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.prokind
    ADD CONSTRAINT prokind_procedure_kind_key UNIQUE (procedure_kind);


--
-- Name: proparallel proparallel_parallel_safety_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.proparallel
    ADD CONSTRAINT proparallel_parallel_safety_key UNIQUE (parallel_safety);


--
-- Name: proparallel proparallel_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.proparallel
    ADD CONSTRAINT proparallel_pkey PRIMARY KEY (proparallel);


--
-- Name: provolatile provolatile_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.provolatile
    ADD CONSTRAINT provolatile_pkey PRIMARY KEY (provolatile);


--
-- Name: provolatile provolatile_volatility_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.provolatile
    ADD CONSTRAINT provolatile_volatility_key UNIQUE (volatility);


--
-- Name: relkind relkind_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.relkind
    ADD CONSTRAINT relkind_pkey PRIMARY KEY (relkind);


--
-- Name: relkind relkind_relkind_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.relkind
    ADD CONSTRAINT relkind_relkind_key UNIQUE (relation_type);


--
-- Name: relpersistence relpersistence_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.relpersistence
    ADD CONSTRAINT relpersistence_pkey PRIMARY KEY (relpersistence);


--
-- Name: relpersistence relpersistence_relation_persistence_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.relpersistence
    ADD CONSTRAINT relpersistence_relation_persistence_key UNIQUE (relation_persistence);


--
-- Name: relreplident relreplident_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.relreplident
    ADD CONSTRAINT relreplident_pkey PRIMARY KEY (relreplident);


--
-- Name: relreplident relreplident_relation_persistence_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.relreplident
    ADD CONSTRAINT relreplident_relation_persistence_key UNIQUE (relation_persistence);


--
-- Name: srsubstate srsubstate_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.srsubstate
    ADD CONSTRAINT srsubstate_pkey PRIMARY KEY (srsubstate);


--
-- Name: srsubstate srsubstate_subscribed_relation_state_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.srsubstate
    ADD CONSTRAINT srsubstate_subscribed_relation_state_key UNIQUE (subscribed_relation_state);


--
-- Name: substream substream_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.substream
    ADD CONSTRAINT substream_pkey PRIMARY KEY (substream);


--
-- Name: substream substream_substream_policy_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.substream
    ADD CONSTRAINT substream_substream_policy_key UNIQUE (substream_policy);


--
-- Name: subtwophasestate subtwophasestate_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.subtwophasestate
    ADD CONSTRAINT subtwophasestate_pkey PRIMARY KEY (subtwophasestate);


--
-- Name: subtwophasestate subtwophasestate_two_phase_mode_state_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.subtwophasestate
    ADD CONSTRAINT subtwophasestate_two_phase_mode_state_key UNIQUE (two_phase_mode_state);


--
-- Name: tgenabled trigger_enabled_condition_uniq; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.tgenabled
    ADD CONSTRAINT trigger_enabled_condition_uniq UNIQUE (trigger_enabled_condition);


--
-- Name: tgenabled trigger_enabled_conditions_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.tgenabled
    ADD CONSTRAINT trigger_enabled_conditions_pkey PRIMARY KEY (tgenabled);


--
-- Name: typalign typalign_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.typalign
    ADD CONSTRAINT typalign_pkey PRIMARY KEY (typalign);


--
-- Name: typalign typalign_type_alignment_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.typalign
    ADD CONSTRAINT typalign_type_alignment_key UNIQUE (type_alignment);


--
-- Name: typcategory typcategory_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.typcategory
    ADD CONSTRAINT typcategory_pkey PRIMARY KEY (typcategory);


--
-- Name: typstorage typstorage_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.typstorage
    ADD CONSTRAINT typstorage_pkey PRIMARY KEY (typstorage);


--
-- Name: typstorage typstorage_type_storage_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.typstorage
    ADD CONSTRAINT typstorage_type_storage_key UNIQUE (type_storage);


--
-- Name: typstorage typstorage_type_storage_notes_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.typstorage
    ADD CONSTRAINT typstorage_type_storage_notes_key UNIQUE (type_storage_notes);


--
-- Name: typtype typtype_pkey; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.typtype
    ADD CONSTRAINT typtype_pkey PRIMARY KEY (typtype);


--
-- Name: typtype typtype_type_type_key; Type: CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.typtype
    ADD CONSTRAINT typtype_type_type_key UNIQUE (type_type);


--
-- Name: privilege_type privilege_type_pkey; Type: CONSTRAINT; Schema: security_lookup; Owner: -
--

ALTER TABLE ONLY security_lookup.privilege_type
    ADD CONSTRAINT privilege_type_pkey PRIMARY KEY (privilege_type);


--
-- Name: privilege_type privilege_type_privilege_code_key; Type: CONSTRAINT; Schema: security_lookup; Owner: -
--

ALTER TABLE ONLY security_lookup.privilege_type
    ADD CONSTRAINT privilege_type_privilege_code_key UNIQUE (privilege_code);


--
-- Name: system_privilege_type system_privilege_type_pkey; Type: CONSTRAINT; Schema: security_lookup; Owner: -
--

ALTER TABLE ONLY security_lookup.system_privilege_type
    ADD CONSTRAINT system_privilege_type_pkey PRIMARY KEY (system_privilege_type);


--
-- Name: valid_object_privileges valid_object_privileges_pkey; Type: CONSTRAINT; Schema: security_lookup; Owner: -
--

ALTER TABLE ONLY security_lookup.valid_object_privileges
    ADD CONSTRAINT valid_object_privileges_pkey PRIMARY KEY (object_type, privilege_type);


--
-- Name: refresh_sequence refresh_sequence_2_pkey1; Type: CONSTRAINT; Schema: update; Owner: -
--

ALTER TABLE ONLY update.refresh_sequence
    ADD CONSTRAINT refresh_sequence_2_pkey1 PRIMARY KEY (refresh_id);


--
-- Name: refresh_sequence refresh_sequence_2_view_group_ordinality_key1; Type: CONSTRAINT; Schema: update; Owner: -
--

ALTER TABLE ONLY update.refresh_sequence
    ADD CONSTRAINT refresh_sequence_2_view_group_ordinality_key1 UNIQUE (view_group, ordinality);


--
-- Name: refresh_sequence refresh_sequence_group_schema_name_uniq; Type: CONSTRAINT; Schema: update; Owner: -
--

ALTER TABLE ONLY update.refresh_sequence
    ADD CONSTRAINT refresh_sequence_group_schema_name_uniq UNIQUE (view_group, view_schema, view_name);


--
-- Name: copy source_pkey; Type: CONSTRAINT; Schema: update; Owner: -
--

ALTER TABLE ONLY update.copy
    ADD CONSTRAINT source_pkey PRIMARY KEY (source_id);


--
-- Name: copy source_target_schema_target_table_key; Type: CONSTRAINT; Schema: update; Owner: -
--

ALTER TABLE ONLY update.copy
    ADD CONSTRAINT source_target_schema_target_table_key UNIQUE (target_schema, target_table);


--
-- Name: search_path search_path_delete; Type: TRIGGER; Schema: meta; Owner: -
--

CREATE TRIGGER search_path_delete INSTEAD OF DELETE ON meta.search_path FOR EACH ROW EXECUTE FUNCTION rule_0.delete_from_search_path_trigger();


--
-- Name: search_path search_path_insert; Type: TRIGGER; Schema: meta; Owner: -
--

CREATE TRIGGER search_path_insert INSTEAD OF INSERT ON meta.search_path FOR EACH ROW EXECUTE FUNCTION rule_0.push_to_search_path_trigger();


--
-- Name: search_path search_path_update; Type: TRIGGER; Schema: meta; Owner: -
--

CREATE TRIGGER search_path_update INSTEAD OF UPDATE ON meta.search_path FOR EACH ROW EXECUTE FUNCTION rule_0.update_search_path_trigger();


--
-- Name: view_key_fields trigger_check_field_exists; Type: TRIGGER; Schema: meta; Owner: -
--

CREATE TRIGGER trigger_check_field_exists BEFORE INSERT OR UPDATE ON meta.view_key_fields FOR EACH ROW EXECUTE FUNCTION rule_0.named_field_must_exist_tg();


--
-- Name: view_keys trigger_check_named_view_is_a_view; Type: TRIGGER; Schema: meta; Owner: -
--

CREATE TRIGGER trigger_check_named_view_is_a_view BEFORE INSERT OR UPDATE ON meta.view_keys FOR EACH ROW EXECUTE FUNCTION rule_0.view_name_must_be_a_view_tg();


--
-- Name: view_key_fields view_key_fields_view_name_key_name_fkey; Type: FK CONSTRAINT; Schema: meta; Owner: -
--

ALTER TABLE ONLY meta.view_key_fields
    ADD CONSTRAINT view_key_fields_view_name_key_name_fkey FOREIGN KEY (view_name, key_name) REFERENCES meta.view_keys(view_name, key_name) ON DELETE CASCADE;


--
-- Name: error_code error_code_error_class_fkey; Type: FK CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.error_code
    ADD CONSTRAINT error_code_error_class_fkey FOREIGN KEY (error_class) REFERENCES meta_lookup.error_class(error_class);


--
-- Name: lock_conflicts lock_conflicts_lock_level_conflicting_request_fkey; Type: FK CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.lock_conflicts
    ADD CONSTRAINT lock_conflicts_lock_level_conflicting_request_fkey FOREIGN KEY (lock_level, conflicting_request) REFERENCES meta_lookup.lock_mode(lock_level, lock_mode);


--
-- Name: lock_conflicts lock_conflicts_lock_level_current_lock_mode_fkey; Type: FK CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.lock_conflicts
    ADD CONSTRAINT lock_conflicts_lock_level_current_lock_mode_fkey FOREIGN KEY (lock_level, current_lock_mode) REFERENCES meta_lookup.lock_mode(lock_level, lock_mode);


--
-- Name: lock_mode lock_mode_lock_level_fkey; Type: FK CONSTRAINT; Schema: meta_lookup; Owner: -
--

ALTER TABLE ONLY meta_lookup.lock_mode
    ADD CONSTRAINT lock_mode_lock_level_fkey FOREIGN KEY (lock_level) REFERENCES meta_lookup.lock_level(lock_level);


--
-- Name: valid_object_privileges valid_object_privileges_privilege_type_fkey; Type: FK CONSTRAINT; Schema: security_lookup; Owner: -
--

ALTER TABLE ONLY security_lookup.valid_object_privileges
    ADD CONSTRAINT valid_object_privileges_privilege_type_fkey FOREIGN KEY (privilege_type) REFERENCES security_lookup.privilege_type(privilege_type);


--
-- PostgreSQL database dump complete
--

