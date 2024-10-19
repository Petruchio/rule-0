CREATE OR REPLACE FUNCTION
	regexp_delete(
		base_string TEXT,
		VARIADIC patterns text[]
	)
RETURNS text
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
$$
LANGUAGE
	plpgsql;

