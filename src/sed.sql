CREATE OR REPLACE FUNCTION sed(
	base_string text,
	pattern text,
	replacement text,
	VARIADIC patterns_and_replacements text[]
) RETURNS text AS $$

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
$$ LANGUAGE plpgsql;

