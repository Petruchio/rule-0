BEGIN;

DROP   VIEW IF EXISTS my.new_session_search_path;
CREATE VIEW           my.new_session_search_path AS

WITH
	rval
AS (
	SELECT
		reset_val
	FROM
		my.settings
	WHERE
		name = 'search_path'
)

SELECT
	ordinality,
	schema_name
FROM
	string_to_table(
		(SELECT reset_val FROM rval),
			', '
		)
		WITH ORDINALITY
			x(schema_name, ordinality)
;

/*
CREATE TRIGGER                new_session_search_path_delete
INSTEAD OF DELETE ON          my.new_session_search_path
FOR EACH ROW EXECUTE FUNCTION rule_0.delete_from_search_path_trigger();

CREATE TRIGGER                new_session_search_path_insert
INSTEAD OF INSERT ON          my.new_session_search_path
FOR EACH ROW EXECUTE FUNCTION rule_0.push_to_search_path_trigger();

CREATE TRIGGER                new_session_search_path_update
INSTEAD OF UPDATE ON          my.new_session_search_path
FOR EACH ROW EXECUTE FUNCTION rule_0.update_search_path_trigger();
*/

COMMIT;
