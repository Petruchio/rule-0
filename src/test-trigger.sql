/*
CREATE OR REPLACE FUNCTION setting_bol_id_is_a_nono()
RETURNS trigger AS $nono$
BEGIN
	IF NEW.bol_id IS NOT NULL THEN
		RAISE NOTICE 'Column bol_is is generated, and cannot be set.';
		INSERT INTO bol(bol_id) VALUES (NEW.bol_id);
	END IF;
	RETURN NEW;
END
$nono$ LANGUAGE plpgsql;
*/


CREATE FUNCTION
	no_evens()
RETURNS TRIGGER
AS $$
BEGIN
	RAISE NOTICE 'Called!';
	RETURN NEW;
END
$$
LANGUAGE PLPGSQL;

CREATE TRIGGER check_update
	AFTER INSERT ON stuff
	EXECUTE FUNCTION no_evens();
