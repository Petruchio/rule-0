CREATE VIEW
	meta.tablespace
AS

SELECT
	oid               AS tablespace_id,
	spcname           AS tablespace_name,
	spcowner::regrole AS owner
FROM pg_tablespace;
;

CREATE VIEW
	meta.tablespace_options
AS
WITH
	split
AS (
	SELECT
		spcname            AS tablespace_name,
		UNNEST(spcoptions) AS option
	FROM pg_tablespace
)
SELECT
	tablespace_name,
	split_part(option,'=',1) AS option,
	split_part(option,'=',2) AS value
FROM split
;
