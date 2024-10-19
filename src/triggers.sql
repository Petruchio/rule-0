

SELECT
	NULLIF(tgparentid, 0)     AS parent_id,
	t.oid                     AS trigger_id,
	nspname                   AS schema_name,
	relname                   AS relation,
	proname                   AS function_name,
	scope,
	operation,
	timing,
	trigger_enabled_condition AS condition,
	-- tgconstrrelid,
	-- tgconstrindid,
	-- tgconstraintoid,
	tgconstraint  != 0        AS constraint,
	tgconstrrelid != 0        AS foreign_key,
	tgisinternal              AS internal,
	tgdeferrable              AS deferrable,
	tginitdeferred            AS initially_deferred,
	tgnargs                   AS number_of_arguments,
	-- tgattr
	-- tgargs
	tgqual                    AS node_tree
	-- tgoldtable
	-- tgnewtable
FROM
	pg_trigger   AS t
JOIN
	pg_class     AS c
ON
	tgrelid = c.oid
JOIN
	pg_namespace AS s
ON
	c.relnamespace = s.oid
JOIN
	pg_proc      AS f
ON
	tgfoid = f.oid
NATURAL JOIN
	meta.tgtype
NATURAL JOIN
	meta.trigger_enabled_conditions
;
