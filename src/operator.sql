CREATE VIEW meta.operator AS
SELECT
	oprnamespace::regnamespace         AS operator_schema,
	oid::regoperator                   AS operator_name,
	oprowner::regrole                  AS owner,
	operator_kind,
	oprcanmerge                        AS can_merge,
	oprcanhash                         AS can_hash,
	NULLIF(oprleft, 0)::regtype        AS left_type,
	oprright::regtype                  AS right_type,
	NULLIF(oprresult, 0)::regtype      AS result_type,
	NULLIF(oprcom, 0)::regoperator     AS commutator_type,
	NULLIF(oprnegate, 0)::regoperator  AS negator_type,
	NULLIF(oprcode, 0)::regproc        AS operator_function,
	NULLIF(oprrest, 0)::regproc        AS restrict_estimator,
	NULLIF(oprjoin, 0)::regproc        AS join_estimator
FROM
	pg_operator
NATURAL JOIN
	meta_lookup.oprkind
;
