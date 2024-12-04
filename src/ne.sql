BEGIN;

CREATE FUNCTION rule_0.reg_does_not_match_text(regclass, text) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $1::TEXT != $2;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(regcollation, text) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $1::TEXT != $2;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(regconfig, text) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $1::TEXT != $2;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(regdictionary, text) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $1::TEXT != $2;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(regnamespace, text) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $1::TEXT != $2;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(regoper, text) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $1::TEXT != $2;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(regoperator, text) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $1::TEXT != $2;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(regproc, text) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $1::TEXT != $2;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(regprocedure, text) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $1::TEXT != $2;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(regrole, text) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $1::TEXT != $2;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(regtype, text) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $1::TEXT != $2;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regclass) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $2::TEXT != $1;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regcollation) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $2::TEXT != $1;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regconfig) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $2::TEXT != $1;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regdictionary) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $2::TEXT != $1;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regnamespace) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $2::TEXT != $1;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regoper) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $2::TEXT != $1;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regoperator) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $2::TEXT != $1;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regproc) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $2::TEXT != $1;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regprocedure) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $2::TEXT != $1;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regrole) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $2::TEXT != $1;
$_$;

CREATE FUNCTION rule_0.reg_does_not_match_text(text, regtype) RETURNS boolean LANGUAGE sql AS $_$
	SELECT $2::TEXT != $1;
$_$;

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = regclass,
	RIGHTARG = text
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = regcollation,
	RIGHTARG = text
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = regconfig,
	RIGHTARG = text
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = regdictionary,
	RIGHTARG = text
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = regnamespace,
	RIGHTARG = text
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = regoper,
	RIGHTARG = text
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = regoperator,
	RIGHTARG = text
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = regproc,
	RIGHTARG = text
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = regprocedure,
	RIGHTARG = text
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = regrole,
	RIGHTARG = text
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = regtype,
	RIGHTARG = text
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = text,
	RIGHTARG = regclass
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = text,
	RIGHTARG = regcollation
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = text,
	RIGHTARG = regconfig
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = text,
	RIGHTARG = regdictionary
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = text,
	RIGHTARG = regnamespace
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = text,
	RIGHTARG = regoper
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = text,
	RIGHTARG = regoperator
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = text,
	RIGHTARG = regproc
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = text,
	RIGHTARG = regprocedure
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = text,
	RIGHTARG = regrole
);

CREATE OPERATOR rule_0.!= (
	FUNCTION = rule_0.reg_does_not_match_text,
	LEFTARG  = text,
	RIGHTARG = regtype
);

COMMIT;
