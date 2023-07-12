/*
BEFORE 	INSERT/UPDATE/DELETE 	Tables and foreign tables 	Tables, views, and foreign tables
TRUNCATE 	— 	Tables
AFTER 	INSERT/UPDATE/DELETE 	Tables and foreign tables 	Tables, views, and foreign tables
TRUNCATE 	— 	Tables
INSTEAD OF 	INSERT/UPDATE/DELETE 	Views 	—
TRUNCATE 	— 	—
*/

BEGIN;

CREATE TYPE rule_0.trigger_timing AS ENUM ('Before', 'After', 'Instead of');
CREATE TYPE rule_0.trigger_event  AS ENUM ('Insert', 'Update', 'Delete', 'Truncate');
CREATE TYPE rule_0.trigger_level  AS ENUM ('Row', 'Statement');

CREATE TABLE rule_0.simple_trigger (
	schema    NAME                  NOT NULL,
	relation  NAME                  NOT NULL,
	timing    rule_0.trigger_timing NOT NULL,
	event     rule_0.trigger_event  NOT NULL,
	level     rule_0.trigger_level  NOT NULL,
	command                         TEXT,
	PRIMARY KEY(schema, relation, timing, event, level),
	CHECK( NOT( event = 'Truncate' AND level = 
);

ROLLBACK;
