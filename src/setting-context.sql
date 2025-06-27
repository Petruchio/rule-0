BEGIN;

CREATE SCHEMA setting_lookup;
CREATE TABLE setting_lookup.setting_context (
	security_rank digit PRIMARY KEY,
	context       TEXT  UNIQUE NOT NULL,
	description   TEXT  UNIQUE NOT NULL
);

COPY setting_lookup.setting_context FROM stdin;
1	internal	These settings cannot be changed directly; they reflect internally determined values. Some of them may be adjustable by rebuilding the server with different configuration options, or by changing options supplied to initdb.
2	postmaster	These settings can only be applied when the server starts, so any change requires restarting the server. Values for these settings are typically stored in the postgresql.conf file, or passed on the command line when starting the server. Of course, settings with any of the lower context types can also be set at server start time.
3	sighup	Changes to these settings can be made in postgresql.conf without restarting the server. Send a SIGHUP signal to the postmaster to cause it to re-read postgresql.conf and apply the changes. The postmaster will also forward the SIGHUP signal to its child processes so that they all pick up the new value.
4	superuser-backend	Changes to these settings can be made in postgresql.conf without restarting the server. They can also be set for a particular session in the connection request packet (for example, via libpq's PGOPTIONS environment variable), but only if the connecting user is a superuser or has been granted the appropriate SET privilege. However, these settings never change in a session after it is started. If you change them in postgresql.conf, send a SIGHUP signal to the postmaster to cause it to re-read postgresql.conf. The new values will only affect subsequently-launched sessions.
5	backend	Changes to these settings can be made in postgresql.conf without restarting the server. They can also be set for a particular session in the connection request packet (for example, via libpq's PGOPTIONS environment variable); any user can make such a change for their session. However, these settings never change in a session after it is started. If you change them in postgresql.conf, send a SIGHUP signal to the postmaster to cause it to re-read postgresql.conf. The new values will only affect subsequently-launched sessions.
6	superuser	These settings can be set from postgresql.conf, or within a session via the SET command; but only superusers and users with the appropriate SET privilege can change them via SET. Changes in postgresql.conf will affect existing sessions only if no session-local value has been established with SET.
7	user	These settings can be set from postgresql.conf, or within a session via the SET command. Any user is allowed to change their session-local value. Changes in postgresql.conf will affect existing sessions only if no session-local value has been established with SET.
\.

COMMIT;
