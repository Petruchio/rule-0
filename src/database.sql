CREATE VIEW
	meta.database
AS

SELECT
	dataname                      AS database_name,
	datdba::regrole               AS owner,
	pg_encoding_to_char(encoding) AS encoding,
	locale_provider               AS locale_provider,
	datistemplate                 AS is_a_template,
datfrozenxid xid
datminmxid xid
dattablespace oid (references pg_tablespace.oid)
datcollate text
datctype text
datlocale text
daticurules text
datcollversion text

FROM
	pg_database
NATURAL JOIN
	meta_lookup.dat_loc_provider
;

datacl aclitem[]
datallowconn                  AS connections_allowed,  // Move to security?
dathasloginevt bool
datconnlimit int4

/*
oid oid

Row identifier

datname name

// Database name

datdba oid (references pg_authid.oid)

// Owner of the database, usually the user who created it

encoding int4

// Character encoding for this database (pg_encoding_to_char() can translate this number to the encoding name)

datlocprovider char

// Locale provider for this database: b = builtin, c = libc, i = icu

datistemplate bool

// If true, then this database can be cloned by any user with CREATEDB privileges; if false, then only superusers or the owner of the database can clone it.

datallowconn bool

// If false then no one can connect to this database. This is used to protect the template0 database from being altered.

dathasloginevt bool

// Indicates that there are login event triggers defined for this database. This flag is used to avoid extra lookups on the pg_event_trigger table during each backend startup. This flag is used internally by PostgreSQL and should not be manually altered or read for monitoring purposes.

datconnlimit int4

// Sets maximum number of concurrent connections that can be made to this database. -1 means no limit, -2 indicates the database is invalid.

datfrozenxid xid

// All transaction IDs before this one have been replaced with a permanent (“frozen”) transaction ID in this database. This is used to track whether the database needs to be vacuumed in order to prevent transaction ID wraparound or to allow pg_xact to be shrunk. It is the minimum of the per-table pg_class.relfrozenxid values.

datminmxid xid

// All multixact IDs before this one have been replaced with a transaction ID in this database. This is used to track whether the database needs to be vacuumed in order to prevent multixact ID wraparound or to allow pg_multixact to be shrunk. It is the minimum of the per-table pg_class.relminmxid values.

dattablespace oid (references pg_tablespace.oid)

// The default tablespace for the database. Within this database, all tables for which pg_class.reltablespace is zero will be stored in this tablespace; in particular, all the non-shared system catalogs will be there.

datcollate text

// LC_COLLATE for this database

datctype text

// LC_CTYPE for this database

datlocale text

// Collation provider locale name for this database. If the provider is libc, datlocale is NULL; datcollate and datctype are used instead.

daticurules text

// ICU collation rules for this database

datcollversion text

// Provider-specific version of the collation. This is recorded when the database is created and then checked when it is used, to detect changes in the collation definition that could lead to data corruption.

datacl aclitem[]

// Access privileges; see Section 5.8 for details
*/
