# Rule 0

## Purpose

This project aims to take PostgreSQL metadata as seriously as its data,
and to allow PostgreSQL to be managed through relational operations.

## Scope

Rule 0 aims to provide:

* Clean, simple representations of PostgreSQL metadata
* Tables resolving all the lookup values found in the PostgreSQL documentation
* The capacity to manage the database through relational operations (such as
  INSERT, UPDATE, and DELETE)
* Comments documenting all the database entities which are part of Rule 0 as
  well as PostgreSQL itself

At some point, the PostgreSQL documentation itself should be included as well,
so that it is available from within PostgreSQL.

## Background

The name "Rule 0" refers to the 0th of Edgar Codd's 12 rules for relational
databases:

### Rule 0: The foundation rule
> For any system that is advertised as, or claimed to be, a relational data
> base management system, that system must be able to manage data bases
> entirely through its relational capabilities.

Rules 1 and 4 are relevant as well:

### Rule 01 The foundation rule
> All information in a relational data base is represented explicitly at
> the logical level and in exactly one way – by values in tables.

### Rule 4: Dynamic online catalog based on the relational model
> The data base description is represented at the logical level in the same
> way as ordinary data, so that authorized users can apply the same relational
> language to its interrogation as they apply to the regular data.

## Status

Rule 0 is already pretty big, and has a lot of useful stuff in it.  It is still,
however, a young project.  There may be mistakes, and there will probably be
changes in the future which break compatibility with this early release.

## Installation

The project can be installed to an existing database in this manner:

`make db=<database name> install`

Stipulating the database name, rather than using the default database when
one is not supplied, is required for safety.  This may be relaxed when the
project matures.

Installing Rule 0 to a database will create the following schemata:
* rule_0
* meta
* meta_lookup
* security
* security_lookup
* update

Well over 100 relations  populate these schemata, along with many functions.
These will be better documented in subsequent releases of the project, but
many useful tables and functions are fairly obvious to one casually browsing
the system.

## Examples

### Metadata

Using psql, you'd get the list of existing schemata like this:

`rule_0=# \dn
           List of schemas
      Name       |       Owner
-----------------+-------------------
 meta            | ray
 meta_lookup     | ray
 public          | pg_database_owner
 rule_0          | ray
 security        | ray
 security_lookup | ray
 update          | ray
(7 rows)`

With Rule 0, you perform an actual query on the schemata view.  Here we'll use
PostgreSQL's TABLE keyword, the little-known (but great) equivalent of SELECT
* FROM

`rule_0=# TABLE meta.schemata;
-[ RECORD 1 ]--------------------------------------------------------------------------------
schema_id   | 12,406
schema_name | information_schema
owner       | postgres
description | ∅
-[ RECORD 2 ]--------------------------------------------------------------------------------
schema_id   | 716,091
schema_name | meta
owner       | ray
description | PostgreSQL metadata, formatted for human beings.
-[ RECORD 3 ]--------------------------------------------------------------------------------
schema_id   | 716,092
schema_name | meta_lookup
owner       | ray
description | Lookup tables to resolve codes and IDs in PostgreSQL metadata.
-[ RECORD 4 ]--------------------------------------------------------------------------------
schema_id   | 11
schema_name | pg_catalog
owner       | postgres
description | system catalog schema
-[ RECORD 5 ]--------------------------------------------------------------------------------
schema_id   | 99
schema_name | pg_toast
owner       | postgres
description | reserved schema for TOAST tables
-[ RECORD 6 ]--------------------------------------------------------------------------------
schema_id   | 716,093
schema_name | rule_0
owner       | ray
description | Entities related to the Rule 0 project for controlling PostgreSQL relationally.
-[ RECORD 7 ]--------------------------------------------------------------------------------
schema_id   | 716,094
schema_name | security
owner       | ray
description | PostgreSQL security information.
-[ RECORD 8 ]--------------------------------------------------------------------------------
schema_id   | 716,095
schema_name | security_lookup
owner       | ray
description | Lookup tables to resolve codes and IDs in PostgreSQL security data.
-[ RECORD 9 ]--------------------------------------------------------------------------------
schema_id   | 716,096
schema_name | update
owner       | ray
description | Information on the status and dependencies of materialized views.`

This is not unlike what you'd see in information_schema.schemata, though it
actually shows you the documentation on the schemata.

A more interesting case, though, would be if you wanted to know how much space
a table took up.  Normally you'd say this:

`rule_0=# SELECT PG_TOTAL_RELATION_SIZE('meta_lookup.srsubstate');
 pg_relation_size
------------------
           49,152
(1 row)`

Or if you want something a little easier to read:

`rule_0=# SELECT PG_SIZE_PRETTY(PG_TOTAL_RELATION_SIZE('meta_lookup.srsubstate'));
 pg_size_pretty
----------------
 48kB
(1 row)`

With Rule 0, you can say:

`rule_0=# SELECT * FROM meta.relation_size WHERE relation = 'srsubstate';
 relation_type | schema_name |  relation  | bytes  | size
---------------+-------------+------------+--------+-------
 Table         | meta_lookup | srsubstate | 49,152 | 48 kB
(1 row)`


### Metadata Metadata

If you've worked much with PostgreSQL metadata, you've had to check the
documentation for what all kinds of inscrutible values mean.  Then you
generally wind up stuffing these into CASE statements in oversized queries.

Rule 0 has brought most of these into the database, where they are kept
in the meta_lookup and security_lookup schemata.  For instance:

`rule_0=# select * from meta_lookup.relkind ;
 relkind |   relation_type   
---------+-------------------
 i       | Index
 S       | Sequence
 t       | Toast Table
 v       | View
 m       | Materialized View
 c       | Composite Type
 f       | Foreign Table
 p       | Partitioned Table
 I       | Partitioned Index
 r       | Table
(10 rows)`

There are presently almost 40 tables full of this stuff.  Because you
know... this is *data*.  PostgreSQL is a database.  It's nice having
the data in the database, rather than spread out in web pages.

### Configuration

We'll begin with a trivial example.  It's a simple enough case that one might
reasonably wonder what is gained by it, but the benefits of a uniform,
relational approach to system configuration are more easily seen with examples
less suited to an introduction.

Suppose you are working with the psql client, and you want to check your
search path.  That looks like this:

`rule_0=# SHOW search_path ;
   search_path
-----------------
 "$user", public
(1 row)`

The information is *sort of* presented as a table.  But it isn't really useful
as a table; you can't use the result as part of another query, for instance.
It isn't normalized, either; it lumps multiple values onto a single row as a
string.

Using Rule 0, you could say this:

`rule_0=# SELECT * FROM meta.search_path ;
 ordinality | schema_name
------------+-------------
          1 | "$user"
          2 | public
(2 rows)`

Here we're peforming an actual query.  That allows us to use our standard SQL
tricks on this:

`rule_0=# SELECT * FROM meta.search_path WHERE ordinality > 1;
 ordinality | schema_name
------------+-------------
          2 | public
(1 row)`

Suppose you wanted to add 'meta' to your search path. Ordinarily use SET:

`rule_0=# SET search_path TO "$user", public, meta;
SET`

The trouble there is that you need to give the full search path; what you had
before, and 'meta' as well.  There is a way around that:

`SELECT set_config('search_path', current_setting('search_path') || ', meta', false);`

The Rule 0 solution is a bit easier to remember:

`rule_0=# INSERT INTO meta.search_path (schema_name) VALUES ('meta');
INSERT 0 0`

(Note that the number of lines inserted is wrong.  Fixing that is on the
agenda.)

Now, checking our search path with SHOW (so that we know that the results
aren't just seen in Rule 0), we see:

`rule_0=# SHOW search_path;
      search_path
-----------------------
 "$user", public, meta`

Okay, that worked.  But what did we gain by all that?

Well, suppose we want to search *all* of our schemata.  We could say this:

`INSERT INTO meta.search_path (schema_name) SELECT schema_name FROM meta.schemata;`

Here we pull the schemata names from another Rule 0 view, meta.schemata, and
add them all to our search path.  Here we'll use meta.search_path itself to check
the result, because the search_path variable is getting annoyingly long:

`rule_0=# SELECT * from meta.search_path;
 ordinality |    schema_name
------------+--------------------
          1 | "$user"
          2 | public
          3 | information_schema
          4 | meta
          5 | meta_lookup
          6 | pg_catalog
          7 | pg_toast
          8 | rule_0
          9 | security
         10 | security_lookup
         11 | update
(11 rows)`

That was a lot faster, and less annoying, than doing it by hand.  Note that we
don't get duplicate entries; meta.search_path understands that the list must be
unique.

Looking at it now, though... perhaps we don't actually want to search
pg_catalog and pg_toast.  We can remove entries from our search path
with a relational operation, too:

`rule_0=# DELETE FROM search_path WHERE schema_name ~ '^pg_';
DELETE 0
(1 row)
rule_0=# SELECT * from meta.search_path ;
 ordinality |    schema_name
------------+--------------------
          1 | "$user"
          2 | public
          3 | information_schema
          4 | meta
          5 | meta_lookup
          6 | rule_0
          7 | security
          8 | security_lookup
          9 | update
(9 rows)
`

Other than that vexing "DELETE 0" message (which *really* needs fixed soon),
it worked as it should.  We were able to use a regular expresion to kill two
schemata with one stone.

If you've learned to write good queries, that effort should pay off in
administration, too.  Generality gives you more power and requires you to
memorize fewer special commands.

This is the way Rule 0 should work:  the relational database can be managed
relationally.

### Security

Rule 0 has a good number of views related to the security of the system.
For instance, in psql you'd look at your users this way:

`rule_0=# \du
                                 List of roles
    Role name     |                         Attributes
------------------+------------------------------------------------------------
 alex             | Superuser
 postgres         | Superuser, Create role, Create DB, Replication, Bypass RLS
 ray              | Superuser
 universal_reader | Cannot login
 will             |`

With Rule 0, you can say:

`rule_0=# SELECT * FROM security.users ;
 user_id | user_name | password | password_expiry | connection_limit 
---------+-----------+----------+-----------------+------------------
      10 | postgres  | ******** | ∅               |                ∅
  16,384 | ray       | ******** | ∅               |                ∅
  16,420 | alex      | ******** | ∅               |                ∅
 644,781 | will      | ******** | ∅               |                ∅
(4 rows)`

Note that universal_reader does not appear in security.users; that's because it
doesn't regard a role which cannot log in as a user. It does appear in the
roles view, however:

`rule_0=# SELECT * FROM security.roles;
 role_id |          role_name          | role_inherits 
---------+-----------------------------+---------------
      10 | postgres                    | t
   6,171 | pg_database_owner           | t
   6,181 | pg_read_all_data            | t
   6,182 | pg_write_all_data           | t
   3,373 | pg_monitor                  | t
   3,374 | pg_read_all_settings        | t
   3,375 | pg_read_all_stats           | t
   3,377 | pg_stat_scan_tables         | t
   4,569 | pg_read_server_files        | t
   4,570 | pg_write_server_files       | t
   4,571 | pg_execute_server_program   | t
   4,200 | pg_signal_backend           | t
   4,544 | pg_checkpoint               | t
   4,550 | pg_use_reserved_connections | t
   6,304 | pg_create_subscription      | t
  16,384 | ray                         | t
  16,420 | alex                        | t
 644,730 | universal_reader            | t
(22 rows)`

## Domains

Rule 0 contains some general-purpose domains (which are kind of like custom
types).  For instance:

* digit (an integer between 0 and 9 (inclusive))
* counting_number (an INT greater than 0)
* natural_number (an INT no less than 0)

There are big_ and small_ variants on the natural and counting numbers as well,
corresponding to the bigint and smallint base types.

More such domains will be added in the future.

## Other stuff

There's actually a lot more stuff in the project.  Install it, explore, and
have fun!
