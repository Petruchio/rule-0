BEGIN;
DROP VIEW meta.schemata;
CREATE OR REPLACE VIEW meta.schemata AS
 SELECT nsp.oid AS schema_id,
 nsp.oid::regnamespace AS schema_name,
    u.usename AS owner,
    obj_description(nsp.oid) AS description
   FROM pg_namespace nsp
     JOIN pg_user u ON nsp.nspowner = u.usesysid
  ORDER BY nsp.nspname
;
COMMIT;
