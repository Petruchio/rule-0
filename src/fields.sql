BEGIN;

DROP VIEW meta.fields;

CREATE OR REPLACE VIEW meta.fields AS
 SELECT
    c.relnamespace::regnamespace AS schema_name,
    c.relname AS relation_name,
    a.attnum AS field_order,
    a.attname AS field_name,
    a.atttypid::regtype AS field_type,
    a.attnotnull = false AS nullable,
    a.atthasdef AS has_default,
    COALESCE(attgenerated.generated, 'Unknown Value'::text) AS generated,
    COALESCE(attidentity.identity, 'Unknown Value'::text) AS identity
   FROM pg_attribute a
     JOIN pg_class c ON a.attrelid = c.oid
     LEFT JOIN meta_lookup.attidentity USING (attidentity)
     LEFT JOIN meta_lookup.attgenerated USING (attgenerated)
  WHERE a.attnum > 0
  ORDER BY c.relnamespace::regnamespace::text, c.relname, a.attnum
;

COMMIT;
