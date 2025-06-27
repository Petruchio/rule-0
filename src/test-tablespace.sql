CREATE TABLESPACE test_tablespace
  LOCATION '/usr/local/pgsql/data/test_tablespace'
  WITH (
    seq_page_cost              = 1.0,
    random_page_cost           = 4.0,
    effective_io_concurrency   = 1,
    maintenance_io_concurrency = 1
  );

