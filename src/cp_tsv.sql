CREATE OR REPLACE PROCEDURE
	cp_tsv(
		from text,
		to   text
	)
AS $$
COPY to
