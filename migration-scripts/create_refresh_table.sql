CREATE TABLE "refresh" (
	id uuid NOT NULL DEFAULT uuid_generate_v4 (),
	username varchar NOT NULL,
	CONSTRAINT refresh_pk PRIMARY KEY (id)
);
