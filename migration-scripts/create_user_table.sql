CREATE TABLE "user" (
	username varchar NOT NULL,
	password varchar NOT NULL,
	id uuid NOT NULL DEFAULT uuid_generate_v4 (),
	auth_type varchar NOT NULL DEFAULT 'Regular',
  create_date timestamptz NOT NULL DEFAULT now(),
	CONSTRAINT user_pk PRIMARY KEY (id),
	CONSTRAINT auth_type_check CHECK (auth_type in ('Regular', 'Admin'))
);
