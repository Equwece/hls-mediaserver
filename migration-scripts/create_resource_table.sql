CREATE TABLE resource (
	title varchar NOT NULL,
	id uuid NOT NULL DEFAULT uuid_generate_v4 (),
	"type" varchar NOT NULL DEFAULT 'video',
	segmented bool NOT NULL DEFAULT False,
	CONSTRAINT resource_pk PRIMARY KEY (id),
	CONSTRAINT type_check CHECK (type in ('audio', 'video'))
);
