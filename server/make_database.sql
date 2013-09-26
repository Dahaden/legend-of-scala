CREATE TABLE adventurers(
    id INTEGER PRIMARY KEY NOT NULL,
    name VARCHAR(255) UNIQUE NOT NULL,
    token VARCHAR(255) NOT NULL,

    x INTEGER NOT NULL,
    y INTEGER NOT NULL,
    realm_id INTEGER NOT NULL,

    spawn_x INTEGER NOT NULL,
    spawn_y INTEGER NOT NULL,
    spawn_realm_id INTEGER NOT NULL,

    hearts INTEGER NOT NULL,

    FOREIGN KEY(realm_id) REFERENCES realms(id),
    FOREIGN KEY(spawn_realm_id) REFERENCES spawn_realms(id)
);

CREATE TABLE realms(
    id INTEGER PRIMARY KEY NOT NULL,
    name VARCHAR(255) UNIQUE NOT NULL,
    w INTEGER NOT NULL,
    h INTEGER NOT NULL
);

CREATE TABLE items(
    id INTEGER PRIMARY KEY NOT NULL,
    kind VARCHAR(255) NOT NULL,
    owner_id INTEGER,
    attrs TEXT,

    FOREIGN KEY(owner_id) REFERENCES adventurers(id) ON DELETE CASCADE
);

CREATE TABLE features(
    id INTEGER PRIMARY KEY NOT NULL,

    x INTEGER NOT NULL,
    y INTEGER NOT NULL,
    realm_id INTEGER NOT NULL,

    kind VARCHAR(255),
    attrs TEXT,

    FOREIGN KEY(realm_id) REFERENCES realms(id)
);

CREATE TABLE monsters(
    id INTEGER PRIMARY KEY NOT NULL,

    x INTEGER NOT NULL,
    y INTEGER NOT NULL,
    realm_id INTEGER NOT NULL,

    hearts INTEGER NOT NULL,

    kind VARCHAR(255),
    drops TEXT NOT NULL,

    FOREIGN KEY(realm_id) REFERENCES realms(id)
);

INSERT INTO realms(name, w, h) VALUES("world", 150, 150);
