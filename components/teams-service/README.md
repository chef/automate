# Teams Service

Teams Service is an API for defining Local Teams that will be used as part of
the new Authorization model for A2. Whether the whole AuthZ API will live in
here or just the Local Teams part is subject to debate :)

## Getting Started

### Running Tests

To run tests against an in-memory store, simply run:

```
make test
```

If you wish to run the tests against postgres, first spin a pg docker container up:

```
make setup_docker_pg
```

Once up, you can run the tests over and over:

```
make test_with_db
```

If you wish to run the tests in VSCode or elsewhere, set the following in the proper environment:

```
export PG_URL="postgresql://postgres@127.0.0.1:5432/teams_test?sslmode=disable&timezone=UTC&password=docker"
```

Once you are done testing against the database, kill the container
and unset `PG_URL` if you previously set it:

```
make kill_docker_pg
unset PG_URL
```

** Note: CI will always run both in-memory and postgres tests. **
