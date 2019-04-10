# Server

## Install Erlang

Erlang is needed to develop and test the server. Version 18 needs to be install. Below
is the command to install this version with brew.

```
brew install erlang@18
```

### Tests

To run the below tests you must first setup sqitch and pgtab. The two below READMEs will help to
install these programs.
*   [Get set up](server/schema/doc/setup_sqitch.md) to hack on the schema
*   [Get set up](server/schema/doc/setup_pgtap.md) to run schema tests

Server tests run against Postgres and they currently assume you have a running
instance of Postgres on your local machine. The easiest way to set that up is
to download and install [Postgres.app](http://postgresapp.com/). Once that's
running, change to the `server` directory, then run:

```
./rebar clean
./rebar get-deps
EUNIT_DROP_DATABASE=1 make clean compile test
```

### Docker Hot Code Reloading

When `is_dev_mode` is true in `config/sys.config`, the hot code sync process in the server
uses about 30% of a CPU constantly, partially due to `osxfs` performance.

Users can switch this setting to `false` when they are not working on the
workflow_api app to save CPU.
