Chef Delivery PostgreSQL Schema
===============================

# Modifying the Schema

The schema for the Chef Delivery is managed using [sqitch][], a
database-agnostic tool for managing schema changes using plain old
SQL.

* [Set up Sqitch](doc/setup_sqitch.md)
* [Using Sqitch](doc/using_sqitch.md)

# Adding Tests

We use [pgTAP][] to test both the schema and the stored procedures in
the database.

* [Developer Setup for Local Testing](doc/setup_pgtap.md)
* [Writing pgTAP Tests for Chef Delivery](doc/writing_tests.md)

# Bringing It All Together

You've got schema patches and tests; how do you bring it all together
and verify everything works?

```
make
```

This will create a new database, load all sqitch changesets, install
the testing framework and test functions, and finally run the tests.

If you want to do any of those steps individually, there are `make`
targets for each of them; consult the [Makefile](Makefile)

## Running Tests under Guard

Alternatively, you can use [Guard][] to run the tests whenever your
schema or test files change.

```
bundle install
bundle exec guard
```

[pgTAP]:http://pgtap.org
[sqitch]:http://sqitch.org
[Guard]:https://github.com/guard/guard
