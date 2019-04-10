package testconstants

// SkipPGMessageFmt is the friendly message the developer sees when any tests
// requiring postgres are skipped because the connection failed.
//
// Yes those lines are long, but if we linebreak, the output ends up starting
// with TAB characters, which makes it harder to copy-paste the commands.
const SkipPGMessageFmt = `
-------------------------------------------------------------------------------

        /!\ Tests that require connecting to PostgreSQL on
            %s
            have been skipped.

  If you want to exercise those tests, ensure that the PG instance pointed to,
  as well as the used credentials, are correct.

  For using docker, run

  docker run --name authn-postgres -e POSTGRES_PASSWORD=authn -e POSTGRES_USER=authn \
    -e POSTGRES_DB=authn -p 15432:5432 -d postgres:9
  env 'PG_URL=postgresql://authn:authn@127.0.0.1:15432/authn?sslmode=disable' make test

  in components/authn-service.

-------------------------------------------------------------------------------
`
