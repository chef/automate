# encoding: utf-8
# copyright: Copyright (c) 2019-2025 Progress Software Corporation and/or its subsidiaries or affiliates. All Rights Reserved.
# license: All rights reserved

title 'Postgres Data Migration Integration Tests'

control 'postgres-data-migration-1' do
  title 'postgres data migration'
  desc 'Verify A1 data is copied to A2\'s postgres'

  # TODO(ssd) 2018-06-01: Make this test better once we have
  # a1-migration-data with psql samples.
  #
  # describe "jobs are preserved" do
  #   subject do
  #     cmd = [
  #       "PGUSER=automate",
  #       "PGHOST=localhost",
  #       "PGPORT=5432",
  #       "PGDATABASE=delivery",
  #       "PGSSLMODE=require",
  #       "PGSSLKEY=/hab/svc/automate-postgresql/config/server.key",
  #       "PGSSLCERT=/hab/svc/automate-postgresql/config/server.crt",
  #       "PGSSLROOTCERT=/hab/svc/automate-postgresql/config/root.crt",
  #       "hab pkg exec chef/automate-postgresql",
  #       "psql",
  #       "-c 'SELECT * FROM jobs' -t -A"
  #     ].join(' ')
  #     command(cmd)
  #   end
  #
  #   its('stderr') { should eq "" }
  #   its('exit_status') { should eq 0 }
  # end

end
