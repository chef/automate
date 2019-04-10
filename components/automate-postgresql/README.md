# automate-postgresql

This is a "wrapper" package for the upstream core/postgresql package.
It contains the configuration and hook templates for our PostgreSQL
service, depending on the upstream binaries in core/postgresql.

Note: Currently this package is locked at the manifest level, changes
to this package will not be seen in live installs unless you also
change A2_ROOT/.expeditor/create-manifest.rb
