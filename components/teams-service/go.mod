module github.com/chef/automate/components/teams-service

go 1.15

replace github.com/chef/automate/api/external v0.0.0-00010101000000-000000000000 => github.com/chef/automate/api/external v0.0.0-20221031101020-dc6ea9deee9e

replace github.com/dexidp/dex => github.com/ryancragun/dex v2.19.0-incompatible+incompatible

replace github.com/chef/automate v0.0.0-20221031101020-dc6ea9deee9e => ./../../

require (
	github.com/chef/automate v0.0.0-20221031101020-dc6ea9deee9e
	github.com/chef/automate/api/external v0.0.0-20221031101020-dc6ea9deee9e
	github.com/grpc-ecosystem/go-grpc-middleware v1.3.0
	github.com/lib/pq v1.8.0
	github.com/pkg/errors v0.9.1
	github.com/spf13/cobra v1.6.1
	github.com/spf13/viper v1.13.0
	github.com/stretchr/testify v1.8.0
	google.golang.org/grpc v1.46.2
)
