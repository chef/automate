module github.com/chef/automate

go 1.13

require (
	cloud.google.com/go v0.60.0 // indirect
	cloud.google.com/go/storage v1.10.0 // indirect
	github.com/Azure/azure-sdk-for-go v57.2.0+incompatible
	github.com/Azure/go-autorest/autorest v0.11.21
	github.com/Azure/go-autorest/autorest/adal v0.9.16
	github.com/Azure/go-autorest/autorest/azure/auth v0.5.8
	github.com/Azure/go-autorest/autorest/to v0.4.0 // indirect
	github.com/Azure/go-autorest/autorest/validation v0.3.1 // indirect
	github.com/BurntSushi/toml v0.3.1
	github.com/DATA-DOG/go-sqlmock v1.5.0
	github.com/Masterminds/squirrel v1.1.0
	github.com/Microsoft/go-winio v0.4.14 // indirect
	github.com/alexedwards/scs v1.4.1
	github.com/ansrivas/fiberprometheus v0.3.2
	github.com/apache/thrift v0.0.0-20161221203622-b2a4d4ae21c7 // indirect
	github.com/apoydence/onpar v0.0.0-20190519213022-ee068f8ea4d1 // indirect
	github.com/armon/go-metrics v0.3.2 // indirect
	github.com/aws/aws-sdk-go v1.40.32
	github.com/blang/semver v3.5.1+incompatible
	github.com/bmizerany/assert v0.0.0-20160611221934-b7ed37b82869 // indirect
	github.com/boltdb/bolt v1.3.2-0.20171120010307-9da317453632
	github.com/briandowns/spinner v0.0.0-20180123222039-b3ed21e9d3b2
	github.com/bufbuild/buf v0.20.5
	github.com/buger/goterm v0.0.0-20180307092342-c9def0117b24
	github.com/buger/jsonparser v0.0.0-20180808090653-f4dd9f5a6b44
	github.com/chef/automate/api/external v0.0.0-00010101000000-000000000000
	github.com/chef/toml v0.3.1-0.20200730001027-920c30b33b5d
	github.com/ckaznocha/protoc-gen-lint v0.2.1
	github.com/codahale/hdrhistogram v0.0.0-20161010025455-3a0bb77429bd // indirect
	github.com/coreos/go-oidc v2.0.0+incompatible
	github.com/dave/jennifer v0.17.0
	github.com/dexidp/dex v2.19.0+incompatible
	github.com/dgrijalva/jwt-go v3.2.0+incompatible
	github.com/docker/distribution v2.7.1+incompatible // indirect
	github.com/docker/docker v1.13.1 // indirect
	github.com/docker/go-connections v0.4.0 // indirect
	github.com/docker/go-units v0.4.0 // indirect
	github.com/envoyproxy/protoc-gen-validate v0.4.0
	github.com/fatih/color v1.9.0
	github.com/felixge/httpsnoop v1.0.0 // indirect
	github.com/ghodss/yaml v1.0.1-0.20180503022059-e9ed3c6dfb39
	github.com/go-chef/chef v0.23.2-0.20201213054559-3243f736d651
	github.com/go-delve/delve v1.3.1
	github.com/go-gorp/gorp v2.0.1-0.20180410155428-6032c66e0f5f+incompatible
	github.com/gocarina/gocsv v0.0.0-20170928100509-7099e67763c2
	github.com/gofiber/cors v0.2.2
	github.com/gofiber/fiber v1.14.6
	github.com/gofiber/utils v0.0.10
	github.com/gofrs/uuid v3.3.0+incompatible
	github.com/golang-migrate/migrate v3.5.4+incompatible
	github.com/golang/mock v1.4.4
	github.com/golang/protobuf v1.5.2
	github.com/google/go-cmp v0.5.7
	github.com/google/uuid v1.1.2
	github.com/gorilla/handlers v1.3.0 // indirect
	github.com/gorilla/mux v1.7.4
	github.com/grpc-ecosystem/go-grpc-middleware v1.0.0
	github.com/grpc-ecosystem/go-grpc-prometheus v1.2.0
	github.com/grpc-ecosystem/grpc-gateway v1.14.6
	github.com/grpc-ecosystem/grpc-opentracing v0.0.0-20171214222146-0e7658f8ee99
	github.com/hashicorp/go-hclog v0.12.0 // indirect
	github.com/hashicorp/go-immutable-radix v1.1.0 // indirect
	github.com/hashicorp/golang-lru v0.5.4 // indirect
	github.com/hashicorp/hcl/v2 v2.0.0
	github.com/hpcloud/tail v1.0.0
	github.com/iancoleman/strcase v0.0.0-20190422225806-e506e3ef7365 // indirect
	github.com/imdario/mergo v0.3.13
	github.com/jaswdr/faker v1.0.2
	github.com/jedib0t/go-pretty/v5 v5.0.2-alpha
	github.com/jhump/protoreflect v1.7.1-0.20200723220026-11eaaf73e0ec
	github.com/kevinburke/go-bindata v3.16.0+incompatible
	github.com/kylelemons/godebug v0.0.0-20170820004349-d65d576e9348
	github.com/leanovate/gopter v0.2.4
	github.com/lib/pq v1.8.0
	github.com/lyft/protoc-gen-star v0.4.16-0.20200805193024-077ca8f98fb2
	github.com/mattn/go-isatty v0.0.12 // indirect
	github.com/mattn/go-sqlite3 v1.11.0 // indirect
	github.com/minio/minio-go/v7 v7.0.14
	github.com/mitchellh/go-homedir v1.1.0
	github.com/mitchellh/mapstructure v1.3.1
	github.com/muesli/crunchy v0.0.0-20170927092902-4ec98f770e27
	github.com/nats-io/gnatsd v1.3.1-0.20190311222832-8ba3abef6ac9
	github.com/nats-io/go-nats v1.7.2 // indirect
	github.com/nats-io/nats-streaming-server v0.17.0
	github.com/nats-io/nats.go v1.9.1
	github.com/nats-io/stan.go v0.6.0
	github.com/olivere/elastic/v7 v7.0.28
	github.com/open-policy-agent/opa v0.19.1
	github.com/opencontainers/go-digest v1.0.0-rc1 // indirect
	github.com/opentracing/opentracing-go v1.2.0
	github.com/patrickmn/go-cache v2.1.0+incompatible
	github.com/pelletier/go-toml v1.2.0
	github.com/peterbourgon/mergemap v0.0.0-20130613134717-e21c03b7a721
	github.com/peterh/liner v1.2.0 // indirect
	github.com/pkg/errors v0.9.1
	github.com/pmezard/go-difflib v1.0.0
	github.com/poy/onpar v0.0.0-20190519213022-ee068f8ea4d1 // indirect
	github.com/pquerna/cachecontrol v0.0.0-20171018203845-0dec1b30a021 // indirect
	github.com/prometheus/client_golang v1.7.1
	github.com/rcrowley/go-metrics v0.0.0-20200313005456-10cdbea86bc0 // indirect
	github.com/russellhaering/goxmldsig v1.1.0 // indirect
	github.com/schollz/closestmatch v2.1.1-0.20170908204616-19d3b334fdfc+incompatible
	github.com/segmentio/backo-go v0.0.0-20160424052352-204274ad699c // indirect
	github.com/sirupsen/logrus v1.8.1
	github.com/spf13/afero v1.3.4 // indirect
	github.com/spf13/cobra v1.0.1-0.20200713175500-884edc58ad08
	github.com/spf13/pflag v1.0.5
	github.com/spf13/viper v1.7.0
	github.com/stretchr/testify v1.8.0
	github.com/teambition/rrule-go v0.0.0-20170616063102-9d6a7aa3e9f9
	github.com/uber-go/atomic v1.4.0 // indirect
	github.com/uber/jaeger-client-go v2.11.2+incompatible
	github.com/uber/jaeger-lib v1.3.1 // indirect
	github.com/xrash/smetrics v0.0.0-20170218160415-a3153f7040e9 // indirect
	github.com/xtgo/uuid v0.0.0-20140804021211-a0b114877d4c // indirect
	github.com/zclconf/go-cty v1.1.0
	github.com/ziutek/mymysql v1.5.4 // indirect
	go.uber.org/atomic v1.6.0
	go.uber.org/multierr v1.5.0
	go.uber.org/zap v1.15.0
	gocloud.dev v0.19.0
	golang.org/x/crypto v0.0.0-20210513164829-c07d793c2f9a
	golang.org/x/oauth2 v0.0.0-20200107190931-bf48bf16ab8d
	golang.org/x/perf v0.0.0-20190823172224-ecb187b06eb0
	golang.org/x/text v0.3.7
	golang.org/x/tools v0.0.0-20200806022845-90696ccdc692 // indirect
	golang.org/x/xerrors v0.0.0-20200804184101-5ec99f83aff1 // indirect
	google.golang.org/api v0.29.0
	google.golang.org/genproto v0.0.0-20200901141002-b3bf27a9dbd1
	google.golang.org/grpc v1.36.0
	google.golang.org/grpc/examples v0.0.0-20220413171549-7567a5d96538
	google.golang.org/protobuf v1.27.1
	gopkg.in/cheggaaa/pb.v1 v1.0.20
	gopkg.in/ldap.v2 v2.5.1 // indirect
	gopkg.in/robfig/cron.v2 v2.0.0-20150107220207-be2e0b0deed5
	gopkg.in/segmentio/analytics-go.v3 v3.0.1
	gopkg.in/square/go-jose.v2 v2.5.1
	gopkg.in/yaml.v2 v2.3.0
)

// https://github.com/dexidp/dex/issues/1578
replace github.com/dexidp/dex => github.com/ryancragun/dex v2.19.0-incompatible+incompatible

replace github.com/chef/automate/api/external => ./api/external/
