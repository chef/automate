module github.com/chef/automate

go 1.13

require (
	github.com/Azure/azure-sdk-for-go v19.1.0+incompatible
	github.com/Azure/go-autorest v10.15.0+incompatible
	github.com/BurntSushi/toml v0.3.1
	github.com/Masterminds/squirrel v1.1.0
	github.com/Microsoft/go-winio v0.4.14 // indirect
	github.com/OneOfOne/xxhash v1.2.5 // indirect
	github.com/alexedwards/scs v1.4.1
	github.com/apache/thrift v0.0.0-20161221203622-b2a4d4ae21c7 // indirect
	github.com/apoydence/onpar v0.0.0-20190519213022-ee068f8ea4d1 // indirect
	github.com/armon/go-metrics v0.3.2 // indirect
	github.com/aws/aws-sdk-go v1.13.42
	github.com/beevik/etree v1.0.0 // indirect
	github.com/blang/semver v3.5.1+incompatible
	github.com/bmizerany/assert v0.0.0-20160611221934-b7ed37b82869 // indirect
	github.com/boltdb/bolt v1.3.2-0.20171120010307-9da317453632
	github.com/briandowns/spinner v0.0.0-20180123222039-b3ed21e9d3b2
	github.com/buger/goterm v0.0.0-20180307092342-c9def0117b24
	github.com/buger/jsonparser v0.0.0-20180808090653-f4dd9f5a6b44
	github.com/chef/go-chef v0.3.1-0.20191115205148-7ac24fba7a27
	github.com/chef/toml v0.3.1-0.20180511201931-e53972c43816
	github.com/ckaznocha/protoc-gen-lint v0.2.1
	github.com/codahale/hdrhistogram v0.0.0-20161010025455-3a0bb77429bd // indirect
	github.com/coreos/go-oidc v2.0.0+incompatible
	github.com/dave/jennifer v0.17.0
	github.com/dexidp/dex v2.19.0+incompatible
	github.com/dgrijalva/jwt-go v3.2.0+incompatible
	github.com/dimchansky/utfbom v0.0.0-20170328061312-6c6132ff69f0 // indirect
	github.com/docker/distribution v2.7.1+incompatible // indirect
	github.com/docker/docker v1.13.1 // indirect
	github.com/docker/go-connections v0.4.0 // indirect
	github.com/docker/go-units v0.4.0 // indirect
	github.com/envoyproxy/protoc-gen-validate v0.1.0
	github.com/fatih/color v1.9.0
	github.com/felixge/httpsnoop v1.0.0 // indirect
	github.com/fortytw2/leaktest v1.3.0 // indirect
	github.com/ghodss/yaml v1.0.1-0.20180503022059-e9ed3c6dfb39
	github.com/go-delve/delve v1.3.1
	github.com/go-gorp/gorp v2.0.1-0.20180410155428-6032c66e0f5f+incompatible
	github.com/gobwas/glob v0.2.3 // indirect
	github.com/gocarina/gocsv v0.0.0-20170928100509-7099e67763c2
	github.com/gofrs/uuid v2.1.0+incompatible
	github.com/golang-migrate/migrate v3.5.4+incompatible
	github.com/golang/mock v1.2.1-0.20190311213431-837231f7bb37
	github.com/golang/protobuf v1.3.3
	github.com/google/go-cloud v0.1.1
	github.com/gorilla/handlers v1.3.0 // indirect
	github.com/gorilla/mux v1.6.1
	github.com/grpc-ecosystem/go-grpc-middleware v1.0.0
	github.com/grpc-ecosystem/go-grpc-prometheus v1.2.0
	github.com/grpc-ecosystem/grpc-gateway v1.12.0
	github.com/grpc-ecosystem/grpc-opentracing v0.0.0-20171214222146-0e7658f8ee99
	github.com/hashicorp/go-hclog v0.12.0 // indirect
	github.com/hashicorp/go-immutable-radix v1.1.0 // indirect
	github.com/hashicorp/golang-lru v0.5.4 // indirect
	github.com/hashicorp/hcl v0.0.0-20171017181929-23c074d0eceb // indirect
	github.com/iancoleman/strcase v0.0.0-20190422225806-e506e3ef7365 // indirect
	github.com/jaswdr/faker v1.0.2
	github.com/jhump/protoreflect v1.5.0
	github.com/jonboulle/clockwork v0.1.0 // indirect
	github.com/kevinburke/go-bindata v3.16.0+incompatible
	github.com/konsorten/go-windows-terminal-sequences v1.0.2 // indirect
	github.com/kylelemons/godebug v0.0.0-20170820004349-d65d576e9348
	github.com/leanovate/gopter v0.2.4
	github.com/lib/pq v1.3.0
	github.com/lyft/protoc-gen-star v0.4.11
	github.com/magiconair/properties v1.7.4 // indirect
	github.com/mattn/go-isatty v0.0.12 // indirect
	github.com/mattn/go-runewidth v0.0.2 // indirect
	github.com/mattn/go-sqlite3 v1.11.0 // indirect
	github.com/mitchellh/mapstructure v0.0.0-20180220230111-00c29f56e238
	github.com/muesli/crunchy v0.0.0-20170927092902-4ec98f770e27
	github.com/nats-io/gnatsd v1.3.1-0.20190311222832-8ba3abef6ac9
	github.com/nats-io/go-nats v1.7.2 // indirect
	github.com/nats-io/nats-streaming-server v0.17.0
	github.com/nats-io/nats.go v1.9.1
	github.com/nats-io/stan.go v0.6.0
	github.com/olivere/elastic v6.2.16+incompatible // indirect
	github.com/open-policy-agent/opa v0.14.2
	github.com/opencontainers/go-digest v1.0.0-rc1 // indirect
	github.com/opentracing/opentracing-go v1.0.2
	github.com/patrickmn/go-cache v2.1.0+incompatible
	github.com/pelletier/go-toml v1.0.1
	github.com/peterbourgon/mergemap v0.0.0-20130613134717-e21c03b7a721
	github.com/pkg/errors v0.8.1
	github.com/pmezard/go-difflib v1.0.0
	github.com/poy/onpar v0.0.0-20190519213022-ee068f8ea4d1 // indirect
	github.com/pquerna/cachecontrol v0.0.0-20171018203845-0dec1b30a021 // indirect
	github.com/prometheus/client_golang v1.4.0
	github.com/prometheus/procfs v0.0.9 // indirect
	github.com/rcrowley/go-metrics v0.0.0-20180125231941-8732c616f529 // indirect
	github.com/russellhaering/goxmldsig v0.0.0-20170911191014-b7efc6231e45 // indirect
	github.com/satori/go.uuid v1.2.0 // indirect
	github.com/schollz/closestmatch v2.1.1-0.20170908204616-19d3b334fdfc+incompatible
	github.com/segmentio/backo-go v0.0.0-20160424052352-204274ad699c // indirect
	github.com/sirupsen/logrus v1.4.3-0.20190701143506-07a84ee7412e
	github.com/spf13/afero v1.0.0 // indirect
	github.com/spf13/cast v1.1.0 // indirect
	github.com/spf13/cobra v0.0.4-0.20180531180338-1e58aa3361fd
	github.com/spf13/jwalterweatherman v0.0.0-20170901151539-12bd96e66386 // indirect
	github.com/spf13/pflag v1.0.1
	github.com/spf13/viper v1.0.1-0.20171207042631-1a0c4a370c3e
	github.com/stretchr/testify v1.4.0
	github.com/teambition/rrule-go v0.0.0-20170616063102-9d6a7aa3e9f9
	github.com/uber-go/atomic v1.4.0 // indirect
	github.com/uber/jaeger-client-go v2.11.2+incompatible
	github.com/uber/jaeger-lib v1.3.1 // indirect
	github.com/xrash/smetrics v0.0.0-20170218160415-a3153f7040e9 // indirect
	github.com/xtgo/uuid v0.0.0-20140804021211-a0b114877d4c // indirect
	github.com/yashtewari/glob-intersection v0.0.0-20180206001645-7af743e8ec84 // indirect
	github.com/ziutek/mymysql v1.5.4 // indirect
	go.uber.org/atomic v1.3.1
	go.uber.org/multierr v1.1.1-0.20180122172545-ddea229ff1df
	go.uber.org/zap v1.7.2-0.20171031232209-f85c78b1dd99
	golang.org/x/crypto v0.0.0-20200214034016-1d94cc7ab1c6
	golang.org/x/oauth2 v0.0.0-20190604053449-0f29369cfe45
	golang.org/x/perf v0.0.0-20190823172224-ecb187b06eb0
	golang.org/x/sys v0.0.0-20200212091648-12a6c2dcc1e4 // indirect
	golang.org/x/text v0.3.1-0.20171218113626-eb22672bea55
	google.golang.org/api v0.0.0-20180829000535-087779f1d2c9
	google.golang.org/genproto v0.0.0-20200211111953-2dc5924e3898
	google.golang.org/grpc v1.27.1
	gopkg.in/asn1-ber.v1 v1.0.0-20170511165959-379148ca0225 // indirect
	gopkg.in/cheggaaa/pb.v1 v1.0.20
	gopkg.in/ldap.v2 v2.5.1 // indirect
	gopkg.in/olivere/elastic.v6 v6.2.28
	gopkg.in/segmentio/analytics-go.v3 v3.0.1
	gopkg.in/square/go-jose.v2 v2.1.3
	gopkg.in/yaml.v2 v2.2.5
)

replace github.com/dexidp/dex => github.com/ryancragun/dex v2.19.0-incompatible+incompatible
