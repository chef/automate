module github.com/chef/automate

go 1.19

require (
	github.com/Azure/azure-sdk-for-go v57.2.0+incompatible
	github.com/Azure/go-autorest/autorest v0.11.21
	github.com/Azure/go-autorest/autorest/adal v0.9.16
	github.com/Azure/go-autorest/autorest/azure/auth v0.5.8
	github.com/BurntSushi/toml v0.3.1
	github.com/DATA-DOG/go-sqlmock v1.5.0
	github.com/Masterminds/squirrel v1.1.0
	github.com/alexedwards/scs v1.4.1
	github.com/apache/thrift v0.0.0-20161221203622-b2a4d4ae21c7 // indirect
	github.com/apoydence/onpar v0.0.0-20190519213022-ee068f8ea4d1 // indirect
	github.com/armon/go-metrics v0.3.2 // indirect
	github.com/aws/aws-sdk-go v1.40.32
	github.com/blang/semver v3.5.1+incompatible
	github.com/boltdb/bolt v1.3.2-0.20171120010307-9da317453632
	github.com/briandowns/spinner v0.0.0-20180123222039-b3ed21e9d3b2
	github.com/bufbuild/buf v0.20.5
	github.com/buger/goterm v0.0.0-20180307092342-c9def0117b24
	github.com/buger/jsonparser v1.1.1
	github.com/chef/automate/api/external v0.0.0-00010101000000-000000000000
	github.com/chef/toml v0.3.1-0.20200730001027-920c30b33b5d
	github.com/ckaznocha/protoc-gen-lint v0.2.1
	github.com/coreos/go-oidc v2.0.0+incompatible
	github.com/dave/jennifer v0.17.0
	github.com/dexidp/dex v2.19.0+incompatible
	github.com/dgrijalva/jwt-go v3.2.0+incompatible
	github.com/envoyproxy/protoc-gen-validate v0.4.0
	github.com/fatih/color v1.9.0
	github.com/ghodss/yaml v1.0.1-0.20180503022059-e9ed3c6dfb39
	github.com/go-chef/chef v0.23.2-0.20201213054559-3243f736d651
	github.com/go-delve/delve v1.3.1
	github.com/go-gorp/gorp v2.0.1-0.20180410155428-6032c66e0f5f+incompatible
	github.com/gocarina/gocsv v0.0.0-20170928100509-7099e67763c2
	github.com/gofrs/uuid v3.3.0+incompatible
	github.com/golang-migrate/migrate v3.5.4+incompatible
	github.com/golang/mock v1.4.4
	github.com/golang/protobuf v1.5.2
	github.com/google/go-cmp v0.5.8
	github.com/google/uuid v1.3.0
	github.com/gorilla/mux v1.7.4
	github.com/grpc-ecosystem/go-grpc-middleware v1.0.0
	github.com/grpc-ecosystem/go-grpc-prometheus v1.2.0
	github.com/grpc-ecosystem/grpc-gateway v1.14.6
	github.com/grpc-ecosystem/grpc-opentracing v0.0.0-20171214222146-0e7658f8ee99
	github.com/hashicorp/hcl/v2 v2.0.0
	github.com/hpcloud/tail v1.0.0
	github.com/imdario/mergo v0.3.13
	github.com/jaswdr/faker v1.0.2
	github.com/jedib0t/go-pretty/v5 v5.0.2-alpha
	github.com/jhump/protoreflect v1.7.1-0.20200723220026-11eaaf73e0ec
	github.com/kevinburke/go-bindata v3.16.0+incompatible
	github.com/kylelemons/godebug v0.0.0-20170820004349-d65d576e9348
	github.com/leanovate/gopter v0.2.4
	github.com/lib/pq v1.10.7
	github.com/lyft/protoc-gen-star v0.4.16-0.20200805193024-077ca8f98fb2
	github.com/mattn/go-sqlite3 v1.11.0 // indirect
	github.com/minio/minio-go/v7 v7.0.63
	github.com/mitchellh/go-homedir v1.1.0
	github.com/mitchellh/mapstructure v1.3.1
	github.com/muesli/crunchy v0.0.0-20170927092902-4ec98f770e27
	github.com/nats-io/gnatsd v1.3.1-0.20190311222832-8ba3abef6ac9
	github.com/nats-io/nats-streaming-server v0.17.0
	github.com/nats-io/nats.go v1.9.1
	github.com/nats-io/stan.go v0.6.0
	github.com/olivere/elastic/v7 v7.0.28
	github.com/open-policy-agent/opa v0.19.1
	github.com/opentracing/opentracing-go v1.2.0
	github.com/patrickmn/go-cache v2.1.0+incompatible
	github.com/pelletier/go-toml v1.2.0
	github.com/peterbourgon/mergemap v0.0.0-20130613134717-e21c03b7a721
	github.com/pkg/errors v0.9.1
	github.com/pmezard/go-difflib v1.0.0
	github.com/poy/onpar v0.0.0-20190519213022-ee068f8ea4d1 // indirect
	github.com/pquerna/cachecontrol v0.0.0-20171018203845-0dec1b30a021 // indirect
	github.com/prometheus/client_golang v1.14.0
	github.com/rcrowley/go-metrics v0.0.0-20200313005456-10cdbea86bc0 // indirect
	github.com/russellhaering/goxmldsig v1.1.0 // indirect
	github.com/schollz/closestmatch v2.1.1-0.20170908204616-19d3b334fdfc+incompatible
	github.com/sirupsen/logrus v1.9.3
	github.com/spf13/cobra v1.0.1-0.20200713175500-884edc58ad08
	github.com/spf13/pflag v1.0.5
	github.com/spf13/viper v1.7.0
	github.com/stretchr/testify v1.8.0
	github.com/teambition/rrule-go v0.0.0-20170616063102-9d6a7aa3e9f9
	github.com/uber/jaeger-client-go v2.11.2+incompatible
	github.com/zclconf/go-cty v1.1.0
	go.uber.org/atomic v1.6.0
	go.uber.org/multierr v1.5.0
	go.uber.org/zap v1.15.0
	gocloud.dev v0.19.0
	golang.org/x/crypto v0.12.0
	golang.org/x/oauth2 v0.0.0-20220223155221-ee480838109b
	golang.org/x/perf v0.0.0-20190823172224-ecb187b06eb0
	golang.org/x/text v0.12.0
	google.golang.org/api v0.30.0
	google.golang.org/genproto v0.0.0-20200901141002-b3bf27a9dbd1
	google.golang.org/grpc v1.36.0
	google.golang.org/grpc/examples v0.0.0-20220413171549-7567a5d96538
	google.golang.org/protobuf v1.28.1
	gopkg.in/cheggaaa/pb.v1 v1.0.20
	gopkg.in/robfig/cron.v2 v2.0.0-20150107220207-be2e0b0deed5
	gopkg.in/segmentio/analytics-go.v3 v3.0.1
	gopkg.in/square/go-jose.v2 v2.5.1
	gopkg.in/yaml.v2 v2.4.0
)

require (
	github.com/ansrivas/fiberprometheus/v2 v2.6.0
	github.com/bmizerany/assert v0.0.0-20160611221934-b7ed37b82869
	github.com/gofiber/fiber/v2 v2.46.0
	github.com/gofiber/utils v1.1.0
	github.com/shirou/gopsutil v3.21.11+incompatible
)

require (
	github.com/chzyer/readline v0.0.0-20180603132655-2972be24d48e // indirect
	github.com/klauspost/cpuid/v2 v2.2.5 // indirect
)

require (
	cloud.google.com/go v0.65.0 // indirect
	cloud.google.com/go/storage v1.10.0
	github.com/Azure/go-autorest v14.2.0+incompatible // indirect
	github.com/Azure/go-autorest/autorest/azure/cli v0.4.2 // indirect
	github.com/Azure/go-autorest/autorest/date v0.3.0 // indirect
	github.com/Azure/go-autorest/autorest/to v0.4.0 // indirect
	github.com/Azure/go-autorest/autorest/validation v0.3.1 // indirect
	github.com/Azure/go-autorest/logger v0.2.1 // indirect
	github.com/Azure/go-autorest/tracing v0.6.0 // indirect
	github.com/Microsoft/go-winio v0.4.14 // indirect
	github.com/OneOfOne/xxhash v1.2.7 // indirect
	github.com/agext/levenshtein v1.2.1 // indirect
	github.com/andybalholm/brotli v1.0.5 // indirect
	github.com/apparentlymart/go-textseg v1.0.0 // indirect
	github.com/asaskevich/govalidator v0.0.0-20180720115003-f9ffefc3facf // indirect
	github.com/beevik/etree v1.1.0 // indirect
	github.com/beorn7/perks v1.0.1 // indirect
	github.com/bgentry/go-netrc v0.0.0-20140422174119-9fd32a8b3d3d // indirect
	github.com/cespare/xxhash/v2 v2.1.2 // indirect
	github.com/codahale/hdrhistogram v0.0.0-20161010025455-3a0bb77429bd // indirect
	github.com/cosiner/argv v0.0.0-20170225145430-13bacc38a0a5 // indirect
	github.com/davecgh/go-spew v1.1.1 // indirect
	github.com/dimchansky/utfbom v1.1.1 // indirect
	github.com/docker/distribution v2.7.1+incompatible // indirect
	github.com/docker/docker v1.13.1 // indirect
	github.com/docker/go-connections v0.4.0 // indirect
	github.com/docker/go-units v0.4.0 // indirect
	github.com/dustin/go-humanize v1.0.1 // indirect
	github.com/felixge/httpsnoop v1.0.0 // indirect
	github.com/fsnotify/fsnotify v1.4.7 // indirect
	github.com/globalsign/mgo v0.0.0-20181015135952-eeefdecb41b8 // indirect
	github.com/go-ole/go-ole v1.2.6 // indirect
	github.com/go-openapi/errors v0.19.0 // indirect
	github.com/go-openapi/strfmt v0.19.0 // indirect
	github.com/gobwas/glob v0.2.3 // indirect
	github.com/gofiber/adaptor/v2 v2.1.31 // indirect
	github.com/gogo/protobuf v1.3.1 // indirect
	github.com/golang-jwt/jwt/v4 v4.0.0 // indirect
	github.com/golang/glog v0.0.0-20160126235308-23def4e6c14b // indirect
	github.com/golang/groupcache v0.0.0-20200121045136-8c9f03a8e57e // indirect
	github.com/google/wire v0.3.0 // indirect
	github.com/googleapis/gax-go v2.0.2+incompatible // indirect
	github.com/googleapis/gax-go/v2 v2.0.5 // indirect
	github.com/gorilla/handlers v1.3.0 // indirect
	github.com/hashicorp/go-hclog v0.12.0 // indirect
	github.com/hashicorp/go-immutable-radix v1.1.0 // indirect
	github.com/hashicorp/go-msgpack v0.5.5 // indirect
	github.com/hashicorp/golang-lru v0.5.4 // indirect
	github.com/hashicorp/hcl v1.0.0 // indirect
	github.com/hashicorp/raft v1.1.1 // indirect
	github.com/iancoleman/strcase v0.0.0-20190422225806-e506e3ef7365 // indirect
	github.com/inconshreveable/mousetrap v1.0.0 // indirect
	github.com/jmespath/go-jmespath v0.4.0 // indirect
	github.com/jonboulle/clockwork v0.2.0 // indirect
	github.com/josharian/intern v1.0.0 // indirect
	github.com/json-iterator/go v1.1.12 // indirect
	github.com/jstemmer/go-junit-report v0.9.1 // indirect
	github.com/klauspost/compress v1.16.7 // indirect
	github.com/klauspost/pgzip v1.2.4 // indirect
	github.com/kr/pretty v0.1.0 // indirect
	github.com/kr/text v0.1.0 // indirect
	github.com/lann/builder v0.0.0-20180802200727-47ae307949d0 // indirect
	github.com/lann/ps v0.0.0-20150810152359-62de8c46ede0 // indirect
	github.com/magiconair/properties v1.8.1 // indirect
	github.com/mailru/easyjson v0.7.7 // indirect
	github.com/manifoldco/promptui v0.9.0
	github.com/mattn/go-colorable v0.1.13 // indirect
	github.com/mattn/go-isatty v0.0.18 // indirect
	github.com/mattn/go-runewidth v0.0.14 // indirect
	github.com/matttproud/golang_protobuf_extensions v1.0.1 // indirect
	github.com/minio/md5-simd v1.1.2 // indirect
	github.com/minio/sha256-simd v1.0.1 // indirect
	github.com/mitchellh/go-wordwrap v1.0.0 // indirect
	github.com/modern-go/concurrent v0.0.0-20180306012644-bacd9c7ef1dd // indirect
	github.com/modern-go/reflect2 v1.0.2 // indirect
	github.com/nats-io/go-nats v1.7.2 // indirect
	github.com/nats-io/jwt v0.3.2 // indirect
	github.com/nats-io/nats-server/v2 v2.1.4 // indirect
	github.com/nats-io/nkeys v0.1.3 // indirect
	github.com/nats-io/nuid v1.0.1 // indirect
	github.com/opencontainers/go-digest v1.0.0-rc1 // indirect
	github.com/peterh/liner v1.2.0 // indirect
	github.com/philhofer/fwd v1.1.2 // indirect
	github.com/pkg/profile v1.5.0 // indirect
	github.com/prometheus/client_model v0.3.0 // indirect
	github.com/prometheus/common v0.37.0 // indirect
	github.com/prometheus/procfs v0.8.0 // indirect
	github.com/rivo/uniseg v0.2.0 // indirect
	github.com/rs/xid v1.5.0 // indirect
	github.com/savsgio/dictpool v0.0.0-20221023140959-7bf2e61cea94 // indirect
	github.com/savsgio/gotils v0.0.0-20230208104028-c358bd845dee // indirect
	github.com/segmentio/backo-go v0.0.0-20160424052352-204274ad699c // indirect
	github.com/spf13/afero v1.3.4 // indirect
	github.com/spf13/cast v1.3.0 // indirect
	github.com/spf13/jwalterweatherman v1.0.0 // indirect
	github.com/stretchr/objx v0.4.0 // indirect
	github.com/subosito/gotenv v1.2.0 // indirect
	github.com/tinylib/msgp v1.1.8 // indirect
	github.com/tklauser/go-sysconf v0.3.11 // indirect
	github.com/tklauser/numcpus v0.6.0 // indirect
	github.com/uber-go/atomic v1.4.0 // indirect
	github.com/uber/jaeger-lib v1.3.1 // indirect
	github.com/valyala/bytebufferpool v1.0.0 // indirect
	github.com/valyala/fasthttp v1.47.0 // indirect
	github.com/valyala/tcplisten v1.0.0 // indirect
	github.com/xrash/smetrics v0.0.0-20170218160415-a3153f7040e9 // indirect
	github.com/xtgo/uuid v0.0.0-20140804021211-a0b114877d4c // indirect
	github.com/yashtewari/glob-intersection v0.0.0-20180916065949-5c77d914dd0b // indirect
	github.com/yusufpapurcu/wmi v1.2.3 // indirect
	github.com/ziutek/mymysql v1.5.4 // indirect
	go.etcd.io/bbolt v1.3.3 // indirect
	go.opencensus.io v0.23.0 // indirect
	go.starlark.net v0.0.0-20190702223751-32f345186213 // indirect
	golang.org/x/arch v0.0.0-20171004143515-077ac972c2e4 // indirect
	golang.org/x/lint v0.0.0-20200302205851-738671d3881b // indirect
	golang.org/x/mod v0.8.0 // indirect
	golang.org/x/net v0.14.0 // indirect
	golang.org/x/sys v0.11.0 // indirect
	golang.org/x/term v0.11.0 // indirect
	golang.org/x/tools v0.6.0 // indirect
	golang.org/x/xerrors v0.0.0-20200804184101-5ec99f83aff1 // indirect
	google.golang.org/appengine v1.6.6 // indirect
	gopkg.in/asn1-ber.v1 v1.0.0-20181015200546-f715ec2f112d // indirect
	gopkg.in/fsnotify.v1 v1.4.7 // indirect
	gopkg.in/ini.v1 v1.67.0 // indirect
	gopkg.in/ldap.v2 v2.5.1 // indirect
	gopkg.in/tomb.v1 v1.0.0-20141024135613-dd632973f1e7 // indirect
	gopkg.in/yaml.v3 v3.0.1 // indirect
)

// https://github.com/dexidp/dex/issues/1578
replace github.com/dexidp/dex => github.com/ryancragun/dex v2.19.0-incompatible+incompatible

replace github.com/chef/automate/api/external => ./api/external/
