module github.com/chef/automate

go 1.24.4

require (
	github.com/Azure/azure-sdk-for-go v68.0.0+incompatible
	github.com/Azure/go-autorest/autorest v0.11.29
	github.com/Azure/go-autorest/autorest/adal v0.9.24
	github.com/Azure/go-autorest/autorest/azure/auth v0.5.13
	github.com/BurntSushi/toml v1.4.0
	github.com/DATA-DOG/go-sqlmock v1.5.0
	github.com/Masterminds/squirrel v1.5.4
	github.com/alexedwards/scs v1.4.1
	github.com/armon/go-metrics v0.4.1 // indirect
	github.com/aws/aws-sdk-go v1.55.5
	github.com/blang/semver v3.5.1+incompatible
	github.com/boltdb/bolt v1.3.2-0.20171120010307-9da317453632
	github.com/briandowns/spinner v0.0.0-20180123222039-b3ed21e9d3b2
	github.com/buger/goterm v1.0.4
	github.com/buger/jsonparser v1.1.1
	github.com/chef/toml v0.3.1-0.20200807000621-f5591d7db659
	github.com/ckaznocha/protoc-gen-lint v0.2.1
	github.com/coreos/go-oidc v2.2.1+incompatible
	github.com/dave/jennifer v1.7.0
	github.com/envoyproxy/protoc-gen-validate v1.2.1
	github.com/fatih/color v1.17.0
	github.com/ghodss/yaml v1.0.1-0.20180503022059-e9ed3c6dfb39
	github.com/go-chef/chef v0.30.1
	github.com/go-delve/delve v1.3.1
	github.com/go-gorp/gorp v2.2.0+incompatible
	github.com/gocarina/gocsv v0.0.0-20240520201108-78e41c74b4b1
	github.com/gofrs/uuid v4.4.0+incompatible
	github.com/golang-migrate/migrate v3.5.4+incompatible
	github.com/golang/mock v1.6.0
	github.com/google/go-cmp v0.7.0
	github.com/google/uuid v1.6.0
	github.com/gorilla/mux v1.8.1
	github.com/grpc-ecosystem/go-grpc-middleware v1.4.0
	github.com/grpc-ecosystem/go-grpc-prometheus v1.2.0
	github.com/grpc-ecosystem/grpc-gateway v1.16.0
	github.com/grpc-ecosystem/grpc-opentracing v0.0.0-20180507213350-8e809c8a8645
	github.com/hashicorp/hcl/v2 v2.22.0
	github.com/hpcloud/tail v1.0.0
	github.com/imdario/mergo v0.3.16
	github.com/jaswdr/faker v1.0.2
	github.com/jedib0t/go-pretty/v5 v5.0.2-alpha
	github.com/jhump/protoreflect v1.10.3
	github.com/kevinburke/go-bindata v3.16.0+incompatible
	github.com/kylelemons/godebug v1.1.0
	github.com/leanovate/gopter v0.2.4
	github.com/lib/pq v1.10.9
	github.com/lyft/protoc-gen-star v0.6.2
	github.com/minio/minio-go/v7 v7.0.72
	github.com/mitchellh/go-homedir v1.1.0
	github.com/mitchellh/mapstructure v1.5.0
	github.com/muesli/crunchy v0.4.0
	github.com/nats-io/nats-streaming-server v0.24.6
	github.com/nats-io/nats.go v1.39.1
	github.com/nats-io/stan.go v0.10.2
	github.com/olivere/elastic/v7 v7.0.28
	github.com/open-policy-agent/opa v1.4.0
	github.com/opentracing/opentracing-go v1.2.0
	github.com/patrickmn/go-cache v2.1.0+incompatible
	github.com/pelletier/go-toml v1.9.5
	github.com/peterbourgon/mergemap v0.0.1
	github.com/pkg/errors v0.9.1
	github.com/pmezard/go-difflib v1.0.1-0.20181226105442-5d4384ee4fb2
	github.com/poy/onpar v0.0.0-20190519213022-ee068f8ea4d1 // indirect
	github.com/pquerna/cachecontrol v0.2.0 // indirect
	github.com/prometheus/client_golang v1.21.1
	github.com/rcrowley/go-metrics v0.0.0-20201227073835-cf1acfcdf475 // indirect
	github.com/russellhaering/goxmldsig v1.1.1 // indirect
	github.com/schollz/closestmatch v2.1.1-0.20170908204616-19d3b334fdfc+incompatible
	github.com/sirupsen/logrus v1.9.3
	github.com/spf13/cobra v1.9.1
	github.com/spf13/pflag v1.0.6
	github.com/spf13/viper v1.20.1
	github.com/stretchr/testify v1.10.0
	github.com/teambition/rrule-go v1.8.2
	github.com/uber/jaeger-client-go v2.30.0+incompatible
	github.com/zclconf/go-cty v1.15.0
	go.uber.org/atomic v1.11.0
	go.uber.org/multierr v1.11.0
	go.uber.org/zap v1.27.0
	gocloud.dev v0.22.0
	golang.org/x/crypto v0.36.0
	golang.org/x/oauth2 v0.26.0
	golang.org/x/perf v0.0.0-20190823172224-ecb187b06eb0
	golang.org/x/text v0.23.0
	google.golang.org/api v0.215.0
	google.golang.org/genproto v0.0.0-20241118233622-e639e219e697 // indirect
	google.golang.org/grpc v1.71.1
	google.golang.org/grpc/examples v0.0.0-20220413171549-7567a5d96538
	google.golang.org/protobuf v1.36.6
	gopkg.in/cheggaaa/pb.v1 v1.0.28
	gopkg.in/robfig/cron.v2 v2.0.0-20150107220207-be2e0b0deed5
	gopkg.in/segmentio/analytics-go.v3 v3.1.0
	gopkg.in/square/go-jose.v2 v2.6.0
	gopkg.in/yaml.v2 v2.4.0
)

require (
	github.com/ansrivas/fiberprometheus/v2 v2.6.0
	github.com/bmizerany/assert v0.0.0-20160611221934-b7ed37b82869
	github.com/bufbuild/buf v1.48.0
	github.com/chef/automate/api/external v0.0.0-20240828051912-8c022fa4f66b
	github.com/dexidp/dex v2.38.0+incompatible
	github.com/dexidp/dex/api/v2 v2.0.0
	github.com/gofiber/fiber/v2 v2.52.5
	github.com/gofiber/utils v1.2.0
	github.com/golang-jwt/jwt/v5 v5.2.2
	github.com/golang/protobuf v1.5.4
	github.com/hashicorp/go-version v1.7.0
	github.com/prashantv/gostub v1.1.0
	github.com/shirou/gopsutil v3.21.11+incompatible
	go.uber.org/automaxprocs v1.6.0
	google.golang.org/genproto/googleapis/api v0.0.0-20250218202821-56aae31c358a
	google.golang.org/genproto/googleapis/rpc v0.0.0-20250218202821-56aae31c358a
)

require (
	github.com/felixge/fgprof v0.9.5 // indirect
	github.com/gofiber/adaptor/v2 v2.1.31 // indirect
	github.com/google/pprof v0.0.0-20241210010833-40e02aabc2ad // indirect
	github.com/moby/docker-image-spec v1.3.1 // indirect
	go.opencensus.io v0.24.0 // indirect
)

require (
	buf.build/gen/go/bufbuild/bufplugin/protocolbuffers/go v1.36.0-20241031151143-70f632351282.1 // indirect
	buf.build/gen/go/bufbuild/protovalidate/protocolbuffers/go v1.36.0-20241127180247-a33202765966.1 // indirect
	buf.build/gen/go/bufbuild/registry/connectrpc/go v1.17.0-20241210175624-28487aef65cd.1 // indirect
	buf.build/gen/go/bufbuild/registry/protocolbuffers/go v1.36.0-20241210175624-28487aef65cd.1 // indirect
	buf.build/gen/go/pluginrpc/pluginrpc/protocolbuffers/go v1.36.0-20241007202033-cf42259fcbfc.1 // indirect
	buf.build/go/bufplugin v0.6.0 // indirect
	buf.build/go/protoyaml v0.3.1 // indirect
	buf.build/go/spdx v0.2.0 // indirect
	cel.dev/expr v0.19.1 // indirect
	cloud.google.com/go/auth v0.13.0 // indirect
	cloud.google.com/go/auth/oauth2adapt v0.2.6 // indirect
	cloud.google.com/go/compute/metadata v0.6.0 // indirect
	cloud.google.com/go/iam v1.2.2 // indirect
	cloud.google.com/go/monitoring v1.21.2 // indirect
	connectrpc.com/connect v1.17.0 // indirect
	connectrpc.com/otelconnect v0.7.1 // indirect
	github.com/Azure/go-ansiterm v0.0.0-20230124172434-306776ec8161 // indirect
	github.com/GoogleCloudPlatform/opentelemetry-operations-go/detectors/gcp v1.25.0 // indirect
	github.com/GoogleCloudPlatform/opentelemetry-operations-go/exporter/metric v0.48.1 // indirect
	github.com/GoogleCloudPlatform/opentelemetry-operations-go/internal/resourcemapping v0.48.1 // indirect
	github.com/HdrHistogram/hdrhistogram-go v1.1.2 // indirect
	github.com/Microsoft/hcsshim v0.12.9 // indirect
	github.com/agnivade/levenshtein v1.2.1 // indirect
	github.com/antlr4-go/antlr/v4 v4.13.1 // indirect
	github.com/apparentlymart/go-textseg/v15 v15.0.0 // indirect
	github.com/bufbuild/protocompile v0.14.1 // indirect
	github.com/bufbuild/protoplugin v0.0.0-20240911180120-7bb73e41a54a // indirect
	github.com/bufbuild/protovalidate-go v0.8.0 // indirect
	github.com/chzyer/readline v1.5.1 // indirect
	github.com/cncf/xds/go v0.0.0-20241223141626-cff3c89139a3 // indirect
	github.com/containerd/cgroups/v3 v3.0.5 // indirect
	github.com/containerd/containerd v1.7.27 // indirect
	github.com/containerd/continuity v0.4.5 // indirect
	github.com/containerd/errdefs v1.0.0 // indirect
	github.com/containerd/errdefs/pkg v0.3.0 // indirect
	github.com/containerd/log v0.1.0 // indirect
	github.com/containerd/platforms v0.2.1 // indirect
	github.com/containerd/stargz-snapshotter/estargz v0.16.3 // indirect
	github.com/containerd/ttrpc v1.2.7 // indirect
	github.com/containerd/typeurl/v2 v2.2.3 // indirect
	github.com/cpuguy83/go-md2man/v2 v2.0.6 // indirect
	github.com/distribution/reference v0.6.0 // indirect
	github.com/docker/cli v27.4.1+incompatible // indirect
	github.com/docker/distribution v2.8.3+incompatible // indirect
	github.com/docker/docker-credential-helpers v0.8.2 // indirect
	github.com/envoyproxy/go-control-plane/envoy v1.32.4 // indirect
	github.com/go-chi/chi/v5 v5.2.2 // indirect
	github.com/go-ini/ini v1.67.0 // indirect
	github.com/go-logr/logr v1.4.2 // indirect
	github.com/go-logr/stdr v1.2.2 // indirect
	github.com/go-task/slim-sprig/v3 v3.0.0 // indirect
	github.com/go-viper/mapstructure/v2 v2.3.0 // indirect
	github.com/goccy/go-json v0.10.3 // indirect
	github.com/gofrs/flock v0.12.1 // indirect
	github.com/google/cel-go v0.22.1 // indirect
	github.com/google/go-containerregistry v0.20.2 // indirect
	github.com/google/go-tpm v0.9.3 // indirect
	github.com/google/s2a-go v0.1.8 // indirect
	github.com/googleapis/enterprise-certificate-proxy v0.3.4 // indirect
	github.com/hashicorp/go-msgpack v1.1.5 // indirect
	github.com/hashicorp/go-msgpack/v2 v2.1.1 // indirect
	github.com/jdx/go-netrc v1.0.0 // indirect
	github.com/klauspost/cpuid/v2 v2.2.8 // indirect
	github.com/lyft/protoc-gen-star/v2 v2.0.4-0.20230330145011-496ad1ac90a4 // indirect
	github.com/mattermost/xml-roundtrip-validator v0.0.0-20201204154048-1a8688af4cf1 // indirect
	github.com/minio/highwayhash v1.0.3 // indirect
	github.com/moby/locker v1.0.1 // indirect
	github.com/moby/patternmatcher v0.6.0 // indirect
	github.com/moby/sys/mount v0.3.4 // indirect
	github.com/moby/sys/mountinfo v0.7.2 // indirect
	github.com/moby/sys/sequential v0.6.0 // indirect
	github.com/moby/sys/user v0.3.0 // indirect
	github.com/moby/sys/userns v0.1.0 // indirect
	github.com/moby/term v0.5.0 // indirect
	github.com/morikuni/aec v1.0.0 // indirect
	github.com/munnerz/goautoneg v0.0.0-20191010083416-a7dc8b61c822 // indirect
	github.com/nats-io/jwt/v2 v2.7.3 // indirect
	github.com/oklog/ulid v1.3.1 // indirect
	github.com/onsi/ginkgo/v2 v2.22.0 // indirect
	github.com/opencontainers/go-digest v1.0.0 // indirect
	github.com/opencontainers/image-spec v1.1.1 // indirect
	github.com/opencontainers/runtime-spec v1.2.0 // indirect
	github.com/pelletier/go-toml/v2 v2.2.3 // indirect
	github.com/pkg/browser v0.0.0-20240102092130-5ac0b6a4141c // indirect
	github.com/planetscale/vtprotobuf v0.6.1-0.20240319094008-0393e58bdf10 // indirect
	github.com/quic-go/qpack v0.5.1 // indirect
	github.com/quic-go/quic-go v0.48.2 // indirect
	github.com/rogpeppe/go-internal v1.13.1 // indirect
	github.com/rs/cors v1.11.1 // indirect
	github.com/russross/blackfriday/v2 v2.1.0 // indirect
	github.com/sagikazarmark/locafero v0.7.0 // indirect
	github.com/segmentio/asm v1.2.0 // indirect
	github.com/segmentio/encoding v0.4.1 // indirect
	github.com/sourcegraph/conc v0.3.0 // indirect
	github.com/stoewer/go-strcase v1.3.0 // indirect
	github.com/tchap/go-patricia/v2 v2.3.2 // indirect
	github.com/tetratelabs/wazero v1.8.2 // indirect
	github.com/vbatts/tar-split v0.11.6 // indirect
	github.com/xeipuuv/gojsonpointer v0.0.0-20190905194746-02993c407bfb // indirect
	github.com/xeipuuv/gojsonreference v0.0.0-20180127040603-bd5ef7bd5415 // indirect
	go.lsp.dev/jsonrpc2 v0.10.0 // indirect
	go.lsp.dev/pkg v0.0.0-20210717090340-384b27a52fb2 // indirect
	go.lsp.dev/protocol v0.12.0 // indirect
	go.lsp.dev/uri v0.3.0 // indirect
	go.mongodb.org/mongo-driver v1.16.1 // indirect
	go.opentelemetry.io/auto/sdk v1.1.0 // indirect
	go.opentelemetry.io/contrib/detectors/gcp v1.34.0 // indirect
	go.opentelemetry.io/contrib/instrumentation/google.golang.org/grpc/otelgrpc v0.54.0 // indirect
	go.opentelemetry.io/contrib/instrumentation/net/http/otelhttp v0.60.0 // indirect
	go.opentelemetry.io/otel v1.35.0 // indirect
	go.opentelemetry.io/otel/metric v1.35.0 // indirect
	go.opentelemetry.io/otel/sdk v1.35.0 // indirect
	go.opentelemetry.io/otel/sdk/metric v1.35.0 // indirect
	go.opentelemetry.io/otel/trace v1.35.0 // indirect
	go.uber.org/mock v0.5.0 // indirect
	go.uber.org/zap/exp v0.3.0 // indirect
	golang.org/x/exp v0.0.0-20241217172543-b2144cdd0a67 // indirect
	golang.org/x/sync v0.12.0 // indirect
	golang.org/x/time v0.11.0 // indirect
	pluginrpc.com/pluginrpc v0.5.0 // indirect
	sigs.k8s.io/yaml v1.4.0 // indirect
)

require (
	cloud.google.com/go v0.116.0 // indirect
	cloud.google.com/go/storage v1.49.0
	github.com/Azure/go-autorest v14.2.0+incompatible // indirect
	github.com/Azure/go-autorest/autorest/azure/cli v0.4.6 // indirect
	github.com/Azure/go-autorest/autorest/date v0.3.0 // indirect
	github.com/Azure/go-autorest/autorest/to v0.4.0 // indirect
	github.com/Azure/go-autorest/autorest/validation v0.3.1 // indirect
	github.com/Azure/go-autorest/logger v0.2.1 // indirect
	github.com/Azure/go-autorest/tracing v0.6.0 // indirect
	github.com/Masterminds/semver/v3 v3.3.0
	github.com/Microsoft/go-winio v0.6.2 // indirect
	github.com/agext/levenshtein v1.2.3 // indirect
	github.com/andybalholm/brotli v1.1.0 // indirect
	github.com/asaskevich/govalidator v0.0.0-20230301143203-a9d515a09cc2 // indirect
	github.com/beevik/etree v1.1.0 // indirect
	github.com/beorn7/perks v1.0.1 // indirect
	github.com/cespare/xxhash/v2 v2.3.0 // indirect
	github.com/cosiner/argv v0.0.0-20170225145430-13bacc38a0a5 // indirect
	github.com/davecgh/go-spew v1.1.2-0.20180830191138-d8f796af33cc // indirect
	github.com/dimchansky/utfbom v1.1.1 // indirect
	github.com/docker/docker v27.4.1+incompatible // indirect
	github.com/docker/go-connections v0.5.0 // indirect
	github.com/docker/go-units v0.5.0 // indirect
	github.com/dustin/go-humanize v1.0.1 // indirect
	github.com/felixge/httpsnoop v1.0.4 // indirect
	github.com/fsnotify/fsnotify v1.8.0 // indirect
	github.com/go-ole/go-ole v1.3.0 // indirect
	github.com/go-openapi/errors v0.22.0 // indirect
	github.com/go-openapi/strfmt v0.23.0 // indirect
	github.com/gobwas/glob v0.2.3 // indirect
	github.com/gogo/protobuf v1.3.2 // indirect
	github.com/golang-jwt/jwt/v4 v4.5.2 // indirect
	github.com/golang/glog v1.2.4 // indirect
	github.com/golang/groupcache v0.0.0-20241129210726-2c02b8208cf8 // indirect
	github.com/google/wire v0.5.0 // indirect
	github.com/googleapis/gax-go/v2 v2.14.1 // indirect
	github.com/gorilla/handlers v1.4.2 // indirect
	github.com/hashicorp/go-hclog v1.6.3 // indirect
	github.com/hashicorp/go-immutable-radix v1.3.1 // indirect
	github.com/hashicorp/golang-lru v1.0.2 // indirect
	github.com/hashicorp/raft v1.6.0 // indirect
	github.com/iancoleman/strcase v0.3.0 // indirect
	github.com/inconshreveable/mousetrap v1.1.0 // indirect
	github.com/jmespath/go-jmespath v0.4.0 // indirect
	github.com/jonboulle/clockwork v0.2.2 // indirect
	github.com/josharian/intern v1.0.0 // indirect
	github.com/klauspost/compress v1.18.0 // indirect
	github.com/klauspost/pgzip v1.2.6 // indirect
	github.com/kr/pretty v0.3.1 // indirect
	github.com/kr/text v0.2.0 // indirect
	github.com/lann/builder v0.0.0-20180802200727-47ae307949d0 // indirect
	github.com/lann/ps v0.0.0-20150810152359-62de8c46ede0 // indirect
	github.com/mailru/easyjson v0.7.7 // indirect
	github.com/manifoldco/promptui v0.9.0
	github.com/mattn/go-colorable v0.1.13 // indirect
	github.com/mattn/go-isatty v0.0.20 // indirect
	github.com/mattn/go-runewidth v0.0.16 // indirect
	github.com/minio/md5-simd v1.1.2 // indirect
	github.com/mitchellh/go-wordwrap v1.0.1 // indirect
	github.com/nats-io/nats-server/v2 v2.11.1
	github.com/nats-io/nkeys v0.4.10 // indirect
	github.com/nats-io/nuid v1.0.1 // indirect
	github.com/peterh/liner v1.2.2 // indirect
	github.com/pkg/profile v1.7.0 // indirect
	github.com/prometheus/client_model v0.6.1 // indirect
	github.com/prometheus/common v0.62.0 // indirect
	github.com/prometheus/procfs v0.15.1 // indirect
	github.com/rivo/uniseg v0.4.7 // indirect
	github.com/rs/xid v1.6.0 // indirect
	github.com/segmentio/backo-go v1.1.0 // indirect
	github.com/spf13/afero v1.12.0 // indirect
	github.com/spf13/cast v1.7.1 // indirect
	github.com/stretchr/objx v0.5.2 // indirect
	github.com/subosito/gotenv v1.6.0 // indirect
	github.com/tklauser/go-sysconf v0.3.14 // indirect
	github.com/tklauser/numcpus v0.8.0 // indirect
	github.com/uber/jaeger-lib v2.4.1+incompatible // indirect
	github.com/valyala/bytebufferpool v1.0.0 // indirect
	github.com/valyala/fasthttp v1.54.0 // indirect
	github.com/valyala/tcplisten v1.0.0 // indirect
	github.com/xrash/smetrics v0.0.0-20240521201337-686a1a2994c1 // indirect
	github.com/xtgo/uuid v0.0.0-20140804021211-a0b114877d4c // indirect
	github.com/yashtewari/glob-intersection v0.2.0 // indirect
	github.com/yusufpapurcu/wmi v1.2.4 // indirect
	github.com/ziutek/mymysql v1.5.4 // indirect
	go.etcd.io/bbolt v1.3.11 // indirect
	// go.opencensus.io v0.24.0 // indirect
	go.starlark.net v0.0.0-20190702223751-32f345186213 // indirect
	golang.org/x/arch v0.0.0-20171004143515-077ac972c2e4 // indirect
	golang.org/x/mod v0.22.0 // indirect
	golang.org/x/net v0.38.0 // indirect
	golang.org/x/sys v0.31.0 // indirect
	golang.org/x/term v0.30.0 // indirect
	golang.org/x/tools v0.28.0 // indirect
	golang.org/x/xerrors v0.0.0-20240716161551-93cc26a95ae9 // indirect
	gopkg.in/asn1-ber.v1 v1.0.0-20181015200546-f715ec2f112d // indirect
	gopkg.in/fsnotify.v1 v1.4.7 // indirect
	gopkg.in/ini.v1 v1.67.0
	gopkg.in/ldap.v2 v2.5.1 // indirect
	gopkg.in/tomb.v1 v1.0.0-20141024135613-dd632973f1e7 // indirect
	gopkg.in/yaml.v3 v3.0.1
)

// https://github.com/dexidp/dex/issues/1578
replace github.com/dexidp/dex => github.com/dexidp/dex v0.0.0-20201214084049-0f9e2888ab65

replace github.com/chef/automate/api/external => ./api/external/

replace github.com/nats-io/jwt => github.com/nats-io/jwt/v2 v2.5.0

//replace go.uber.org/automaxprocs => ./automaxprocs-excluded
