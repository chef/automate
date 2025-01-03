module github.com/chef/automate/api/external

go 1.22

replace google.golang.org/genproto => google.golang.org/genproto v0.0.0-20200513103714-09dca8ec2884

require (
	github.com/golang/mock v1.1.1
	github.com/grpc-ecosystem/grpc-gateway v1.14.6
	github.com/lib/pq v1.10.9
	github.com/olivere/elastic/v7 v7.0.28
	github.com/pkg/errors v0.9.1
	github.com/sirupsen/logrus v1.9.3
	github.com/stretchr/testify v1.9.0
	google.golang.org/genproto v0.0.0-20200526211855-cb27e3aa2013
	google.golang.org/grpc v1.33.2
	google.golang.org/protobuf v1.28.0
)

require (
	github.com/davecgh/go-spew v1.1.1 // indirect
	github.com/josharian/intern v1.0.0 // indirect
	github.com/mailru/easyjson v0.7.7 // indirect
	github.com/pmezard/go-difflib v1.0.0 // indirect
	golang.org/x/net v0.0.0-20210614182718-04defd469f4e // indirect
	golang.org/x/sys v0.0.0-20220715151400-c0bba94af5f8 // indirect
	golang.org/x/text v0.3.6 // indirect
	gopkg.in/yaml.v3 v3.0.1 // indirect
)

replace google.golang.org/protobuf => google.golang.org/protobuf v1.28.0
