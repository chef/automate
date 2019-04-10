package main

import (
	"github.com/chef/automate/components/automate-grpc/protoc-gen-grpc-mock/mock"
	pgs "github.com/lyft/protoc-gen-star"
	pgsgo "github.com/lyft/protoc-gen-star/lang/go"
)

func main() {
	pgs.Init(pgs.DebugEnv("DEBUG")).
		RegisterModule(mock.MockServer()).
		RegisterPostProcessor(pgsgo.GoFmt()).
		Render()
}
