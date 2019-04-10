package main

import (
	pgs "github.com/lyft/protoc-gen-star"
	pgsgo "github.com/lyft/protoc-gen-star/lang/go"

	"github.com/chef/automate/components/automate-grpc/protoc-gen-a2-config/gen"
)

func main() {
	pgs.Init(pgs.DebugEnv("DEBUG")).
		RegisterModule(gen.NewA2ServiceConfigModule()).
		RegisterModule(gen.NewA2RootConfigModule()).
		RegisterPostProcessor(pgsgo.GoFmt()).
		Render()
}
