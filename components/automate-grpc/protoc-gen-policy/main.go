package main

import (
	pgs "github.com/lyft/protoc-gen-star"
	pgsgo "github.com/lyft/protoc-gen-star/lang/go"

	"github.com/chef/automate/components/automate-grpc/protoc-gen-policy/policy"
)

func main() {
	pgs.Init(pgs.DebugEnv("DEBUG")).
		RegisterModule(policy.Policy()).
		RegisterPostProcessor(pgsgo.GoFmt()).
		Render()
}
