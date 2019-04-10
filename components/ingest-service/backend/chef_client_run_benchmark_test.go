package backend_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"testing"

	proto "github.com/chef/automate/api/external/ingest/request"
	"github.com/chef/automate/components/automate-gateway/gateway"
	"github.com/chef/automate/components/ingest-service/backend"
	"github.com/stretchr/testify/require"
)

var (
	rawruns map[string][]byte
)

func init() {
	// load chef_run's for reuse
	rawruns = make(map[string][]byte)
	runs := []string{
		"chef_client_run",
		"converge-bad-report",
		"converge-failure-report",
		"converge-success-report",
		// TODO Add more examples!
	}
	for _, r := range runs {
		// load chef_run json into memory, so that we do not count the json
		// generation
		content, err := ioutil.ReadFile(fmt.Sprintf("../examples/%s.json", r))
		if err != nil {
			panic(err)
		}
		rawruns[r] = content
	}
}

var (
	pRunRes *proto.Run
	bRunRes *backend.ChefClientRun
)

func BenchmarkProtoParseBytesToChefRun(b *testing.B) {
	var (
		pRun *proto.Run
		bRun *backend.ChefClientRun
		err  error
	)

	for r, body := range rawruns {
		// Convert the body '[]byte' to a proto.Run
		//
		// @afiune This is either very Slick or very Dump
		// because we are using the function that the gateway
		// uses to transform the body of the incoming requests
		pRun = gateway.ParseBytesToChefRun(body)
		if err != nil {
			b.Fatalf("could not parse body to a proto.Run: %v", err)
		}

		b.Run("ChefClientRun builder "+r, func(b *testing.B) {
			for n := 0; n < b.N; n++ {
				// TODO @afiune Bench mark the new manual conversion! (AIA-457)
				//bRun = backend.ParseBytesToChefRun(*pRun)
				if err != nil {
					b.Errorf("Error building ChefClientRun object: %v", err)
				}
			}
		})
	}

	pRunRes = pRun
	bRunRes = bRun
}

// For a good introduction into golang benchmark, see
// https://dave.cheney.net/2013/06/30/how-to-write-benchmarks-in-go

// BenchFlatten benchmarks the previous incarnation of the flatten function,
// which was recursive, against the recent one, which is not.
//
// It does that using three different data sets: the test data's chef run
// object's default, normal, and override attributes.
func BenchmarkFlatten(b *testing.B) {
	implementations := map[string]func(map[string]interface{}) map[string]interface{}{
		"recursive": recursiveFlatten,
		"iterative": backend.Flatten,
	}

	exampleCCR, err := ioutil.ReadFile("../examples/chef_client_run.json")
	require.NoError(b, err)
	data := backend.ChefClientRun{}
	err = json.NewDecoder(bytes.NewReader(exampleCCR)).Decode(&data)
	require.NoError(b, err)

	inputs := map[string]map[string]interface{}{
		"automatic": data.NodePayload.Automatic,
		"normal":    data.NodePayload.Normal,
		"override":  data.NodePayload.Override,
	}

	for inputName, input := range inputs {
		b.Run(inputName, func(b *testing.B) {
			for name, fun := range implementations {
				b.Run(name, func(b *testing.B) {
					var r map[string]interface{} // our result
					for n := 0; n < b.N; n++ {
						// always record the result to prevent the compiler eliminating the function
						// call.
						r = fun(input)
					}
					// always store the result to a package level variable
					// so the compiler cannot eliminate the Benchmark itself.
					result = r
				})
			}
		})
	}
}

// Keep this a global so the calculation isn't optimized away by the compiler
var result map[string]interface{}
