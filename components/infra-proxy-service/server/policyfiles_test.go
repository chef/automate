package server

import (
	gwres "github.com/chef/automate/api/external/infra_proxy/response"
	"github.com/chef/automate/components/automate-gateway/handler/infra_proxy"
	"github.com/go-chef/chef"
	"reflect"
	"testing"
)

func TestFromAPIIncludedSolutionDependencies(t *testing.T) {
	type args struct {
		sp chef.SolutionDep
	}
	tests := []struct {
		name string
		args args
		want []*gwres.SolutionDependencies
	}{
		{"Should transform a solution dependency with non-similar version",
			args{
				sp: chef.SolutionDep{
					PolicyFile: [][]string{
						[]string{"viv-test", ">= 0.0.0"},
					},
					Dependencies: map[string]interface{}{
						"viv-test (0.1.0)": []interface{}{
							[]interface{}{"ohai", "~> 5.2"},
						},
					},
				},
			},
			[]*gwres.SolutionDependencies{
				&gwres.SolutionDependencies{
					Name:    "viv-test",
					Version: ">= 0.0.0",
					Dependencies: []*gwres.DepedenciesData{
						&gwres.DepedenciesData{
							Name:    "ohai",
							Version: "~> 5.2",
						},
					},
				},
			},
		},
		{"Should transform a solution dependency with multiple level version",
			args{
				sp: chef.SolutionDep{
					PolicyFile: [][]string{
						[]string{"build-essential", "= 8.2.1"},
						[]string{"chocolatey", "= 3.0.0"},
						[]string{"homebrew", "= 5.2.1"},
						[]string{"mingw", "= 2.1.1"},
						[]string{"mycookbook", "= 0.1.0"},
						[]string{"pantry", "= 1.0.0"},
						[]string{"seven_zip", "= 4.2.1"},
					},
					Dependencies: map[string]interface{}{
						"build-essential (8.2.1)": []interface{}{
							[]interface{}{"seven_zip", ">= 0.0.0"},
							[]interface{}{"mingw", ">= 1.1.0"},
						},
						"chocolatey (3.0.0)": []interface{}{},
						"homebrew (5.2.1)":   []interface{}{},
						"mingw (2.1.1)": []interface{}{
							[]interface{}{"seven_zip", ">= 0.0.0"},
						},
						"mycookbook (5.2.1)": []interface{}{},
						"pantry (1.0.0)": []interface{}{
							[]interface{}{"homebrew", ">= 0.0.0"},
							[]interface{}{"build-essential", ">= 0.0.0"},
							[]interface{}{"chocolatey", ">= 0.0.0"},
						},
						"seven_zip (4.2.1)": []interface{}{},
					},
				},
			},
			[]*gwres.SolutionDependencies{
				&gwres.SolutionDependencies{
					Name:    "build-essential",
					Version: "= 8.2.1",
					Dependencies: []*gwres.DepedenciesData{
						&gwres.DepedenciesData{
							Name:    "seven_zip",
							Version: ">= 0.0.0",
						},
						&gwres.DepedenciesData{
							Name:    "mingw",
							Version: ">= 1.1.0",
						},
					},
				},
				&gwres.SolutionDependencies{
					Name:    "chocolatey",
					Version: "= 3.0.0",
					Dependencies: []*gwres.DepedenciesData{},
				},
				&gwres.SolutionDependencies{
					Name:    "homebrew",
					Version: "= 5.2.1",
					Dependencies: []*gwres.DepedenciesData{},
				},
				&gwres.SolutionDependencies{
					Name:    "mingw",
					Version: "= 2.1.1",
					Dependencies: []*gwres.DepedenciesData{
						&gwres.DepedenciesData{
							Name:    "seven_zip",
							Version: ">= 0.0.0",
						},
					},
				},
				&gwres.SolutionDependencies{
					Name:    "mycookbook",
					Version: "= 0.1.0",
					Dependencies: []*gwres.DepedenciesData{},
				},
				&gwres.SolutionDependencies{
					Name:    "pantry",
					Version: "= 1.0.0",
					Dependencies: []*gwres.DepedenciesData{
						&gwres.DepedenciesData{
							Name:    "homebrew",
							Version: ">= 0.0.0",
						},
						&gwres.DepedenciesData{
							Name:    "build-essential",
							Version: ">= 0.0.0",
						},
						&gwres.DepedenciesData{
							Name:    "chocolatey",
							Version: ">= 0.0.0",
						},
					},
				},
				&gwres.SolutionDependencies{
					Name:    "seven_zip",
					Version: "= 4.2.1",
					Dependencies: []*gwres.DepedenciesData{},
				},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			iproxy_data := FromAPIIncludedSolutionDependencies(tt.args.sp)
			if got := infra_proxy.FromUpstreamIncludeSolutionDependecies(iproxy_data); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("FromAPIIncludedSolutionDependencies() = %+v, want %+v", got, tt.want)
			}
		})
	}
}
