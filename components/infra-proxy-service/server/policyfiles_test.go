package server

import (
	"github.com/chef/automate/api/interservice/infra_proxy/response"
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
		want []*response.SolutionDependencies
	}{
		{"Should transform a solution dependency with similar version",
			args{
				sp: chef.SolutionDep{
					PolicyFile: [][]string{
						[]string{"nginx", "= 11.5.3"},
					},
					Dependencies: map[string][][]string{
						"nginx (11.5.3)": [][]string{
							[]string{"ohai", "~> 5.2"},
						},
					},
				},
			},
			[]*response.SolutionDependencies{
				&response.SolutionDependencies{
					Name:    "nginx",
					Version: "11.5.3",
					Dependencies: []*response.DepedenciesData{
						&response.DepedenciesData{
							Name:    "ohai",
							Version: "~> 5.2",
						},
					},
				},
			},
		},
		{"Should transform a solution dependency with non-similar version",
			args{
				sp: chef.SolutionDep{
					PolicyFile: [][]string{
						[]string{"viv-test", ">= 0.0.0"},
					},
					Dependencies: map[string]interface{}{
						"viv-test (0.1.0)": []interface{}{
							[]string{"ohai", "~> 5.2"},
						},
					},
				},
			},
			[]*response.SolutionDependencies{
				&response.SolutionDependencies{
					Name:    "nginx",
					Version: "11.5.3",
					Dependencies: []*response.DepedenciesData{
						&response.DepedenciesData{
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
							[]string{"seven_zip", ">= 0.0.0"},
							[]string{"mingw", ">= 1.1.0"},
						},
						"chocolatey (3.0.0)": []interface{}{},
						"homebrew (5.2.1)":   []interface{}{},
						"mingw (2.1.1)": []interface{}{
							[]string{"seven_zip", ">= 0.0.0"},
						},
						"mycookbook (5.2.1)": []interface{}{},
						"pantry (1.0.0)": []interface{}{
							[]string{"homebrew", ">= 0.0.0"},
							[]string{"build-essential", ">= 0.0.0"},
							[]string{"chocolatey", ">= 0.0.0"},
						},
						"seven_zip (4.2.1)": []interface{}{},
					},
				},
			},
			[]*response.SolutionDependencies{
				&response.SolutionDependencies{
					Name:    "build-essential",
					Version: "= 8.2.1",
					Dependencies: []*response.DepedenciesData{
						&response.DepedenciesData{
							Name:    "seven_zip",
							Version: ">= 0.0.0",
						},
						&response.DepedenciesData{
							Name:    "seven_zip",
							Version: ">= 1.1.0",
						},
					},
				},
				&response.SolutionDependencies{
					Name:         "chocolatey",
					Version:      "= 8.2.1",
					Dependencies: []*response.DepedenciesData{},
				},
				&response.SolutionDependencies{
					Name:         "homebrew",
					Version:      "= 5.2.1",
					Dependencies: []*response.DepedenciesData{},
				},
				&response.SolutionDependencies{
					Name:    "mingw",
					Version: "= 0.1.0",
					Dependencies: []*response.DepedenciesData{
						&response.DepedenciesData{
							Name:    "mingw",
							Version: ">= 0.0.0",
						},
					},
				},
				&response.SolutionDependencies{
					Name:         "mycookbook",
					Version:      "= 2.1.1",
					Dependencies: []*response.DepedenciesData{},
				},
				&response.SolutionDependencies{
					Name:    "pantry",
					Version: "= 1.0.0",
					Dependencies: []*response.DepedenciesData{
						&response.DepedenciesData{
							Name:    "homebrew",
							Version: ">= 0.0.0",
						},
						&response.DepedenciesData{
							Name:    "build-essential",
							Version: ">= 0.0.0",
						},
						&response.DepedenciesData{
							Name:    "chocolatey",
							Version: ">= 0.0.0",
						},
					},
				},
				&response.SolutionDependencies{
					Name:         "seven_zip",
					Version:      "= 4.2.1",
					Dependencies: []*response.DepedenciesData{},
				},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := FromAPIIncludedSolutionDependencies(tt.args.sp); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("FromAPIIncludedSolutionDependencies() = %v, want %v", got, tt.want)
			}
		})
	}
}
