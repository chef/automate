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
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := FromAPIIncludedSolutionDependencies(tt.args.sp); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("FromAPIIncludedSolutionDependencies() = %v, want %v", got, tt.want)
			}
		})
	}
}
