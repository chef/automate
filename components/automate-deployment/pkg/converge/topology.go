package converge

import "github.com/chef/automate/components/automate-deployment/pkg/target"

// Topology represents our targets and the services that run on them
type Topology map[target.Target][]Service

// Service is a representation of a package and the state we
// want it in
type Service struct {
	Name          string
	ConvergeState ServiceConvergeState
}
