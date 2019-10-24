package preflight

import (
	"github.com/pkg/errors"

	"github.com/chef/automate/api/config/deployment"
	dc "github.com/chef/automate/api/config/deployment"
)

type Check struct {
	Name        string
	Description string
	TestFunc    func(TestProbe) error
}

var noopCheck = Check{
	Name: "noop",
}

type DeployPreflightCheckOptions struct {
	SkipSharedPortCheck bool
	SkipA2DeployedCheck bool
	Airgap              bool
	AutomateConfig      *dc.AutomateConfig
}

type DeployPreflightChecker struct {
	skipSharedPorts     bool
	skipA2DeployedCheck bool
	airgap              bool
	automateConfig      *deployment.AutomateConfig
}

func NewDeployPreflightChecker(opts DeployPreflightCheckOptions) *DeployPreflightChecker {
	return &DeployPreflightChecker{
		skipSharedPorts:     opts.SkipSharedPortCheck,
		skipA2DeployedCheck: opts.SkipA2DeployedCheck,
		airgap:              opts.Airgap,
		automateConfig:      opts.AutomateConfig,
	}
}

func (c *DeployPreflightChecker) Run(probe TestProbe) error {
	if c.automateConfig == nil {
		c.automateConfig = dc.DefaultAutomateConfig()
	}

	portCheck, err := DefaultPortCheck(c.skipSharedPorts, c.automateConfig)
	if err != nil {
		return errors.Wrap(err, "failed to configure preflight checks")
	}
	isA2DeployedCheck := IsA2DeployedCheck(&portCheck)
	chefAutomateInBinCheck := CLIInBin()
	connectivityCheck := DefaultConnectivityCheck()

	if c.skipA2DeployedCheck {
		isA2DeployedCheck = noopCheck
		chefAutomateInBinCheck = noopCheck
	}

	if c.airgap {
		connectivityCheck = noopCheck
	}

	checks := []Check{
		RootUserRequiredCheck(),
		DefaultMinimumDiskCheck(c.automateConfig),
		chefAutomateInBinCheck,
		isA2DeployedCheck,
		IsSystemdCheck(),
		HasUseraddCheck(),
		HasNobodyCheck(),
		DefaultMinimumRAMCheck(),
		DefaultSysctlCheck(),
		DefaultKernelVersionCheck(),
		connectivityCheck,
	}

	for _, c := range checks {
		if c.TestFunc == nil {
			continue
		}
		if err := c.TestFunc(probe); err != nil {
			return errors.Wrap(err, "failed to run preflight checks")
		}
	}

	return nil
}

// RunDeployPreflightCheck runs the Deploy preflight checks
func RunDeployPreflightCheck(opts DeployPreflightCheckOptions) (string, error) {
	reporter := NewInMemoryPrintReporter()
	probe := NewTestProbe(reporter)
	checker := NewDeployPreflightChecker(opts)
	err := checker.Run(probe)
	if err != nil {
		return "", err
	}

	out := reporter.Out()
	if reporter.HasFailures() {
		return "", errors.New(out)
	}

	return out, nil
}
