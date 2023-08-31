package genconfig

import (
	"fmt"

	"github.com/chef/automate/lib/pmt"
)

type TopologyType string

const (
	TOPOLOGY_TYPE_STANDALONE TopologyType = "Standalone Chef Automate"
	TOPOLOGY_TYPE_HA         TopologyType = "Chef Automate HA"
)

type DeployType string

const (
	DEPLOY_TYPE_ON_PREMISE DeployType = "On-Premise"
	DEPLOY_TYPE_AWS        DeployType = "AWS"
)

const (
	LOCATION_TYPE_ON_GCP DeployType = "gcs"
	LOCATION_TYPE_ON_AWS DeployType = "s3"
)

type ConfigType string

const (
	CONFIG_TYPE_DEPLOY   ConfigType = "Deployment"
	CONFIG_TYPE_AUTOMATE ConfigType = "Automate"
	CONFIG_TYPE_INFRA    ConfigType = "Chef Infra Server"
	CONFIG_TYPE_PG       ConfigType = "PostgreSQL"
	CONFIG_TYPE_OS       ConfigType = "OpenSearch"
)

type GenConfig interface {
	GenConfigWithPrompts() (err error)
	Toml() (tomlBytes []byte, err error)
	TomlSave(filepath string, err error)
}

type GenConfigImp struct {
	Prompt pmt.Prompt
	Config IConfig
}

func GenConfigImpFactory(p pmt.Prompt) *GenConfigImp {
	return &GenConfigImp{
		Prompt: p,
	}
}

func (g *GenConfigImp) GenConfigWithPrompts() (err error) {
	fmt.Println("### Welcome to Config Generation Tool ###")
	fmt.Println("We will ask you various info based on your choices, like:")
	fmt.Println("\ta. Topology Details")
	fmt.Println("\tb. SSH Details")
	fmt.Println("\tc. Infrastructre Details")
	fmt.Println("\td. Databases Details")
	fmt.Println("\te. Certificates")
	fmt.Println("\tf. Backup Details")
	fmt.Println("\nBegin the flow below:")
	return g.TopologyFlow()
}

func (g *GenConfigImp) Toml() (tomlBytes []byte, err error) {
	return g.Config.Toml()
}

func (g *GenConfigImp) TopologyFlow() (err error) {
	_, topologyType, err := g.Prompt.Select("Choose Topology", string(TOPOLOGY_TYPE_HA)) //, string(TOPOLOGY_TYPE_STANDALONE))
	if err != nil {
		return
	}
	switch topologyType {
	case string(TOPOLOGY_TYPE_HA):
		return g.HaFlow()
	case string(TOPOLOGY_TYPE_STANDALONE):
		return g.StandaloneFlow()
	}
	return
}

func (g *GenConfigImp) StandaloneFlow() (err error) {
	g.Config = AutomateStandaloneConfigFactory()
	return
}

func (g *GenConfigImp) HaFlow() (err error) {
	_, deployType, err := g.Prompt.Select("Choose type of Deployment Type", string(DEPLOY_TYPE_ON_PREMISE), string(DEPLOY_TYPE_AWS))
	if err != nil {
		return
	}
	switch deployType {
	case string(DEPLOY_TYPE_ON_PREMISE):
		return g.OnPremFlow()
	case string(DEPLOY_TYPE_AWS):
		return g.AwsHaProvisionFlow()
	}
	return
}

func (g *GenConfigImp) OnPremFlow() (err error) {
	_, configType, err := g.Prompt.Select("Choose config type",
		string(CONFIG_TYPE_DEPLOY))
	//, string(CONFIG_TYPE_AUTOMATE), string(CONFIG_TYPE_INFRA), string(CONFIG_TYPE_PG), string(CONFIG_TYPE_OS))
	if err != nil {
		return
	}
	switch configType {
	case string(CONFIG_TYPE_DEPLOY):
		return g.HaDeployFlow()
	case string(CONFIG_TYPE_AUTOMATE):
		return g.HaAutomateFlow()
	case string(CONFIG_TYPE_INFRA):
		return g.HaInfraFlow()
	case string(CONFIG_TYPE_PG):
		return g.HaPgFlow()
	case string(CONFIG_TYPE_OS):
		return g.HaOsFlow()
	}
	return
}

func (g *GenConfigImp) HaDeployFlow() (err error) {
	g.Config = HaDeployConfigFactory(g.Prompt)
	return g.Config.Prompts()
}

func (g *GenConfigImp) HaAutomateFlow() (err error) {
	g.Config = AutomateHaConfigFactory()
	return g.Config.Prompts()
}

func (g *GenConfigImp) HaInfraFlow() (err error) {
	g.Config = InfraHaConfigFactory()
	return g.Config.Prompts()
}

func (g *GenConfigImp) HaPgFlow() (err error) {
	g.Config = PgHaConfigFactory()
	return g.Config.Prompts()
}

func (g *GenConfigImp) HaOsFlow() (err error) {
	g.Config = OsHaConfigFactory()
	return g.Config.Prompts()
}

func (g *GenConfigImp) AwsHaProvisionFlow() (err error) {
	g.Config = AwsHaProvisionConfigFactory(g.Prompt)
	return g.Config.Prompts()
}
