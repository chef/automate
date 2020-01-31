package generator

import (
	"github.com/BurntSushi/toml"
	"github.com/pkg/errors"
)

// builtinConfigFullToml is a load generation profile that emulates a habitat
// setup comprising multiple services deployed to multiple environments
const builtinConfigFullToml = `
[templates]
  [[templates.supervisor]]
    name = "pos_terminal"
    [[ templates.supervisor.service ]]
      package = "noms/pos-service-1"
      application = "pos_terminal"
    [[ templates.supervisor.service ]]
      package = "noms/pos-service-2"
      application = "pos_terminal"
    [[ templates.supervisor.service ]]
      package = "noms/pos-service-3"
      application = "pos_terminal"

  [[templates.supervisor]]
    name = "order_mgmt"
    [[ templates.supervisor.service ]]
      package = "noms/om-service-1"
      application = "order_mgmt"
    [[ templates.supervisor.service ]]
      package = "noms/om-service-2"
      application = "order_mgmt"
    [[ templates.supervisor.service ]]
      package = "noms/om-service-3"
      application = "order_mgmt"

[generator]
  [[generator.supervisor]]
    name = "pos_terminals:production1"
    template = "pos_terminal"
    count = 4
    channel = "production1"
    environment = "production1"
    site = "site1"

  [[generator.supervisor]]
    name = "order_mgmt:production1"
    template = "order_mgmt"
    count = 4
    channel = "production1"
    environment = "production1"
    site = "site1"

  [[generator.supervisor]]
    name = "pos_terminals:test1"
    template = "pos_terminal"
    count = 1
    channel = "test1"
    environment = "test1"
    site = "test1"

  [[generator.supervisor]]
    name = "order_mgmt:test1"
    template = "order_mgmt"
    count = 1
    channel = "test1"
    environment = "test1"
    site = "test1"
`

func BuiltinConfigFull() (*LoadProfileCfg, error) {
	var profileCfg LoadProfileCfg
	_, err := toml.Decode(builtinConfigFullToml, &profileCfg)
	if err != nil {
		return nil, errors.Wrap(err, "Oops :( The builtin load profile isn't valid")
	}
	return &profileCfg, nil
}
