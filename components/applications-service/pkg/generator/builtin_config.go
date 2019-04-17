package generator

import (
	"github.com/burntsushi/toml"
	"github.com/pkg/errors"
)

const builtinConfigToml = `
[templates]
  [[templates.supervisor]]
    name = "three-svc-sup"
    [[ templates.supervisor.service ]]
      package = "example/service-1"
      application = "example-app"
    [[ templates.supervisor.service ]]
      package = "example/service-2"
      application = "example-app"
    [[ templates.supervisor.service ]]
      package = "example/service-3"
      application = "example-app"

[generator]
  [[generator.supervisor]]
		name = "supervisors:qa"
    template = "three-svc-sup"
    count = 10
    channel = "qa"
    environment = "qa"
    site = "site1"
`

func BuiltinConfig() (*LoadProfileCfg, error) {
	var profileCfg LoadProfileCfg
	_, err := toml.Decode(builtinConfigToml, &profileCfg)
	if err != nil {
		return nil, errors.Wrap(err, "Oops :( The builtin load profile isn't valid")
	}
	return &profileCfg, nil
}
