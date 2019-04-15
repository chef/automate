package generator

import (
	"time"

	"github.com/burntsushi/toml"
	"github.com/pkg/errors"
)

var now = time.Now()

func ProfileFromFile(path string) (*LoadProfileCfg, error) {
	var profileCfg LoadProfileCfg
	_, err := toml.DecodeFile(path, &profileCfg)
	if err != nil {
		return nil, errors.Wrapf(err, "Invalid load profile %q", path)
	}
	return &profileCfg, nil
}

type LoadProfileCfg struct {
	Templates    Templates    `toml:"templates"`
	GeneratorCfg GeneratorCfg `toml:"generator"`
}

func (l *LoadProfileCfg) BuildRunner() (*LoadGenRunner, error) {
	groups, err := l.BuildSupervisorGroups()
	if err != nil {
		return nil, err
	}
	return &LoadGenRunner{SupervisorGroups: groups}, nil
}

func (l *LoadProfileCfg) BuildSupervisorGroups() ([]*SupervisorGroup, error) {
	groups := []*SupervisorGroup{}

	for _, supCfg := range l.GeneratorCfg.Supervisors {

		group := SupervisorGroup{Name: supCfg.Name, Count: supCfg.Count}

		supT := l.Templates.GetTemplate(supCfg.TemplateName)

		for _, svcT := range supT.ServiceTemplates {
			var m MessagePrototype
			m.SetRelease()
			err := m.ApplySvcTemplate(svcT)
			if err != nil {
				return nil, err
			}
			m.ApplySupCfg(supCfg)
			group.MessagePrototypes = append(group.MessagePrototypes, &m)
		}

		groups = append(groups, &group)
	}
	return groups, nil
}

// [templates]
//   [[templates.supervisor]]
//     # SupervisorTemplate one
//   [[templates.supervisor]]
//     # SupervisorTemplate two
type Templates struct {
	SupervisorTemplates []SupervisorTemplate `toml:"supervisor"`
}

func (ts Templates) GetTemplate(name string) *SupervisorTemplate {
	for _, t := range ts.SupervisorTemplates {
		if t.Name == name {
			return &t
		}
	}

	return nil
}

// name = "pos_terminal"
// [[ service ]]
// package = "noms/pos-service-1"
// [[ service ]]
// package = "noms/pos-service-2"
type SupervisorTemplate struct {
	Name             string            `toml:"name"`
	ServiceTemplates []ServiceTemplate `toml:"service"`
}

// [[ service ]]
// package = "noms/pos-service-2"
type ServiceTemplate struct {
	Package     string `toml:"package"`
	Application string `toml:"application"`
}

// [generator]
//   [[generator.supervisor]]
//     # SupervisorCfg one
//   [[generator.supervisor]]
//     # SupervisorCfg two
type GeneratorCfg struct {
	Supervisors []SupervisorCfg `toml:"supervisor"`
}

// [[generator.supervisor]]
//   name = "pos_terminals:production1"
//   template = "pos_terminal"
//   count = 1000
//   channel = "production1"
//   [site_range]
//     format = "restaurant-prod1-%s"
//     count = 1000
type SupervisorCfg struct {
	Name         string `toml:"name"`
	TemplateName string `toml:"template"`
	Count        int32  `toml:"count"`
	Channel      string `toml:"channel"`
	Environment  string `toml:"environment"`
	Site         string `toml:"site"`
}

//   [site_range]
//     format = "restaurant-prod1-%s"
//     count = 1000
type SiteRange struct {
	FormatStr string `toml:"format"`
	Count     int32  `toml:"count"`
}
