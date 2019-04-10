package commands

import (
	"fmt"
	"strings"
	"time"

	"github.com/burntsushi/toml"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	//uuid "github.com/chef/automate/lib/uuid4"
)

var now = time.Now()

func newDescribeCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "describe",
		Short: "explains what running a load profile will do",
		RunE:  runDescribeCmd,
	}
}

func runDescribeCmd(cmd *cobra.Command, args []string) error {
	if profileFile == "" {
		return errors.New("no profile filename given")
	}

	fmt.Printf("Reading profile %q\n", profileFile)

	var profile LoadProfile
	_, err := toml.DecodeFile(profileFile, &profile)
	if err != nil {
		fmt.Printf("Invalid load profile\nError: %s\n", err)
		return err
	}

	//fmt.Printf("%+v", profile)

	for i, supCfg := range profile.GeneratorCfg.Supervisors {
		supT := profile.Templates.GetTemplate(supCfg.TemplateName)

		headerStr := fmt.Sprintf("Supervisor group %d: %s", i, supCfg.Name)
		fmt.Println(headerStr)
		fmt.Println(strings.Repeat("=", len(headerStr)))
		fmt.Println("")

		fmt.Printf("Number of simulated supervisors: %d\n", supCfg.Count)
		fmt.Printf("Number of services per supervisor: %d\n", len(supT.ServiceTemplates))

		for _, svcT := range supT.ServiceTemplates {
			var m MessagePrototype
			m.SetRelease()
			m.ApplySvcTemplate(svcT)
			m.ApplySupCfg(supCfg)
			fmt.Println(m.PrettyStr())
		}

	}

	return nil
}

// Data elements of a message that currently exist:
// sup-id         AUTO
// group          hardcode to "default"
// application    assign per-service in sup template
// environment    <name>   The environment name of the current deployment
// health         hardcode to 0 (OK) for now.
// status         hardcode to 0 (RUNNING) for now
//
// Package Indentifier
// --origin      parse from service name
// --name        parse from service name
// --version     hardcode to 1.0.0 for now
// --release     generate from the startup time of the generator
type MessagePrototype struct {
	Application string
	Environment string
	Channel     string
	Site        string
	Origin      string
	PkgName     string
	Release     string
}

func (m *MessagePrototype) SetRelease() {
	m.Release = now.Format("20060102150405")
}

func (m *MessagePrototype) ApplySvcTemplate(t ServiceTemplate) {
	// TODO: make this safer
	parts := strings.Split(t.Package, "/")
	m.Origin = parts[0]
	m.PkgName = parts[1]
	m.Application = t.Application
}

func (m *MessagePrototype) ApplySupCfg(s SupervisorCfg) {
	m.Environment = s.Environment
	m.Channel = s.Channel
	m.Site = s.Site
}

func (m *MessagePrototype) PrettyStr() string {
	var b strings.Builder
	b.WriteString(fmt.Sprintf("  - Package: %s/%s/%s\n", m.Origin, m.PkgName, m.Release))
	b.WriteString(fmt.Sprintf("  - Application: %s\n", m.Application))
	b.WriteString(fmt.Sprintf("  - Environment: %s\n", m.Environment))
	b.WriteString(fmt.Sprintf("  - Channel: %s\n", m.Channel))
	b.WriteString(fmt.Sprintf("  - Site: %s\n", m.Site))
	return b.String()
}

// FIXME: move all this into pkg/ after prototyping is done

type LoadProfile struct {
	Templates    Templates    `toml:"templates"`
	GeneratorCfg GeneratorCfg `toml:"generator"`
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
