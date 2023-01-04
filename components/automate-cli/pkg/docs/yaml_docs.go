// This file was originally part of cobra/doc and has the following
// license:
//
// Copyright 2016 French Ben. All rights reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
package docs

import (
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/spf13/cobra"
	"github.com/spf13/pflag"
	"gopkg.in/yaml.v2"

	"github.com/chef/automate/components/automate-cli/pkg/status"
)

const (
	Compatibility        = "compatibility"
	RunFrom              = "runFrom"
	ForStandalone        = "forStandalone"
	ForHA                = "forHA"
	AutomateCommandID    = "Automate"
	AutomateCommandTag   = "Automate"
	AutomateHACommandID  = "Automate_HA"
	AutomateHACommandTag = "Automate HA"
	OpenSearch           = "opensearch"
	OpenSearchTag        = "Node:OpenSearch"
	OpenSearchToolTip    = "This command can be run from OpenSearch node"
	Postgres             = "pg"
	PostgresTag          = "Node:Postgres"
	PostgresToolTip      = "This command can be run from Postgres node"
	ChefServer           = "cs"
	ChefServerTag        = "Node:ChefServer"
	ChefServerToolTip    = "This command can be run from Chef Server node"
	Automate             = "automate"
	AutomateTag          = "Node:Automate"
	AutomateToolTip      = "This command can be run from Automate node"
	Bastion              = "bastion"
	BastionTag           = "Node:Bastion"
	BastionToolTip       = "This command can be run from Bastion node"
)

var commandIDs = map[string]string{
	ForHA:         AutomateHACommandID,
	ForStandalone: AutomateCommandID,
}

var commandTags = map[string]string{
	ForHA:         AutomateHACommandTag,
	ForStandalone: AutomateCommandTag,
}

var haCommandTags = map[string]string{
	OpenSearch: OpenSearchTag,
	Postgres:   PostgresTag,
	ChefServer: ChefServerTag,
	Automate:   AutomateTag,
	Bastion:    BastionTag,
}

var haCommandTips = map[string]string{
	OpenSearch: OpenSearchToolTip,
	Postgres:   PostgresToolTip,
	ChefServer: ChefServerToolTip,
	Automate:   AutomateToolTip,
	Bastion:    BastionToolTip,
}

// skipCommands is a list of commands that should be omitted when generating
// documentation. Hidden, deprecated and help commands are excluded by default
// and do not need to be added here.
var skipCommands = []string{
	"chef-automate diagnostics",
}

type cmdOption struct {
	Name         string
	Shorthand    string `yaml:",omitempty"`
	DefaultValue string `yaml:"default_value,omitempty"`
	Usage        string `yaml:",omitempty"`
}

type compatibleString struct {
	Id      string
	Name    string
	Tag     string `yaml:",omitempty"`
	ToolTip string `yaml:",omitempty"`
}

type cmdDoc struct {
	Name                       string
	Synopsis                   string             `yaml:",omitempty"`
	Usage                      string             `yaml:",omitempty"`
	Description                string             `yaml:",omitempty"`
	Options                    []cmdOption        `yaml:",omitempty"`
	StandaloneOptions          []cmdOption        `yaml:",omitempty"`
	HaOptions                  []cmdOption        `yaml:",omitempty"`
	InheritedOptions           []cmdOption        `yaml:"inherited_options,omitempty"`
	StandaloneInheritedOptions []cmdOption        `yaml:"standalone_inherited_options,omitempty"`
	HAInheritedOptions         []cmdOption        `yaml:"ha_inherited_options,omitempty"`
	Example                    string             `yaml:",omitempty"`
	SeeAlso                    []string           `yaml:"see_also,omitempty"`
	Aliases                    []string           `yaml:"aliases,omitempty"`
	CompatibleString           []compatibleString `yaml:",omitempty"`
}

type statusDoc struct {
	Errors []statusError `yaml:"errors"`
}

type statusError struct {
	Name        string `yaml:"name"`
	Description string `yaml:"description"`
	Code        string `yaml:"code"`
}

// GenYamlTree creates yaml structured ref files for this command and all descendants
// in the directory given. This function may not work
// correctly if your command names have `-` in them. If you have `cmd` with two
// subcmds, `sub` and `sub-third`, and `sub` has a subcommand called `third`
// it is undefined which help output will be in the file `cmd-sub-third.1`.
func GenYamlTree(cmd *cobra.Command, dir string) error {
	if err := newStatusDoc().ToYamlFile(filepath.Join(dir, "status", "errors.yaml")); err != nil {
		return err
	}

	identity := func(s string) string { return s }
	emptyStr := func(s string) string { return "" }
	return GenYamlTreeCustom(cmd, dir, emptyStr, identity)
}

// GenYamlTreeCustom creates yaml structured ref files.
func GenYamlTreeCustom(cmd *cobra.Command, dir string, filePrepender, linkHandler func(string) string) error {
	for _, c := range cmd.Commands() {
		if !c.IsAvailableCommand() || c.IsAdditionalHelpTopicCommand() || isSkippedCommand(cmd) {
			continue
		}
		if err := GenYamlTreeCustom(c, dir, filePrepender, linkHandler); err != nil {
			return err
		}
	}

	basename := strings.Replace(cmd.CommandPath(), " ", "_", -1) + ".yaml"
	filename := filepath.Join(dir, "commands", basename)
	if err := os.MkdirAll(filepath.Dir(filename), os.ModePerm); err != nil {
		return err
	}
	f, err := os.Create(filename)
	if err != nil {
		return err
	}
	defer func() {
		_ = f.Close()
	}()

	if _, err := io.WriteString(f, filePrepender(filename)); err != nil {
		return err
	}
	if err := GenYamlCustom(cmd, f, linkHandler); err != nil {
		return err
	}
	return nil
}

// GenYaml creates yaml output.
func GenYaml(cmd *cobra.Command, w io.Writer) error {
	return GenYamlCustom(cmd, w, func(s string) string { return s })
}

// GenYamlCustom creates custom yaml output.
func GenYamlCustom(cmd *cobra.Command, w io.Writer, linkHandler func(string) string) error {
	cmd.InitDefaultHelpCmd()
	cmd.InitDefaultHelpFlag()

	yamlDoc := cmdDoc{}
	yamlDoc.Name = cmd.CommandPath()

	yamlDoc.Synopsis = forceMultiLine(cmd.Short)
	yamlDoc.Description = forceMultiLine(cmd.Long)
	yamlDoc.Usage = forceMultiLine(cmd.UseLine())
	yamlDoc.CompatibleString = make([]compatibleString, 0)

	annotations := cmd.Annotations
	if len(annotations) > 0 {
		switch annotations[Compatibility] {
		case ForHA:
			yamlDoc.CompatibleString = append(yamlDoc.CompatibleString, compatibleString{
				Id:      commandIDs[ForHA],
				Name:    commandTags[ForHA],
				Tag:     haCommandTags[annotations[RunFrom]],
				ToolTip: haCommandTips[annotations[RunFrom]],
			})
		case ForStandalone:
			yamlDoc.CompatibleString = append(yamlDoc.CompatibleString, compatibleString{
				Id:   commandIDs[ForStandalone],
				Name: commandTags[ForStandalone],
			})
		default:
			yamlDoc.CompatibleString = append(yamlDoc.CompatibleString, []compatibleString{
				{
					Id:      commandIDs[ForHA],
					Name:    commandTags[ForHA],
					Tag:     haCommandTags[annotations[RunFrom]],
					ToolTip: haCommandTips[annotations[RunFrom]],
				},
				{
					Id:   commandIDs[ForStandalone],
					Name: commandTags[ForStandalone],
				},
			}...)
		}
	}
	if len(cmd.Aliases) > 0 {
		yamlDoc.Aliases = cmd.Aliases
	}

	if len(cmd.Example) > 0 {
		yamlDoc.Example = cmd.Example
	}

	flags := cmd.NonInheritedFlags()
	if flags.HasFlags() {
		commonOptions, haOptions, standaloneOptions := genFlagResult(flags)
		yamlDoc.Options = commonOptions
		yamlDoc.HaOptions = haOptions
		yamlDoc.StandaloneOptions = standaloneOptions
	}
	flags = cmd.InheritedFlags()
	if flags.HasFlags() {
		//yamlDoc.InheritedOptions = genFlagResult(flags)
		commonOptions, haOptions, standaloneOptions := genFlagResult(flags)
		yamlDoc.InheritedOptions = commonOptions
		yamlDoc.HAInheritedOptions = haOptions
		yamlDoc.StandaloneInheritedOptions = standaloneOptions
	}

	if hasSeeAlso(cmd) {
		result := []string{}
		if cmd.HasParent() {
			parent := cmd.Parent()
			result = append(result, parent.CommandPath()+" - "+parent.Short)
		}
		children := cmd.Commands()
		sort.Sort(byName(children))
		for _, child := range children {
			if !child.IsAvailableCommand() || child.IsAdditionalHelpTopicCommand() {
				continue
			}
			result = append(result, child.Name()+" - "+child.Short)
		}
		yamlDoc.SeeAlso = result
	}

	final, err := yaml.Marshal(&yamlDoc)
	if err != nil {
		return err
	}

	if _, err := w.Write(final); err != nil {
		return err
	}
	return nil
}

func genFlagResult(flags *pflag.FlagSet) ([]cmdOption, []cmdOption, []cmdOption) {
	var result, haResults, standaloneResults []cmdOption

	flags.VisitAll(func(flag *pflag.Flag) {
		if flag.Hidden {
			return
		}
		// Todo, when we mark a shorthand is deprecated, but specify an empty message.
		// The flag.ShorthandDeprecated is empty as the shorthand is deprecated.
		// Using len(flag.ShorthandDeprecated) > 0 can't handle this, others are ok.
		if !(len(flag.ShorthandDeprecated) > 0) && len(flag.Shorthand) > 0 {
			opt := cmdOption{
				Name:         flag.Name,
				Shorthand:    flag.Shorthand,
				DefaultValue: flag.DefValue,
				Usage:        forceMultiLine(flag.Usage),
			}
			if len(flag.Annotations[Compatibility]) > 0 {
				switch flag.Annotations[Compatibility][0] {
				case ForHA:
					haResults = append(haResults, opt)
				case ForStandalone:
					standaloneResults = append(standaloneResults, opt)
				}
			} else {
				result = append(result, opt)
			}
		} else {
			opt := cmdOption{
				Name:         flag.Name,
				DefaultValue: forceMultiLine(flag.DefValue),
				Usage:        forceMultiLine(flag.Usage),
			}
			if len(flag.Annotations[Compatibility]) > 0 {
				switch flag.Annotations[Compatibility][0] {
				case ForHA:
					haResults = append(haResults, opt)
				case ForStandalone:
					standaloneResults = append(standaloneResults, opt)
				}
			} else {
				result = append(result, opt)
			}
		}
	})

	return result, haResults, standaloneResults
}

// isSkippedCommand determines if the given cobra command should not have its
// documentation auto-generated.
func isSkippedCommand(cmd *cobra.Command) bool {
	for _, path := range skipCommands {
		if path == cmd.CommandPath() {
			return true
		}
	}

	return false
}

func newStatusDoc() *statusDoc {
	errors := []statusError{}
	for _, e := range status.ErrorMetadata {
		errors = append(errors, statusError{
			Code:        e[0],
			Name:        e[1],
			Description: e[2],
		})
	}
	sort.Sort(byExitCode(errors))

	return &statusDoc{Errors: errors}
}

// ToYamlFile takes
func (d *statusDoc) ToYamlFile(path string) error {
	path, err := filepath.Abs(path)
	if err != nil {
		return err
	}

	if err = os.MkdirAll(filepath.Dir(path), os.ModePerm); err != nil {
		return err
	}

	f, err := os.Create(path)
	if err != nil {
		return err
	}
	defer func() {
		_ = f.Close()
	}()

	bytes, err := yaml.Marshal(d)
	if err != nil {
		return err
	}

	_, err = f.Write(bytes)
	return err
}
