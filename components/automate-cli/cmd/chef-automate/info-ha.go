// Copyright © 2017 Chef Software

package main

import (
	"bytes"
	"errors"
	"fmt"
	"text/template"

	"github.com/chef/automate/components/automate-cli/pkg/docs"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

var (
	infoCommandTemp = `AUTOMATION DETAILS:{{- "\n"}}
	{{- "\t"}}{{- "Automate Admin User:"}} {{"\t\t"}} {{.Outputs.AutomateAdminUser.Value}}{{- "\n"}}
	{{- "\t"}}{{- "Automate Data Collector Token:"}} {{"\t"}} {{.Outputs.AutomateDataCollectorToken.Value }}{{- "\n"}}
	
	{{- "\t"}}{{- "Automate Private IPs:"}} {{"\t\t"}} {{- range .Outputs.AutomatePrivateIps.Value}} {{.}}{{"\n\t\t\t\t\t"}}
	{{- end}}
	{{- "\n"}}

	{{- "\t"}}{{- "Automate SSH:"}} {{"\t\t\t"}} {{- range .Outputs.AutomateSSH.Value}} {{.}}{{"\n\t\t\t\t\t"}}
	{{- end}}
	{{- "\n"}}

	{{- "\t"}}{{- "Automate URL:"}} {{"\t\t\t"}} {{.Outputs.AutomateURL.Value }}{{- "\n"}}x
	{{- "\t"}}{{- "Backup Config EFS:"}} {{"\t\t"}} {{.Outputs.BackupConfigEFS.Value }}{{- "\n"}}
	{{- "\t"}}{{- "Backup Config S3:"}} {{"\t\t"}} {{.Outputs.BackupConfigS3.Value }}{{- "\n"}}
	
	{{- "\t"}}{{- "Chef Server Private IPs:"}} {{"\t"}} {{- range .Outputs.ChefServerPrivateIps.Value}} {{.}}{{"\n\t\t\t\t\t"}}
	{{- end}}
	{{- "\n"}}

	{{- "\t"}}{{- "Chef Server SSH:"}} {{"\t\t"}} {{- range .Outputs.ChefServerSSH.Value}} {{.}}{{"\n\t\t\t\t\t"}}
	{{- end}}
	{{- "\n"}}

	{{- "\t"}}{{- "Opensearch Private IPs:"}} {{"\t"}} {{- range .Outputs.OpensearchPrivateIps.Value}} {{.}}{{"\n\t\t\t\t\t"}}
	{{- end}}
	{{- "\n"}}

	{{- "\t"}}{{- "Opensearch Public IPs:"}} {{"\t\t"}} {{- range .Outputs.OpensearchPublicIps.Value}} {{.}}{{"\n\t\t\t\t\t"}}
	{{- end}}
	{{- "\n"}}

	{{- "\t"}}{{- "Opensearch SSH:"}} {{"\t\t"}} {{- range .Outputs.OpensearchSSH.Value}} {{.}}{{"\n\t\t\t\t\t"}}
	{{- end}}
	{{- "\n"}}

	{{- "\t"}}{{- "Postgresql Private IPs:"}} {{"\t"}} {{- range .Outputs.PostgresqlPrivateIps.Value}} {{.}}{{"\n\t\t\t\t\t"}}
	{{- end}}
	{{- "\n"}}

	{{- "\t"}}{{- "Postgresql SSH:"}} {{"\t\t"}} {{- range .Outputs.PostgresqlSSH.Value}} {{.}}{{"\n\t\t\t\t\t"}}
	{{- end}}
	{{- "\n"}}
	{{- "\t"}}{{- "SSH Key File:"}} {{"\t\t\t"}} {{.Outputs.SSHKeyFile.Value}}{{- "\n"}}
	{{- "\t"}}{{- "SSH Port:"}} {{"\t\t\t"}} {{.Outputs.SSHPort.Value}}{{- "\n"}}
	{{- "\t"}}{{- "SSH User:"}} {{"\t\t\t"}} {{.Outputs.SSHUser.Value}}{{- "\n"}}

`
)

func init() {
	infoCmd.SetUsageTemplate(infoHelpDocs)
	RootCmd.AddCommand(infoCmd)
}

var infoCmd = &cobra.Command{
	Use:   "info",
	Short: "Info about Automate HA",
	Long:  "Info for Automate HA cluster",
	Annotations: map[string]string{
		NoCheckVersionAnnotation: NoCheckVersionAnnotation,
		docs.Compatibility:       docs.CompatiblewithHA,
	},
	RunE: runInfoConfigCmd,
}

func runInfoConfigCmd(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		return execInfo()
	}
	return errors.New(AUTOMATE_HA_INVALID_BASTION)
}

func execInfo() error {

	automate, err := getAutomateHAInfraDetails()
	if err != nil {
		return err

	}
	var b bytes.Buffer
	err = printInfo(infoCommandTemp, automate, &b)
	if err != nil {
		return err
	}
	fmt.Print(b.String())
	return nil
}

func printInfo(infoCommandTemplate string, automate *AutomateHAInfraDetails, writer *bytes.Buffer) error {
	tmpl, err := template.New("output").Parse(infoCommandTemplate)
	if err != nil {
		logrus.Errorf("Error: %v", err)
		return err
	}
	if err := tmpl.Execute(writer, automate); err != nil {
		logrus.Errorf("Error: %v", err)
		return err
	}
	return nil
}

func printInfoTest(automate *AutomateHAInfraDetails, writer *bytes.Buffer) error {
	tmpl, err := template.New("output").Parse(infoCommandTemp)
	if err != nil {
		logrus.Errorf("Error: %v", err)
		return err
	}
	if err := tmpl.Execute(writer, automate); err != nil {
		logrus.Errorf("Error: %v", err)
		return err
	}
	return nil
}
