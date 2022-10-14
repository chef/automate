package main

import (
	"fmt"

	"github.com/spf13/cobra"
)

var rootCaFlags = struct {
	rootCa      string
	privateCert string
	publicCert  string
}{}

var sshFlag = struct {
	automate bool
	pg       bool
}{}

// certRotateCmd represents the certRotate command
var certRotateCmd = &cobra.Command{
	Use:   "certRotate",
	Short: "Chef Automate rotate cert",
	Long:  "Chef Automate CLI command to rotate certificates",
	RunE:  certRotate,
}

func init() {
	RootCmd.AddCommand(certRotateCmd)

	certRotateCmd.PersistentFlags().BoolVar(&sshFlag.automate, "automate", false, "Automate ha server name to ssh")
	certRotateCmd.PersistentFlags().BoolVar(&sshFlag.pg, "pg", false, "Automate ha server name to ssh")

	certRotateCmd.PersistentFlags().StringVar(&rootCaFlags.rootCa, "root-ca", "", "Automate Root CA value")
	certRotateCmd.PersistentFlags().StringVar(&rootCaFlags.privateCert, "private-cert", "", "Automate ha private certificate")
	certRotateCmd.PersistentFlags().StringVar(&rootCaFlags.publicCert, "public-cert", "", "Automate ha public certificate")
}

func certRotate(cmd *cobra.Command, args []string) error {
	fmt.Println(rootCaFlags.rootCa)
	if sshFlag.automate {
		fmt.Println("automate")
	} else {
		fmt.Println("pg")
	}
	return nil
}
