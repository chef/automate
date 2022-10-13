package main

import (
	"fmt"

	"github.com/spf13/cobra"
)

// certRotateCmd represents the certRotate command
var certRotateCmd = &cobra.Command{
	Use:   "certRotate",
	Short: "A brief description of your command",
	Long: `A longer description that spans multiple lines and likely contains examples
and usage of using your command. For example:

Cobra is a CLI library for Go that empowers applications.
This application is a tool to generate the needed files
to quickly create a Cobra application.`,
	Run: func(cmd *cobra.Command, args []string) {
		fmt.Println("certRotate called")
	},
}

func init() {
	RootCmd.AddCommand(certRotateCmd)

	// Here you will define your flags and configuration settings.

	// Cobra supports Persistent Flags which will work for this command
	// and all subcommands, e.g.:
	// certRotateCmd.PersistentFlags().String("foo", "", "A help for foo")

	// Cobra supports local flags which will only run when this command
	// is called directly, e.g.:
	// certRotateCmd.Flags().BoolP("toggle", "t", false, "Help message for toggle")
}
