package main

import (
	"fmt"
	"os"

	"github.com/spf13/cobra"
)

func main() {
	rootCmd := &cobra.Command{
		Use: "builder-scaffolding",
	}

	rootCmd.AddCommand(generateCmd)
	rootCmd.AddCommand(loginCmd)
	rootCmd.AddCommand(repoCmd)
	rootCmd.AddCommand(integrityCmd)

	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
}
