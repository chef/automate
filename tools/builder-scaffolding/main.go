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

	generateCmd := &cobra.Command{
		Use:  "generate",
		RunE: runGenerate,
	}

	rootCmd.AddCommand(generateCmd)
	rootCmd.AddCommand(loginCmd)
	rootCmd.AddCommand(repoCmd)

	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
}
