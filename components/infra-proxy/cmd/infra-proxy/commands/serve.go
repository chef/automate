package commands

import (
	"fmt"
	"time"

	"github.com/spf13/cobra"
)

var serveCmd = &cobra.Command{
	Use:   "serve",
	Short: fmt.Sprintf("Launches the automate infra proxy service..."),
	Run: func(cmd *cobra.Command, args []string) {
		for {
			time.Sleep(10 * time.Second);
		}
	},
}
