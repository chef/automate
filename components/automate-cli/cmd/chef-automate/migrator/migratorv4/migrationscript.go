package migratorV4

import (
	"fmt"
	"os/exec"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

const habrootcmd = "HAB_LICENSE=accept-no-persist hab pkg path chef/deployment-service"

const fscript = `
mv %[1]vsvc/automate-opensearch/data %[1]vsvc/automate-opensearch/data.os; 
mv %[1]vsvc/automate-opensearch/var %[1]vsvc/automate-opensearch/var.os; 
cp -r %[1]vsvc/automate-elasticsearch/data %[1]vsvc/automate-opensearch/; 
cp -r %[1]vsvc/automate-elasticsearch/var %[1]vsvc/automate-opensearch/; 
chown -RL hab:hab %[1]vsvc/automate-opensearch/data; 
chown -RL hab:hab %[1]vsvc/automate-opensearch/var;`

type MigrationScript struct {
	writer *cli.Writer
	utils  MigratorV4Utils
}

func NewMigrationScript(w *cli.Writer, utils MigratorV4Utils) *MigrationScript {
	return &MigrationScript{
		writer: w,
		utils:  utils,
	}
}

func (ms *MigrationScript) Run() error {
	habRoot := ms.utils.GetHabRootPath(habrootcmd)
	script := fmt.Sprintf(fscript, habRoot)
	command := exec.Command("/bin/sh", "-c", script)
	return command.Run()
}

func (ms *MigrationScript) Skip() error {
	return nil
}

func (ms *MigrationScript) ExitHandler() error {
	return nil
}
