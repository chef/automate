package upgradeinspectorv5

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
)

type ReindexingInspection struct {
	writer       *cli.Writer
	isExternalOS bool
}

func (ri *ReindexingInspection) ShowInfo(index *int) error {
	if !ri.isExternalOS {
		res, err := ri.writer.Confirm(fmt.Sprintf("%d. Have you done the reindexing?", *index))
		if err != nil {
			ri.writer.Error(err.Error())
			return status.Errorf(status.InvalidCommandArgsError, err.Error())
		}
		if !res {
			ri.writer.Error(reindexingError)
			return status.New(status.InvalidCommandArgsError, reindexingError)
		}
		*index++
	}
	return nil
}

func NewReindexingInspection(w *cli.Writer, isExternalOS bool) *ReindexingInspection {
	return &ReindexingInspection{
		writer:       w,
		isExternalOS: isExternalOS,
	}
}
