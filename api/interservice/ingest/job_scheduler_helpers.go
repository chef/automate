package ingest

import (
	"time"

	"github.com/chef/automate/lib/simpledatemath"
)

// Validate is a helper function for the JobSettings proto message that will validate
// that every field has the right values, if one of them doesn't have a valid value
// then it will return an error with the problem and a suggestion to fix it
func (js *JobSettings) Validate() error {
	// Validate field: Every
	if e := js.GetEvery(); len(e) > 0 {
		_, err := time.ParseDuration(e)
		if err != nil {
			return err
		}
	}

	// Validate field: Threshold
	if t := js.GetThreshold(); len(t) > 0 {
		err := simpledatemath.Validate(t)
		if err != nil {
			return err
		}
	}

	return nil
}
