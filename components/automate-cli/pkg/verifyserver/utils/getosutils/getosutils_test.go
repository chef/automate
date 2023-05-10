package getosutils

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

// const failurefile = "components/automate-cli/pkg/verifyserver/services/softwareversionservice/testfiles/successfile"
const successfile = "../../services/softwareversionservice/testfiles/successfile"
const failurefile = "../../services/softwareversionservice/testfiles/file"

func TestGetOsVersion(t *testing.T) {
	type args struct {
		osFilepath string
	}
	tests := []struct {
		description string
		args        args
		osName      string
		osVersion   string
		expectedErr bool
	}{
		{
			description: "When we get the OSName and OSVersion correct",
			args: args{
				osFilepath: successfile,
			},
			osName:      "Ubuntu 20.04.6",
			osVersion:   "20.04",
			expectedErr: false,
		},
		{
			description: "When we are not able to get the OSName or OSVersion",
			args: args{
				osFilepath: failurefile,
			},
			osName:      "",
			osVersion:   "",
			expectedErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			got, got1, err := GetOsVersion(tt.args.osFilepath)
			if (err != nil) != tt.expectedErr {
				assert.Equal(t, err, tt.expectedErr)
				return
			}
			if got != tt.osName {
				assert.Equal(t, got, tt.osName)
			}
			if got1 != tt.osVersion {
				assert.Equal(t, got, tt.osVersion)
			}
		})
	}
}
