package getosutils

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

const successfile = "../../services/softwareversionservice/testfiles/success.txt"
const failurefile = "../../services/softwareversionservice/testfiles/wrongfile.txt"

func TestGetOsVersion(t *testing.T) {
	type args struct {
		osFilepath string
	}
	tests := []struct {
		description string
		args        args
		osName      string
		osVersion   string
		expectedErr string
	}{
		{
			description: "When we get the OSName and OSVersion correct",
			args: args{
				osFilepath: successfile,
			},
			osName:      "Ubuntu 20.04.6",
			osVersion:   "20.04",
			expectedErr: "",
		},
		{
			description: "When we are not able to get the OSName or OSVersion",
			args: args{
				osFilepath: failurefile,
			},
			osName:      "",
			osVersion:   "",
			expectedErr: "open ../../services/softwareversionservice/testfiles/wrongfile.txt: no such file or directory",
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			got, got1, err := GetOsVersion(tt.args.osFilepath)
			if err != nil {
				assert.Equal(t, tt.expectedErr, err.Error())
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
