package arrayutils_test

import (
	"testing"

	"github.com/bmizerany/assert"
	"github.com/chef/automate/lib/arrayutils"
)

func TestRemoveStringDuplicates(t *testing.T) {

	testCases := []struct {
		title         string
		inputArray    []string
		expectedArray []string
		wantErr       bool
		expectedErr   error
	}{
		{
			title:         "Have duplicate value",
			inputArray:    []string{"0.0.0.0", "0.0.0.0", "0.0.0.1"},
			expectedArray: []string{"0.0.0.0", "0.0.0.1"},
			wantErr:       false,
			expectedErr:   nil,
		},
		{
			title:         "Have no duplicate value",
			inputArray:    []string{"0.0.0.0", "0.0.0.1"},
			expectedArray: []string{"0.0.0.0", "0.0.0.1"},
			wantErr:       false,
			expectedErr:   nil,
		},
		{
			title:         "empty array",
			inputArray:    []string{},
			expectedArray: []string{},
			wantErr:       false,
			expectedErr:   nil,
		},
	}
	for _, testCase := range testCases {
		t.Run(testCase.title, func(t *testing.T) {
			output := arrayutils.RemoveStringDuplicates(testCase.inputArray)
			if !testCase.wantErr {
				// assert.Error(t, err)
				assert.Equal(t, testCase.expectedArray, output)
			}
		})
	}

}
