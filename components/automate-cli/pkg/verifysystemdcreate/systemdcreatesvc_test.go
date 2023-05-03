package verifysystemdcreate_test

import (
	"testing"
)

func TestCreateDestinationAndCopyFunc(t *testing.T) {
	// cw := majorupgrade_utils.NewCustomWriter()
	// tests := []struct {
	// 	createDestinationAndCopy func(binarySrcPath, binaryDestPath string) error
	// 	executeShellCommand      func(command string) error
	// 	binaryDestinationFolder  string
	// 	systemdLocation          string
	// 	writer                   *cli.Writer
	// }{
	// 	{
	// 		createDestinationAndCopy: func(binarySrcPath, binaryDestPath string) error {
	// 			return nil
	// 		},
	// 	},
	// }
	// statusEndpoint := "/status"
	// // Setup the app as it is done in the main function
	// app := SetupDefaultHandlers(SetupMockStatusService())

	// for _, test := range tests {
	// 	t.Run(test.description, func(t *testing.T) {
	// 		req := httptest.NewRequest("GET", statusEndpoint, nil)
	// 		req.Header.Add("Content-Type", "application/json")
	// 		res, err := app.Test(req, -1)
	// 		assert.NoError(t, err)
	// 		body, err := ioutil.ReadAll(res.Body)
	// 		assert.NoError(t, err, test.description)
	// 		assert.Contains(t, string(body), test.expectedBody)
	// 		assert.Equal(t, res.StatusCode, test.expectedCode)
	// 	})
	// }
}
