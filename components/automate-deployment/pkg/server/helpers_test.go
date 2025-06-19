package server

import (
	"github.com/stretchr/testify/assert"
	"testing"
)

func TestGetLogFileName(t *testing.T) {
	t.Run("when the path has / in the end", func(t *testing.T) {
		pathForFile := "/var/log/"
		expectedString := "/var/log/automate.log"
		actualString := getLogFileName(pathForFile)
		assert.Equal(t, expectedString, actualString)
	})
	t.Run("when the path doesn't have / in the end", func(t *testing.T) {
		pathForFile := "/var/log"
		expectedString := "/var/log/automate.log"
		actualString := getLogFileName(pathForFile)
		assert.Equal(t, expectedString, actualString)
	})
}

func TestGetConcatStringFromConfig(t *testing.T) {
	t.Run("when passing the config for rotate number", func(t *testing.T) {
		constant := "rotate"
		variable := 10

		expectedString := "rotate 10"
		actualString := getConcatStringFromConfig(constant, variable)
		assert.Equal(t, expectedString, actualString)

	})
	t.Run("when passing the config for max size for logs", func(t *testing.T) {
		constant := "maxsize"
		variable := "100M"

		expectedString := "maxsize 100M"
		actualString := getConcatStringFromConfig(constant, variable)
		assert.Equal(t, expectedString, actualString)

	})
}

func TestForGetContentForLogRotateConfigFile(t *testing.T) {
	t.Run("When having parameters", func(t *testing.T) {
		maxSizeString := "size 100M"
		rotateString := "rotate 10"
		pathString := "/var/log/automate.log"
		expectedString := `/var/log/automate.log {
	dateext
	size 100M
	rotate 10
	copytruncate
	missingok
	dateformat -%Y%m%d%s
}
`

		actualString := LogRotateConf(pathString, "dateext", maxSizeString, rotateString, "copytruncate", "missingok", "dateformat -%Y%m%d%s")

		assert.Equal(t, actualString, expectedString)

	})
	t.Run("When zero parameters", func(t *testing.T) {
		actualString := LogRotateConf("test")
		assert.Equal(t, "", actualString)
	})

}
