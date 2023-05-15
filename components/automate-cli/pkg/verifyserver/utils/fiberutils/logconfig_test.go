package fiberutils_test

import (
	"os"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

func TestLogUtils(t *testing.T) {
	l, err := logger.NewLogger("text", "debug")
	assert.NoError(t, err)

	t.Run("It should create the debug format in case of debug level", func(t *testing.T) {
		lc := fiberutils.GetLogConfig(l)
		assert.Contains(t, lc.Format, "| ${magenta}DEBUG${reset} | ${red}${time}${reset}  |  ${green}${pid}${reset}  |"+
			"  ${yellow}${status}${reset}  |  ${blue}${method}${reset}  |  ${magenta}${path}${reset}  |"+
			"  ${cyan}${latency}${reset}  |  ${white}${error}${reset}  |  ${red}${bytesReceived}${reset}  |"+
			"  ${green}${bytesSent}${reset}  |  ${yellow}${body}${reset}  |  \n")
	})
	t.Run("It should create the info format in case of trace level", func(t *testing.T) {
		l, err := logger.NewLogger("text", "trace")
		assert.NoError(t, err)
		lc := fiberutils.GetLogConfig(l)
		assert.Contains(t, lc.Format, "| ${green}TRACE${reset} | ${red}${time}${reset}  |  ${green}${pid}${reset}  |"+
			"  ${yellow}${status}${reset}  |  ${blue}${method}${reset}  |  ${magenta}${path}${reset}  |"+
			"  ${cyan}${latency}${reset}  |  ${white}${error}${reset}  |  ${red}${bytesReceived}${reset}  |"+
			"  ${green}${bytesSent}${reset}  |  \n")
	})
	t.Run("It should print the selected timezone", func(t *testing.T) {
		assert.Equal(t, "UTC", fiberutils.CfgLogTimeZone())
	})
	t.Run("It should use UTC if timezone format is incorrect", func(t *testing.T) {
		os.Setenv(fiberutils.VERIFY_SERVER_TIMEZONE, "GAMU")
		defer os.Unsetenv(fiberutils.VERIFY_SERVER_TIMEZONE)
		assert.Equal(t, fiberutils.DEFAULT_TIMEZONE, fiberutils.CfgLogTimeZone())
	})
	t.Run("It should use timezone provided its correct", func(t *testing.T) {
		testTimeZone := "America/New_York"
		os.Setenv(fiberutils.VERIFY_SERVER_TIMEZONE, testTimeZone)
		defer os.Unsetenv(fiberutils.VERIFY_SERVER_TIMEZONE)
		assert.Equal(t, testTimeZone, fiberutils.CfgLogTimeZone())
	})
}
