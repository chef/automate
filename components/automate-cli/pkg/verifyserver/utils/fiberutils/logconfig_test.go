package fiberutils_test

import (
	"os"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/stretchr/testify/assert"
)

func TestSystemdCreateUtils(t *testing.T) {
	l, err := logger.NewLogger("text", "debug")
	assert.NoError(t, err)
	cw := majorupgrade_utils.NewCustomWriter()

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
		l, err := logger.NewLoggerWithOut("text", "debug", cw.CliWriter)
		assert.NoError(t, err)
		fiberutils.GetLogConfig(l)
		assert.Contains(t, cw.Output(), "Using TimeZone: UTC")
	})
	t.Run("It should use UTC if timezone format is incorrect", func(t *testing.T) {
		os.Setenv(fiberutils.VERIFY_SERVER_TIMEZONE, "GAMU")
		defer os.Unsetenv(fiberutils.VERIFY_SERVER_TIMEZONE)
		lc := fiberutils.GetLogConfig(l)
		assert.Contains(t, lc.TimeZone, fiberutils.DEFAULT_TIMEZONE)
	})
	t.Run("It should use timezone provided its correct", func(t *testing.T) {
		testTimeZone := "America/New_York"
		os.Setenv(fiberutils.VERIFY_SERVER_TIMEZONE, testTimeZone)
		defer os.Unsetenv(fiberutils.VERIFY_SERVER_TIMEZONE)
		lc := fiberutils.GetLogConfig(l)
		assert.Contains(t, lc.TimeZone, testTimeZone)
	})
}
