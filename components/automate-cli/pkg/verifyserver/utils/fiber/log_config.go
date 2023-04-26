package fiber_utils

import (
	"strings"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/logger"
	"github.com/gofiber/fiber/middleware"
	"github.com/sirupsen/logrus"
)

func GetLogConfig(log logger.ILogger) (lc middleware.LoggerConfig) {
	lc = middleware.LoggerConfig{
		TimeFormat: time.RFC3339,
		TimeZone:   "Asia/Kolkata",
	}
	if log.GetLevel() <= logrus.DebugLevel {
		lc.Format = generateLogFormat(
			"${magenta}"+strings.ToUpper(log.GetLevel().String())+"${reset}",
			"time", "pid", "status", "method", "path", "latency", "error", "bytesReceived", "bytesSent", "body",
		)
	} else {
		lc.Format = generateLogFormat(
			"${green}"+strings.ToUpper(log.GetLevel().String())+"${reset}",
			"time", "pid", "status", "method", "path", "latency", "error", "bytesReceived", "bytesSent",
		)
	}
	return lc
}

func generateLogFormat(prefix string, fields ...string) string {
	color := []string{
		"red", "green", "yellow", "blue", "magenta", "cyan", "white",
	}
	logFormat := "| " + prefix + " | "
	for i, v := range fields {
		c := color[i%7]
		logFormat += "${" + c + "}${" + v + "}${reset}  |  "
	}
	logFormat += "\n"
	return logFormat
}
