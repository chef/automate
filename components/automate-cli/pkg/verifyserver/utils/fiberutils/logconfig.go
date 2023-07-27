package fiberutils

import (
	"os"
	"strings"
	"time"

	"github.com/chef/automate/lib/logger"
	fiberlogger "github.com/gofiber/fiber/v2/middleware/logger"
)

const (
	VERIFY_SERVER_TIMEZONE = "VERIFY_SERVER_TIMEZONE"
	DEFAULT_TIMEZONE       = "UTC"
	LOG_REQ_BODY           = "LOG_REQ_BODY"
)

func CfgLogTimeZone() string {
	timeZone := DEFAULT_TIMEZONE
	if loc, err := time.LoadLocation(DEFAULT_TIMEZONE); err == nil {
		time.Local = loc
	}
	envTimeZone := os.Getenv(VERIFY_SERVER_TIMEZONE)
	if envTimeZone != "" {
		if loc, err := time.LoadLocation(envTimeZone); err == nil {
			timeZone = envTimeZone
			time.Local = loc
		}
	}
	return timeZone
}

func GetLogConfig(log logger.Logger) (lc fiberlogger.Config) {
	lc = fiberlogger.Config{
		TimeFormat: time.RFC3339,
	}

	if isLogReqBodyEnabled() {
		lc.Format = generateLogFormat(
			"${magenta}"+strings.ToUpper(log.NewEntry().Logger.GetLevel().String())+"${reset}",
			"time", "pid", "status", "method", "path", "latency", "error", "bytesReceived", "bytesSent", "body",
		)
	} else {
		lc.Format = generateLogFormat(
			"${green}"+strings.ToUpper(log.NewEntry().Logger.GetLevel().String())+"${reset}",
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

func isLogReqBodyEnabled() bool {
	logReqBody := strings.ToUpper(strings.TrimSpace(os.Getenv(LOG_REQ_BODY))) == "TRUE"
	return logReqBody
}
