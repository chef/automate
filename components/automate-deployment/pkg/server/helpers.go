package server

import (
	"bytes"
	"fmt"
)

func LogRotateConf(path string, params ...string) string {
	if len(params) < 1 {
		return ""
	}
	var buffer bytes.Buffer
	buffer.WriteString(path + " {" + "\n")
	for _, param := range params {
		buffer.WriteString("\t" + param + "\n")
	}
	buffer.WriteString("}" + "\n")
	return buffer.String()
}

func getLogFileName(path string) string {
	if string(path[len(path)-1]) == "/" {
		return fmt.Sprintf("%sautomate.log", path)
	} else {
		return fmt.Sprintf("%s/automate.log", path)
	}
}

func getConcatStringFromConfig(constant string, variable interface{}) string {
	return fmt.Sprintf("%s %v", constant, variable)
}
