package config

import (
	"regexp"
)

func MaskPGCredInURI(s string) string {
	selector := regexp.MustCompile("^(.*?)//(.*):(.*)@(.*)$")
	replaceRegex := "${1}//<USER>:<PASSWORD>@${4}"
	maskedPGCredInURI := selector.ReplaceAllString(s, replaceRegex)
	return maskedPGCredInURI
}
