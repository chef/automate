package config

import (
	"regexp"
)

func MaskPGCredInURI(s string) string {
	selector := regexp.MustCompile("^(.*?)postgresql://(.*):(.*)@(.*)$")
	replaceRegex := "${1}postgresql://<USER>:<PASSWORD>@${4}"
	maskedPGCredInURI := selector.ReplaceAllString(s, replaceRegex)
	return maskedPGCredInURI
}
