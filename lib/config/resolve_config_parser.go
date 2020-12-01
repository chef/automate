package config

import (
	"bytes"
	"io/ioutil"
	"net"
	"regexp"
)

var (
	defaultResolvConf = "/etc/resolv.conf"
	nameServerRegex   = regexp.MustCompile(`^\s*nameserver\s*(.+?)\s*$`)
)

// GetNameServersFromResolveConfig parses a file in the format of /etc/resolv.conf
// The resolve.conf format is described in resolv.conf(5):
//
//    https://man7.org/linux/man-pages/man5/resolv.conf.5.html
//
// This function looks up at the lines starting with `nameserver`.
// There can be a single nameserver each line and maximum 3 (defined
// my MAXNS in resolv.h) in the file.
func GetNameServersFromResolveConfig(filePath string) ([]string, error) {
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return nil, err
	}

	nameservers := ParseNameServers(content)
	return nameservers, nil
}

// ParseNameServers parses an array of bytes and returns the nameservers
// The content should be of format of /etc/resolv.conf i.e.
// nameserver 12.0.0.3
func ParseNameServers(fileContent []byte) []string {
	fileLines := bytes.Split(fileContent, []byte("\n"))
	var nameservers []string
	for _, currentLine := range fileLines {
		var contentToParse = currentLine
		var commentIndicatorIndex = bytes.Index(currentLine, []byte("#"))
		if commentIndicatorIndex != -1 {
			// Only check the content before the comment section
			contentToParse = currentLine[:commentIndicatorIndex]
		}

		server := nameServerRegex.FindSubmatch(contentToParse)
		if len(server) == 2 {
			address := string(server[1])
			if net.ParseIP(address) != nil {
				nameservers = append(nameservers, address)
			}
		}
	}

	return nameservers
}

// GetSystemResolvers returns resolvers discovered via
// /etc/resolv.conf. If not valid resolvers are found, an empty array
// is returned.
func GetSystemResolvers() []string {
	resolvers, err := GetNameServersFromResolveConfig(defaultResolvConf)
	if err != nil {
		return []string{}
	}
	return resolvers
}
