package config

import (
	"bytes"
	"io/ioutil"
	"regexp"
)

var (
	nameServerRegex = regexp.MustCompile(`^\s*nameserver\s*(.+?)\s*$`)
)

/*GetNameServersFromResolveConfig parses a file in the format of /etc/resolv.conf
  The resolve.conf format can be seen by looking at `man resolv.conf`.
  This function looks up at the lines starting with `nameserver`.
  There can be a single nameserver each line and maximum 3 in the file.
*/
func GetNameServersFromResolveConfig(filePath string) ([]string, error) {
	content, err := ioutil.ReadFile(filePath)
	if err != nil {
		return nil, err
	}

	nameservers := ParseNameServers(content)
	return nameservers, nil
}

//ParseNameServers parses an array of bytes and returns the nameservers
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
		server := nameServerRegex.FindSubmatch([]byte(contentToParse))

		if len(server) > 0 {
			nameservers = append(nameservers, string(server[1]))
		}
	}

	return nameservers
}
