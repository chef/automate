package version

import "fmt"

// Version represents the current version of Delve.
type Version struct {
	Major    string
	Minor    string
	Patch    string
	Metadata string
	Build    string
}

var (
	// DelveVersion is the current version of Delve.
	DelveVersion = Version{
		Major: "1", Minor: "3", Patch: "0", Metadata: "",
		Build: "$Id: 2f59bfc686d60989dcef9de40b480d0a34aa2fa5 $",
	}
)

func (v Version) String() string {
	ver := fmt.Sprintf("Version: %s.%s.%s", v.Major, v.Minor, v.Patch)
	if v.Metadata != "" {
		ver += "-" + v.Metadata
	}
	return fmt.Sprintf("%s\nBuild: %s", ver, v.Build)
}
