package manifest

import (
	"encoding/json"
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

func TestA2ManifestJSON(t *testing.T) {
	hart, err := habpkg.HartFromPath("/some/path/ssd-deployment-service-0.1.0-20180119115432-x86_64-linux.hart")
	require.NoError(t, err)
	m := A2{
		Build:    "build",
		BuildSHA: "buildsha",
		Packages: []habpkg.HabPkg{
			habpkg.NewFQ("origin", "name", "0.1.0", "20180119115433"),
		},
		HartOverrides: []habpkg.Hart{
			hart,
		},
	}

	data, err := json.MarshalIndent(&m, "", "    ")
	require.NoError(t, err)
	fmt.Printf("%s\n", string(data))

	unmarshalled := A2{}
	err = json.Unmarshal(data, &unmarshalled)
	require.NoError(t, err)

	assert.Equal(t, m, unmarshalled)

}
