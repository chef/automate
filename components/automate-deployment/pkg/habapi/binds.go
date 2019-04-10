package habapi

import (
	"context"
	"fmt"
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

// AllConsumingServices returns a list of Identifiables for all of the
// loaded services that are bound to the given service. This assumes
// that the service group is always default.
func AllConsumingServices(c *Client, pkg habpkg.VersionedPackage) ([]habpkg.HabPkg, error) {
	svcMap := map[string]habpkg.HabPkg{}
	loadedServices, err := c.ListServices(context.TODO())
	if err != nil {
		return nil, errors.Wrap(err, "failed to list Habitat services")
	}

	expectedBind := fmt.Sprintf("%s.default", pkg.Name())
	for _, svc := range loadedServices {
		for _, b := range svc.Binds {
			parts := strings.SplitN(b, ":", 2)
			if len(parts) != 2 {
				return nil, errors.Errorf("Unable to parse bind data: %s", b)
			}

			if parts[1] == expectedBind {
				// Assume we only want 1 habpkg per name
				svcMap[svc.Pkg.Name] = habpkg.NewFQ(svc.Pkg.Origin, svc.Pkg.Name, svc.Pkg.Version, svc.Pkg.Release)
			}
		}
	}

	ret := make([]habpkg.HabPkg, 0, len(svcMap))
	for _, pkg := range svcMap {
		ret = append(ret, pkg)
	}
	return ret, nil
}
