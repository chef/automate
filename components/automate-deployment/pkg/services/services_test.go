// Copyright © 2017 Chef Software

package services

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/bind"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

func TestServicesSorted(t *testing.T) {
	ids, err := ServicesInCollections(productList)
	require.NoError(t, err)

	sortedServices, err := bind.TopoSortAll(ids, AllBinds)
	require.Nil(t, err, "Services are no longer topologically sortable.  Please check any newly added binds")
	assert.Equal(t, sortedServices, ids)
}
func TestServicesSortable(t *testing.T) {
	ids, err := ServicesInCollection("automate-full")
	require.NoError(t, err)

	sortedServices, err := bind.TopoSortAll(ids, AllBinds)
	require.Nil(t, err, "Services are no longer topologically sortable.  Please check any newly added binds")
	assert.Equal(t, len(sortedServices), len(ids))
}

func TestChefServices(t *testing.T) {
	packages, err := ServicesInCollection("automate-full")
	require.NoError(t, err)

	nonChefPackages := make([]string, 0, 2)
	for _, pkg := range packages {
		if pkg.Origin() != "chef" {
			nonChefPackages = append(nonChefPackages, habpkg.Ident(&pkg))
		}
	}

	require.Equalf(t, 0, len(nonChefPackages),
		"services.json can only contain service packages in the chef origin: %s", strings.Join(nonChefPackages, ","))
}

func TestServicePathsByCollection(t *testing.T) {
	automateServiceIDs, err := ServicesInCollection("automate-full")
	assert.NoError(t, err)
	set := make(map[habpkg.HabPkg]struct{}, len(automateServiceIDs))
	for _, sp := range automateServiceIDs {
		set[sp] = struct{}{}
	}

	mustExist := []habpkg.HabPkg{
		habpkg.New("chef", "ingest-service"),
		habpkg.New("chef", "automate-postgresql"),
	}
	for _, expected := range mustExist {
		_, ok := set[expected]
		assert.Equal(t, true, ok)
	}

	// ensure deployment-service is removed from the list
	_, ok := set[habpkg.New("chef", "deployment-service")]
	assert.Equal(t, false, ok)

	// ensure no empty services
	for _, sp := range automateServiceIDs {
		assert.NotEqual(t, habpkg.HabPkg{}, sp)
	}

	csServiceIDs, err := ServicesInCollection("chef-server")
	assert.NoError(t, err)

	csSet := make(map[habpkg.HabPkg]struct{}, len(csServiceIDs))
	for _, sp := range csServiceIDs {
		csSet[sp] = struct{}{}
	}

	mustExist = []habpkg.HabPkg{
		habpkg.New("chef", "automate-cs-bookshelf"),
		habpkg.New("chef", "automate-cs-oc-bifrost"),
	}
	for _, expected := range mustExist {
		_, ok := csSet[expected]
		assert.Equal(t, true, ok)
	}
}

func TestServicePathsForMultiCollection(t *testing.T) {
	serviceIDs, err := ServicesInCollections([]string{"automate-full", "chef-server"})
	assert.NoError(t, err)
	set := make(map[habpkg.HabPkg]struct{}, len(serviceIDs))
	for _, sp := range serviceIDs {
		set[sp] = struct{}{}
	}

	mustExist := []habpkg.HabPkg{
		habpkg.New("chef", "ingest-service"),
		habpkg.New("chef", "automate-postgresql"),
		habpkg.New("chef", "automate-cs-bookshelf"),
		habpkg.New("chef", "automate-cs-oc-bifrost"),
	}
	for _, expected := range mustExist {
		_, ok := set[expected]
		assert.Equal(t, true, ok)
	}

	// ensure deployment-service is removed from the list
	_, ok := set[habpkg.New("chef", "deployment-service")]
	assert.Equal(t, false, ok)

	// ensure no empty services
	for _, sp := range serviceIDs {
		assert.NotEqual(t, habpkg.HabPkg{}, sp)
	}
}

func TestSupplementaryPackages(t *testing.T) {
	packageIDs, err := SupplementaryPackagesInCollection("automate-full")
	assert.NoError(t, err)

	pkgSet := make(map[habpkg.HabPkg]struct{}, len(packageIDs))
	for _, p := range packageIDs {
		pkgSet[p] = struct{}{}
	}

	mustExist := []habpkg.HabPkg{
		habpkg.New("core", "rsync"),
	}

	for _, expected := range mustExist {
		_, ok := pkgSet[expected]
		assert.Equal(t, true, ok)
	}
}

func TestIsDataService(t *testing.T) {
	assert.True(t, IsDataService("automate-postgresql"))
	assert.False(t, IsDataService("compliance-service"))
}

func TestDoLoadServiceBinds(t *testing.T) {
	t.Run("Binds can load from the binary and look reasonable", func(t *testing.T) {
		b := loadServiceBinds()
		// We have at least one of each type
		assert.True(t, len(b.Required) > 0)
		assert.True(t, len(b.Optional) > 0)

		// Each component listed
		for _, bmap := range []bind.Bindmap{b.Required, b.Optional} {
			for component, svcs := range bmap {
				// Each component has to have a name
				assert.True(t, strings.TrimSpace(component) != "")
				// Each listed component has to have at least one bind
				assert.True(t, len(svcs) > 0)

				for _, s := range svcs {
					assert.True(t, strings.TrimSpace(s) != "")
				}
			}
		}
	})
}

func TestBinlinksLoad(t *testing.T) {
	t.Run("a service with no binlinks has an empty list of binlinks", func(t *testing.T) {
		var nilSlice []string
		assert.Equal(t, nilSlice, BinlinksForPackage("compliance-service"))
	})

	t.Run("automate-cli has a binlink entry for chef-automate exe", func(t *testing.T) {
		assert.Equal(t, []string{"chef-automate"}, BinlinksForPackage("automate-cli"))
	})
	t.Run("automate-cs-nginx has a binlink entry for knife", func(t *testing.T) {
		assert.Equal(t, []string{"knife", "chef-server-ctl"}, BinlinksForPackage("automate-cs-nginx"))
	})
}

func TestAllPackagesUniq(t *testing.T) {
	packageIDs, err := ServicesInCollections([]string{"automate-full", "chef-server", "workflow"})
	require.NoError(t, err)
	assert.True(t, len(packageIDs) > 0)

	pkgSet := make(map[habpkg.HabPkg]struct{})
	for _, p := range packageIDs {
		pkgSet[p] = struct{}{}
	}
	assert.Equal(t, len(packageIDs), len(pkgSet))
}

func TestServicesUniq(t *testing.T) {
	packageIDs, err := ServicesInCollections([]string{"automate-full", "core"})
	require.NoError(t, err)
	assert.True(t, len(packageIDs) > 0)

	pkgSet := make(map[habpkg.HabPkg]struct{})
	for _, p := range packageIDs {
		pkgSet[p] = struct{}{}
	}
	assert.Equal(t, len(packageIDs), len(pkgSet))
}

func TestServicesUniqWithAliases(t *testing.T) {
	packageIDs, err := ServicesInCollections([]string{"automate-full", "automate"})
	require.NoError(t, err)
	assert.True(t, len(packageIDs) > 0)

	pkgSet := make(map[habpkg.HabPkg]struct{})
	for _, p := range packageIDs {
		pkgSet[p] = struct{}{}
	}
	assert.Equal(t, len(packageIDs), len(pkgSet))
}

func TestAutomateFullAliases(t *testing.T) {
	packageIDsAutomateFull, err := ServicesInCollections([]string{"automate-full"})
	require.NoError(t, err)
	packageIDsAutomate, err := ServicesInCollections([]string{"automate"})
	require.NoError(t, err)
	assert.Equal(t, packageIDsAutomate, packageIDsAutomateFull)
}

func TestListProducts(t *testing.T) {
	assert.Subset(t, ListProducts(), []string{"automate", "chef-server", "workflow"})
	assert.NotContains(t, ListProducts(), "monitoring")
}

func TestValidateProductDeployment(t *testing.T) {
	t.Run("error unknown product", func(t *testing.T) {
		assert.Error(t, ValidateProductDeployment([]string{"foo"}))
		assert.Error(t, ValidateProductDeployment([]string{"automate", "foo", "bar"}))
	})
	t.Run("error on base collection", func(t *testing.T) {
		assert.Error(t, ValidateProductDeployment([]string{"automate", "core"}))
	})
	t.Run("error unspecified product dependencies", func(t *testing.T) {
		assert.Error(t, ValidateProductDeployment([]string{"workflow"}))
	})
	t.Run("specified product dependencies", func(t *testing.T) {
		assert.NoError(t, ValidateProductDeployment([]string{"automate", "workflow"}))
		assert.NoError(t, ValidateProductDeployment([]string{"automate-full", "workflow"}))
		assert.NoError(t, ValidateProductDeployment([]string{"automate", "workflow"}))
	})
	t.Run("we support these for sure", func(t *testing.T) {
		assert.NoError(t, ValidateProductDeployment([]string{"automate", "monitoring"}))
		assert.NoError(t, ValidateProductDeployment([]string{"automate", "workflow"}))
		assert.NoError(t, ValidateProductDeployment([]string{"automate", "workflow", "chef-server"}))
		assert.NoError(t, ValidateProductDeployment([]string{"automate", "chef-server"}))
		assert.NoError(t, ValidateProductDeployment([]string{"chef-server"}))
		assert.NoError(t, ValidateProductDeployment([]string{"automate-full", "workflow"}))
		assert.NoError(t, ValidateProductDeployment([]string{"automate-full", "workflow", "chef-server"}))
		assert.NoError(t, ValidateProductDeployment([]string{"automate-full", "chef-server"}))
	})
}

func TestContainsCollection(t *testing.T) {
	t.Run("returns false if desired collection is unknown", func(t *testing.T) {
		assert.False(t, ContainsCollection("asdf", []string{"automate", "chef-server"}))
	})
	t.Run("returns false if desired collection is not in the list", func(t *testing.T) {
		assert.False(t, ContainsCollection("workflow", []string{"automate", "chef-server"}))
	})
	t.Run("returns false if desired collection is not in the list and not a base dependency", func(t *testing.T) {
		assert.False(t, ContainsCollection("automate", []string{"workflow"}))
	})
	t.Run("returns false list is empty", func(t *testing.T) {
		assert.False(t, ContainsCollection("core", []string{}))
		assert.False(t, ContainsCollection("core", nil))
	})
	t.Run("returns true when product is in the list", func(t *testing.T) {
		assert.True(t, ContainsCollection("workflow", []string{"workflow"}))
		assert.True(t, ContainsCollection("automate", []string{"automate"}))
		assert.True(t, ContainsCollection("automate-full", []string{"automate"}))
		assert.True(t, ContainsCollection("automate-full", []string{"automate-full"}))
		assert.True(t, ContainsCollection("automate", []string{"automate-full"}))
	})
	t.Run("returns true if a base component is implicitly included", func(t *testing.T) {
		assert.True(t, ContainsCollection("core", []string{"automate"}))
		assert.True(t, ContainsCollection("core", []string{"automate-full"}))
	})
}
