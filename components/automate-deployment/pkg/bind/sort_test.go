package bind

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

func TestTopoSortAll(t *testing.T) {
	testPackages := []habpkg.HabPkg{
		habpkg.New("chef", "test-service-1"),
		habpkg.New("chef", "test-service-2"),
		habpkg.New("chef", "test-service-3"),
		habpkg.New("chef", "test-service-4"),
		habpkg.New("chef", "test-service-5"),
		habpkg.New("chef", "test-service-6"),
		habpkg.New("chef", "test-service-7"),
		habpkg.New("chef", "test-service-8"),
		habpkg.New("chef", "test-service-9"),
		habpkg.New("chef", "test-service-10"),
	}

	t.Run("detects cycles", func(t *testing.T) {
		binds := Binds{
			Optional: Bindmap{
				"test-service-1": []string{"test-service-2"},
				"test-service-2": []string{"test-service-3"},
				"test-service-3": []string{"test-service-1"},
			}}
		_, err := TopoSortAll(testPackages, binds)
		assert.Error(t, err)
	})

	t.Run("sorts according to binds", func(t *testing.T) {
		binds := Binds{
			Optional: Bindmap{
				"test-service-1": []string{"test-service-2"},
				"test-service-2": []string{"test-service-3"},
				"test-service-3": []string{"test-service-4"},
			}}
		sorted, err := TopoSortAll(testPackages, binds)
		require.NoError(t, err)

		var idxService1 int
		var idxService2 int
		var idxService3 int
		var idxService4 int
		for i, pkg := range sorted {
			if pkg.Name() == "test-service-1" {
				idxService1 = i
			}
			if pkg.Name() == "test-service-2" {
				idxService2 = i
			}
			if pkg.Name() == "test-service-3" {
				idxService3 = i
			}
			if pkg.Name() == "test-service-4" {
				idxService4 = i
			}

		}

		assert.True(t, idxService4 < idxService3)
		assert.True(t, idxService3 < idxService2)
		assert.True(t, idxService2 < idxService1)
	})
}

func TestTopoSort(t *testing.T) {
	testPackages := []habpkg.HabPkg{
		habpkg.New("chef", "test-service-1"),
		habpkg.New("chef", "test-service-2"),
		habpkg.New("chef", "test-service-3"),
		habpkg.New("chef", "test-service-4"),
		habpkg.New("chef", "test-service-5"),
		habpkg.New("chef", "test-service-6"),
		habpkg.New("chef", "test-service-7"),
		habpkg.New("chef", "test-service-8"),
		habpkg.New("chef", "test-service-9"),
		habpkg.New("chef", "test-service-10"),
	}

	t.Run("detects cycles", func(t *testing.T) {
		binds := Binds{
			Optional: Bindmap{
				"test-service-1": []string{"test-service-2"},
				"test-service-2": []string{"test-service-3"},
				"test-service-3": []string{"test-service-1"},
			}}
		_, err := TopoSort([]string{"wut/test-service-2"}, testPackages, binds)
		assert.Error(t, err)
	})

	t.Run("sorts according to binds", func(t *testing.T) {
		binds := Binds{
			Optional: Bindmap{
				"test-service-1": []string{"test-service-2"},
				"test-service-2": []string{"test-service-3"},
				"test-service-3": []string{"test-service-4"},
			}}
		sorted, err := TopoSort([]string{"wut/test-service-1"}, testPackages, binds)
		require.NoError(t, err)

		var idxService1 int
		var idxService2 int
		var idxService3 int
		var idxService4 int
		for i, pkg := range sorted {
			if pkg.Name() == "test-service-1" {
				idxService1 = i
			}
			if pkg.Name() == "test-service-2" {
				idxService2 = i
			}
			if pkg.Name() == "test-service-3" {
				idxService3 = i
			}
			if pkg.Name() == "test-service-4" {
				idxService4 = i
			}

		}

		// It should only return the services reachable from test-service-1
		assert.Equal(t, 4, len(sorted))
		assert.True(t, idxService4 < idxService3)
		assert.True(t, idxService3 < idxService2)
		assert.True(t, idxService2 < idxService1)
	})
}

func TestRDeps(t *testing.T) {
	testPackages := []habpkg.HabPkg{
		habpkg.New("chef", "test-service-1"),
		habpkg.New("chef", "test-service-2"),
		habpkg.New("chef", "test-service-3"),
		habpkg.New("chef", "test-service-4"),
		habpkg.New("chef", "test-service-5"),
		habpkg.New("chef", "test-service-6"),
		habpkg.New("chef", "test-service-7"),
		habpkg.New("chef", "test-service-8"),
		habpkg.New("chef", "test-service-9"),
		habpkg.New("chef", "test-service-10"),
	}

	t.Run("detects cycles", func(t *testing.T) {
		binds := Binds{
			Optional: Bindmap{
				"test-service-1": []string{"test-service-2"},
				"test-service-2": []string{"test-service-3"},
				"test-service-3": []string{"test-service-1"},
			}}
		_, err := RDeps([]string{"wut/test-service-2"}, testPackages, binds)
		assert.Error(t, err)
	})

	t.Run("sorts according to reverse binds", func(t *testing.T) {
		binds := Binds{
			Optional: Bindmap{
				"test-service-1": []string{"test-service-2"},
				"test-service-2": []string{"test-service-3"},
				"test-service-3": []string{"test-service-4"},
			}}
		sorted, err := RDeps([]string{"wut/test-service-4"}, testPackages, binds)
		require.NoError(t, err)

		var idxService1 int
		var idxService2 int
		var idxService3 int
		var idxService4 int
		for i, pkg := range sorted {
			if pkg.Name() == "test-service-1" {
				idxService1 = i
			}
			if pkg.Name() == "test-service-2" {
				idxService2 = i
			}
			if pkg.Name() == "test-service-3" {
				idxService3 = i
			}
			if pkg.Name() == "test-service-4" {
				idxService4 = i
			}
		}

		// It should only return the services reachable from test-service-1
		assert.Equal(t, 4, len(sorted))
		assert.True(t, idxService1 < idxService2)
		assert.True(t, idxService2 < idxService3)
		assert.True(t, idxService3 < idxService4)
	})
}
