package projectassignment

import (
	"math/rand"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCalculateProjectsToAuthorizeForUpdate(t *testing.T) {
	isUpdate := true
	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"unassigned to unassigned", func(t *testing.T) {
			assert.ElementsMatch(t, []string{}, CalculateProjectsToAuthorize([]string{}, []string{}, isUpdate))
		}},
		{"there are only new projects", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "b", "(unassigned)"}, CalculateProjectsToAuthorize([]string{}, []string{"a", "b"}, isUpdate))
		}},
		{"no projects were removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{}, CalculateProjectsToAuthorize([]string{"a", "b"}, []string{"a", "b"}, isUpdate))
		}},
		{"there are only old projects", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "b", "(unassigned)"}, CalculateProjectsToAuthorize([]string{"a", "b"}, []string{}, isUpdate))
		}},
		{"one project is removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a"}, CalculateProjectsToAuthorize([]string{"a", "b"}, []string{"b"}, isUpdate))
		}},
		{"multiple projects are removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "c"}, CalculateProjectsToAuthorize([]string{"a", "b", "c"}, []string{"b"}, isUpdate))
		}},
		{"one project is added", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"c"}, CalculateProjectsToAuthorize([]string{"a", "b"}, []string{"a", "b", "c"}, isUpdate))
		}},
		{"multiple projects are added", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "d"}, CalculateProjectsToAuthorize([]string{"b", "c"}, []string{"a", "b", "c", "d"}, isUpdate))
		}},
		{"multiple projects are added and removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "c", "d"}, CalculateProjectsToAuthorize([]string{"b", "c"}, []string{"a", "b", "d"}, isUpdate))
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
	}
}

func TestCalculateProjectsToAuthorizeForCreate(t *testing.T) {
	isUpdate := false
	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"unassigned to unassigned", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"(unassigned)"}, CalculateProjectsToAuthorize([]string{}, []string{}, isUpdate))
		}},
		{"there are only new projects", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "b", "(unassigned)"}, CalculateProjectsToAuthorize([]string{}, []string{"a", "b"}, isUpdate))
		}},
		{"no projects were removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{}, CalculateProjectsToAuthorize([]string{"a", "b"}, []string{"a", "b"}, isUpdate))
		}},
		{"there are only old projects", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "b", "(unassigned)"}, CalculateProjectsToAuthorize([]string{"a", "b"}, []string{}, isUpdate))
		}},
		{"one project is removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a"}, CalculateProjectsToAuthorize([]string{"a", "b"}, []string{"b"}, isUpdate))
		}},
		{"multiple projects are removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "c"}, CalculateProjectsToAuthorize([]string{"a", "b", "c"}, []string{"b"}, isUpdate))
		}},
		{"one project is added", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"c"}, CalculateProjectsToAuthorize([]string{"a", "b"}, []string{"a", "b", "c"}, isUpdate))
		}},
		{"multiple projects are added", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "d"}, CalculateProjectsToAuthorize([]string{"b", "c"}, []string{"a", "b", "c", "d"}, isUpdate))
		}},
		{"multiple projects are added and removed", func(t *testing.T) {
			assert.ElementsMatch(t, []string{"a", "c", "d"}, CalculateProjectsToAuthorize([]string{"b", "c"}, []string{"a", "b", "d"}, isUpdate))
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
	}
}
