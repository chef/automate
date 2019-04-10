package bind

import (
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

// A small 'ordered set'. This is used to track our "seen nodes". We
// could have just used either a slice or a map, but using both allows
// us to have easy lookup while being able to recover ordering, which
// is nice for printing errors.
type set struct {
	members   []string
	memberMap map[string]bool
}

func newSet(size int) *set {
	return &set{
		members:   make([]string, 0, size),
		memberMap: make(map[string]bool),
	}
}

func (s *set) Add(item string) bool {
	_, found := s.memberMap[item]
	if found {
		return false
	}

	s.members = append(s.members, item)
	s.memberMap[item] = true
	return true
}

func (s *set) Dup() *set {
	ret := newSet(len(s.members))
	for _, m := range s.members {
		ret.Add(m)
	}
	return ret
}

// TopoSortAll topologically sorts the slice of service packages based
// on the given binds data. It returns a slice of sorted HabPkgs.  If
// error is non-nil there is a dependency cycle.
//
// The service list and dependency information is passed separately
// rather than using a more standard adjacency list representation
// simply to save on data-munging (services and binds are parsed
// separately currently)
//
// This uses a "depth-first" approach. It uses more memory than it
// needs to to facilitate nice errors and a bit of programming ease.
func TopoSortAll(services []habpkg.HabPkg, binds Binds) ([]habpkg.HabPkg, error) {
	// The service map serves double-duty. It both gives us easy
	// lookup-by-name of HabPkg's and keeps track of our
	// "permanent marks".
	serviceMap := makeServiceMap(services)
	return visitAll(services, serviceMap, &binds)
}

// TopoSort is like TopoSortAll but only returns the nodes in
// allServices reachable from the nodes identified in services.
func TopoSort(serviceList []string, allServices []habpkg.HabPkg, binds Binds) ([]habpkg.HabPkg, error) {
	services := make([]habpkg.HabPkg, len(serviceList))
	for i, s := range serviceList {
		pkg, err := habpkg.FromString(s)
		if err != nil {
			return []habpkg.HabPkg{}, err
		}
		services[i] = pkg
	}

	serviceMap := makeServiceMap(allServices)
	return visitAll(services, serviceMap, &binds)
}

// RDeps returns all of the reverse dependencies of the given packages.
func RDeps(serviceList []string, allServices []habpkg.HabPkg, binds Binds) ([]habpkg.HabPkg, error) {
	services := make([]habpkg.HabPkg, len(serviceList))
	for i, s := range serviceList {
		pkg, err := habpkg.FromString(s)
		if err != nil {
			return []habpkg.HabPkg{}, err
		}
		services[i] = pkg
	}

	revBinds := binds.ToReverseBinds()
	serviceMap := makeServiceMap(allServices)
	return visitAll(services, serviceMap, revBinds)
}

func makeServiceMap(serviceSlice []habpkg.HabPkg) map[string]habpkg.HabPkg {
	serviceMap := make(map[string]habpkg.HabPkg)
	for _, svc := range serviceSlice {
		serviceMap[svc.Name()] = svc
	}
	return serviceMap
}

func visitAll(services []habpkg.HabPkg, serviceMap map[string]habpkg.HabPkg, binds BindLookup) ([]habpkg.HabPkg, error) {
	result := make([]habpkg.HabPkg, 0, len(services))
	for _, s := range services {
		new, err := visit(s.Name(), serviceMap, binds, nil)
		if err != nil {
			return result, err
		}
		result = append(result, new...)
	}

	return result, nil
}

func visit(svcName string, serviceMap map[string]habpkg.HabPkg, binds BindLookup, tmarks *set) ([]habpkg.HabPkg, error) {
	results := make([]habpkg.HabPkg, 0, 3)

	// If we can't find the node, it means we have already visited
	// it.
	svc, found := serviceMap[svcName]
	if !found {
		return results, nil
	}

	if tmarks == nil {
		tmarks = newSet(2)
	}

	if !tmarks.Add(svc.Name()) {
		// If we weren't added it means we were seen before, return err with cycle.
		return results, errors.Errorf("service bind cycle found: %s -> %s",
			strings.Join(tmarks.members, " -> "), svc.Name())
	}

	for _, boundSvc := range binds.AllForService(svc.Name()) {
		localResults, err := visit(boundSvc, serviceMap, binds, tmarks.Dup())
		if err != nil {
			return results, err
		}
		results = append(results, localResults...)
	}

	delete(serviceMap, svcName)
	results = append(results, svc)
	return results, nil
}
