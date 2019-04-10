package bind

import (
	"bufio"
	"bytes"
	"fmt"
	"reflect"
	"sort"
	"strings"

	"github.com/pkg/errors"
)

var (
	// ErrNotEnoughParams is returned from ParseData when the data does
	// not contain the required number of fields to construct a bind
	// argument.
	ErrNotEnoughParams = errors.New("Not enough parameters provided")

	// ErrUnknownBindType is returned from ParseData when the data
	// includes a Bind type other than Required or Optional.
	ErrUnknownBindType = errors.New("Unknown bind type")
)

// Bind represents a bind name and a service group it is bound to
type Bind struct {
	name         string
	serviceGroup string
}

type byName []Bind

func (a byName) Len() int           { return len(a) }
func (a byName) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a byName) Less(i, j int) bool { return a[i].name < a[j].name }

// Collection is a set of binds
type Collection struct {
	Binds []Bind
}

// IsBound returns true if the bind has a service group
func (b Bind) IsBound() bool {
	return b.serviceGroup != ""
}

// Name returns the bind name
func (b Bind) Name() string {
	return b.name
}

// ServiceGroup returns the service group
func (b Bind) ServiceGroup() string {
	return b.serviceGroup
}

// WithBinds creates a set of Binds
func WithBinds(binds ...Bind) Collection {
	sorted := make([]Bind, len(binds))
	copy(sorted, binds)
	sort.Sort(byName(sorted))

	return Collection{Binds: sorted}
}

// Equal compares 2 sets of Binds for equality
func (xs Collection) Equal(ys Collection) bool {
	return reflect.DeepEqual(xs.Binds, ys.Binds)
}

// New returns a Bind serviceGroup can be an empty string to
// represent and unbound bind
func New(name string, serviceGroup string) Bind {
	return Bind{
		name:         name,
		serviceGroup: serviceGroup,
	}
}

// Bindmap maps components to the things they bind to
type Bindmap map[string][]string

// BindModeMap maps components to their binding modes
type BindModeMap map[string]string

// Binds is a type that holds optional and required bindings for
// all components
type Binds struct {
	Optional Bindmap
	Required Bindmap
	BindMode BindModeMap
}

// ReverseBinds holds reverse bind dependencies, ignoring the optional
// vs required distinction.
type ReverseBinds struct {
	revMap Bindmap
}

type BindLookup interface {
	AllForService(string) []string
}

// AllForService returns a list of all binds (both optional and
// non-optional) for a service.
func (b *Binds) AllForService(name string) []string {
	reqBinds, haveReqBinds := b.Required[name]
	optBinds, haveOptBinds := b.Optional[name]
	allForService := []string{}

	if haveReqBinds {
		allForService = append(allForService, reqBinds...)
	}

	if haveOptBinds {
		allForService = append(allForService, optBinds...)
	}

	return allForService
}

func (b *Binds) ModeForService(name string) (string, error) {
	mode, haveMode := b.BindMode[name]
	if haveMode {
		return mode, nil
	}
	return "", errors.Errorf("No binding mode data exists for service %q", name)
}

// Info contains the bind information for a single service
// required for interaction with Habitat. Right now this is lossy and
// we lose required vs optional bind information.
type Info struct {
	// Specs is an array of strings in the form of
	// BIND:SERVICE_GROUP, each representing a bind->service_group
	// map that that can be passed to Habitat.
	Specs []string
	// Mode is a bind mode.  Must be non-empty if len(Specs) > 0
	Mode string
}

// DefaultsForService is a helper function that returns a BindInfo
// struct for the given service.
//
// The returned BindSpecs assume that all binds should be mapped to
// the service of the same name in the default group.
//
// An error is returned if any of the assumptions are violated:
//
// - All services must specify a binding mode
func (b *Binds) DefaultsForService(name string) (Info, error) {
	bindMode, err := b.ModeForService(name)
	if err != nil {
		return Info{}, err
	}

	bindSvcNames := b.AllForService(name)
	if len(bindSvcNames) > 0 && bindMode == "" {
		return Info{}, errors.Errorf("illegal empty bind mode for service %q", name)
	}

	binds := make([]string, len(bindSvcNames))
	for i, bind := range bindSvcNames {
		binds[i] = fmt.Sprintf("%[1]s:%[1]s.default", bind)
	}

	return Info{
		Specs: binds,
		Mode:  bindMode,
	}, nil
}

func (b *Binds) ToReverseBinds() *ReverseBinds {
	ret := &ReverseBinds{
		revMap: make(Bindmap, len(b.Required)+len(b.Optional)),
	}

	for name, binds := range b.Required {
		for _, svc := range binds {
			ret.revMap[svc] = append(ret.revMap[svc], name)
		}
	}

	for name, binds := range b.Optional {
		for _, svc := range binds {
			ret.revMap[svc] = append(ret.revMap[svc], name)
		}
	}

	return ret
}

func (r *ReverseBinds) AllForService(name string) []string {
	return r.revMap[name]
}

// ParseData parses a text description of binds an returns a Binds
// struct.
func ParseData(data []byte) (Binds, error) {
	b := Binds{
		Optional: make(Bindmap),
		Required: make(Bindmap),
		BindMode: make(BindModeMap),
	}

	lineScanner := bufio.NewScanner(bytes.NewReader(data))

	for lineScanner.Scan() {
		line := lineScanner.Text()
		// bindInfo will look like:
		// bindInfo[0]   = The service name that the bindings are for
		// bindInfo[1]   = The type of the binding (REQUIRED or OPTIONAL) or BINDING_MODE
		// bindInfo[2:]  = The services to bind, or the binding mode ("strict" or "relaxed")
		//
		// For example:
		// foo REQUIRED bar baz
		// // means that foo has 2 bindings that required, bar and baz
		// foo BINDING_MODE relaxed
		// // means that foo wants to be started with relaxed binding mode
		bindInfo := strings.Fields(line)

		if len(bindInfo) == 0 {
			continue
		} else if len(bindInfo) < 3 {
			return b, ErrNotEnoughParams
		}

		switch bindInfo[1] {
		case "REQUIRED":
			b.Required.readBinds(bindInfo[0], bindInfo[2:])
		case "OPTIONAL":
			b.Optional.readBinds(bindInfo[0], bindInfo[2:])
		case "BINDING_MODE":
			if err := b.BindMode.readBindMode(bindInfo[0], bindInfo[2]); err != nil {
				return b, err
			}
		default:
			return b, ErrUnknownBindType
		}
	}

	if err := lineScanner.Err(); err != nil {
		return b, errors.Wrap(err, "Scanner failed")
	}

	return b, nil
}

func (b BindModeMap) readBindMode(component string, mode string) error {
	switch mode {
	case "strict", "relaxed":
		b[component] = mode
	default:
		return errors.Errorf("Unknown binding mode \"%s\"; valid values are \"strict\" or \"relaxed\"", mode)
	}
	return nil
}

func (b Bindmap) readBinds(component string, bindList []string) {
	b[component] = append(b[component], bindList...)
}
