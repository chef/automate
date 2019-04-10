package deployment

import (
	"fmt"
	"strings"
)

// FormatStatus handles a map of services states and returns a user-meaningful string
func (ss *ServiceStatus) FormatStatus() string {
	s := func(i uint64) string { return fmt.Sprintf("%d", i) }

	svcNameHeader := "Service Name"
	uptimeHeader := "Uptime (s)"
	pidHeader := "PID"

	maxName, maxUptime, maxPID := len(svcNameHeader), len(uptimeHeader), len(pidHeader)
	for _, svc := range ss.Services {
		if len(svc.Name) > maxName {
			maxName = len(svc.Name)
		}
		uptimeDecimalStr := s(svc.Uptime)
		if len(uptimeDecimalStr) > maxUptime {
			maxUptime = len(uptimeDecimalStr)
		}
		PIDDecimalStr := s(svc.Pid)
		if len(PIDDecimalStr) > maxPID {
			maxPID = len(PIDDecimalStr)
		}
	}

	fmtStr := fmt.Sprintf("%%-%ds %%-13s  %%-13s %%-%ds %%-%ds\n", maxName, maxUptime, maxPID)

	b := new(strings.Builder)
	b.WriteString(fmt.Sprintf(fmtStr, svcNameHeader, "Process State", "Health Check", uptimeHeader, pidHeader))
	for _, svc := range ss.FormattedServices() {
		b.WriteString(fmt.Sprintf(fmtStr, svc.Name, svc.State, svc.Health, svc.Uptime, svc.Pid))
	}

	return b.String()
}

// FormattedServiceStatus is a translation of api.ServiceStatus to include
// string formats for non-string types.
type FormattedServiceStatus struct {
	Name   string `json:"name"`
	State  string `json:"state"`
	Health string `json:"health"`
	Uptime string `json:"uptime"`
	Pid    string `json:"pid"`
}

// FormattedServices returns a slice of FormattedServiceStatus objects
func (ss *ServiceStatus) FormattedServices() []FormattedServiceStatus {
	s := func(i uint64) string { return fmt.Sprintf("%d", i) }

	addStatus := func(state *ServiceState) FormattedServiceStatus {
		var stateAndHealth []string

		switch state.State {
		case ServiceState_OK:
			stateAndHealth = []string{"running", "ok"}
		case ServiceState_WARNING:
			stateAndHealth = []string{"running", "warning"}
		case ServiceState_CRITICAL:
			stateAndHealth = []string{"running", "CRITICAL"}
		case ServiceState_UNKNOWN:
			stateAndHealth = []string{"running", "unknown"}
		case ServiceState_DOWN:
			stateAndHealth = []string{"DOWN", "unknown"}
		case ServiceState_CONNECTION_ERROR:
			stateAndHealth = []string{"UNKNOWN", "supervisor connection refused"}
		default:
		}

		return FormattedServiceStatus{
			Name:   state.Name,
			State:  stateAndHealth[0],
			Health: stateAndHealth[1],
			Uptime: s(state.Uptime),
			Pid:    s(state.Pid),
		}
	}

	statuses := []FormattedServiceStatus{}
	for _, svc := range ss.Services {
		statuses = append(statuses, addStatus(svc))
	}

	return statuses
}

// Add - add the state for an individual service
func (ss *ServiceStatus) Add(state *ServiceState) {
	ss.Services = append(ss.Services, state)
}

// AllHealthy - check that all services are healthy
func (ss *ServiceStatus) AllHealthy() bool {
	for _, svc := range ss.Services {
		if svc.State != ServiceState_OK {
			return false
		}
	}
	return true
}

// ServiceHealthy takes a service name and returns true if the service is found
// and is in a healthy state
func (ss *ServiceStatus) ServiceHealthy(name string) bool {
	for _, svc := range ss.Services {
		if svc.GetName() == name {
			return svc.State == ServiceState_OK
		}
	}

	return false
}
