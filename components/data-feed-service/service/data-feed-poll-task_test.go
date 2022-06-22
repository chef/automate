package service

import (
	"strings"
	"testing"

	"github.com/chef/automate/api/interservice/compliance/reporting"
	"github.com/chef/automate/components/data-feed-service/config"
)

const (
	reportNodeId = "e5a3d952-5dfc-3215-bfe4-541eb0b5d345"
)

func TestEmptyFilters(t *testing.T) {
	serviceConfig := config.ServiceConfig{DisableCIDRFilter: false, CIDRFilter: ""}
	config := &config.DataFeedConfig{ServiceConfig: serviceConfig}
	ipNets, err := parseCIDRFilters(config)
	if err == nil {
		t.Error("expected error but got nil")
	}
	if ipNets != nil {
		t.Errorf("Expected nil got %s", ipNets)
	}
	t.Log("TestEmptyFilters OK")
}

func TestBadFilter(t *testing.T) {
	serviceConfig := config.ServiceConfig{DisableCIDRFilter: false, CIDRFilter: "bad filter"}
	config := &config.DataFeedConfig{ServiceConfig: serviceConfig}
	ipNets, err := parseCIDRFilters(config)
	if err == nil {
		t.Error("expected error but got nil")
	}
	if ipNets != nil {
		t.Errorf("Expected nil got %s", ipNets)
	}
	t.Log("TestBadFilter OK")
}

func TestBadFilters(t *testing.T) {
	serviceConfig := config.ServiceConfig{DisableCIDRFilter: false, CIDRFilter: "10.0.0.0,bad filter,another"}
	config := &config.DataFeedConfig{ServiceConfig: serviceConfig}
	ipNets, err := parseCIDRFilters(config)
	if err == nil {
		t.Error("expected error but got nil")
	}
	if ipNets != nil {
		t.Errorf("Expected nil got %s", ipNets)
	}
	t.Log("TestBadFilters OK")
}

func TestDefaultFilter(t *testing.T) {
	serviceConfig := config.ServiceConfig{DisableCIDRFilter: false, CIDRFilter: "0.0.0.0/0"}
	config := &config.DataFeedConfig{ServiceConfig: serviceConfig}
	ipNets, err := parseCIDRFilters(config)
	if err != nil {
		t.Errorf("expected nil but got %s", err)
	}
	if ipNets == nil {
		t.Error("Expected ipnet got nil")
	} else if _, ok := ipNets["0.0.0.0/0"]; !ok {
		t.Error("Expected 0.0.0.0/0 to be in the ipNets")
	}
	t.Log("TestDefaultFilter OK")
}

func TestInvalidFilter(t *testing.T) {
	serviceConfig := config.ServiceConfig{DisableCIDRFilter: false, CIDRFilter: "10.0.0.0/50"}
	config := &config.DataFeedConfig{ServiceConfig: serviceConfig}
	ipNets, err := parseCIDRFilters(config)
	if err == nil {
		t.Error("expected error but got nil")
	}
	if ipNets != nil {
		t.Errorf("Expected nil got %s", ipNets)
	}
	t.Log("TestInvalidFilter OK")
}

func TestValidFilter(t *testing.T) {
	serviceConfig := config.ServiceConfig{DisableCIDRFilter: false, CIDRFilter: "10.0.0.0/10"}
	config := &config.DataFeedConfig{ServiceConfig: serviceConfig}
	ipNets, err := parseCIDRFilters(config)
	if err != nil {
		t.Errorf("expected nil but got %s", err)
	}
	if ipNets == nil {
		t.Error("Expected ipnet got nil")
	} else if _, ok := ipNets["10.0.0.0/10"]; !ok {
		t.Error("Expected 10.0.0.0/10 to be in the ipNets")
	}
	t.Log("TestValidFilter OK")
}

func TestValidAndInvalidFilters(t *testing.T) {
	serviceConfig := config.ServiceConfig{DisableCIDRFilter: false, CIDRFilter: "10.0.0.0/10,10.0.0.0/50"}
	config := &config.DataFeedConfig{ServiceConfig: serviceConfig}
	ipNets, err := parseCIDRFilters(config)
	if err == nil {
		t.Error("expected error but got nil")
	}
	if ipNets != nil {
		t.Errorf("Expected nil got %s", ipNets)
	}
	t.Log("TestValidAndInvalidFilters OK")
}

func TestValidFilters(t *testing.T) {
	filters := "10.0.0.0/10,192.168.0.0/24,172.18.2.0/32"
	filterArray := strings.Split(filters, ",")
	serviceConfig := config.ServiceConfig{DisableCIDRFilter: false, CIDRFilter: filters}
	config := &config.DataFeedConfig{ServiceConfig: serviceConfig}
	ipNets, err := parseCIDRFilters(config)
	if err != nil {
		t.Errorf("expected nil but got %s", err)
	}
	if ipNets == nil {
		t.Error("Expected ipnet got nil")
	} else {
		for filter := range filterArray {
			if value, ok := ipNets[filterArray[filter]]; !ok {
				t.Errorf("Expected %s to be in the ipNets", value)
			}
		}
	}
	t.Log("TestValidFilters OK")
}

func TestIncludeIPAddressWhenDisableTrue(t *testing.T) {
	pollTask := &DataFeedPollTask{disableCIDRFilter: true}
	included := pollTask.includeNode("anything")
	if !included {
		t.Error("Expected true")
	}
	t.Log("TestIncludeIPAddressWhenDisableTrue OK")
}

func TestResolveResourceIdNodeId(t *testing.T) {
	report := &reporting.ReportSummaryLevelOne{NodeId: reportNodeId}
	resourceId := resolveResourceId(report)
	if resourceId != reportNodeId {
		t.Logf("expected %s, got: %s", reportNodeId, resourceId)
		t.Fail()
	}
}

func TestResolveResourceIdIPAddress(t *testing.T) {
	report := &reporting.ReportSummaryLevelOne{NodeId: reportNodeId, Ipaddress: ipAttr}
	resourceId := resolveResourceId(report)
	if resourceId != ipAttr {
		t.Logf("expected %s, got: %s", ipAttr, resourceId)
		t.Fail()
	}
}
