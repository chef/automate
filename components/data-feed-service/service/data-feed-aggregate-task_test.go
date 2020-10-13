package service

import (
	"context"
	"testing"
)

const (
	ipAddress = "192.168.10.10"
	nodeId    = "e5a3d952-5dfc-3215-bfe4-541eb0b5d345"
)

func TestIsIpaddressTrue(t *testing.T) {
	isIPAddress := isIPAddress(ipAddress)
	if !isIPAddress {
		t.Log("Expected true got false")
		t.Fail()
	}
}

func TestIsIpaddressFalse(t *testing.T) {
	isIPAddress := isIPAddress(nodeId)
	if isIPAddress {
		t.Log("Expected false got true")
		t.Fail()
	}
}

func TestSafeGetNodeDataNoIPFilter(t *testing.T) {
	aggregateTask := NewDataFeedAggregateTask(mockConfig, nil, nil, nil, nil)
	var filters []string
	nodeData, err := aggregateTask.safeGetNodeData(context.Background(), filters)
	if nodeData == nil || len(nodeData) != 0 {
		t.Logf("Expected empty nodeData, got %v", nodeData)
		t.Fail()
	}
	if err != nil {
		t.Logf("Expected nil, got %v", err)
		t.Fail()
	}
}
