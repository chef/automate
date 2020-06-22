package service

import (
	"net/http"
	"net/http/httptest"
	"testing"
)

const ipAttr = "172.18.2.110"
const macAttr = "0A:B1:4A:DB:01:C5"
const hostAttr = "datafeed.test.com"

func TestAssetCreated(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusAccepted)
	}))
	defer ts.Close()

	client := ts.Client()
	dataClient := DataClient{client: *client}
	notification := datafeedNotification{url: ts.URL}
	err := send(dataClient, notification)
	if err != nil {
		t.Errorf("error got: %s, want nil", err)
	}
}

func TestSendError(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusBadRequest)
	}))
	defer ts.Close()

	client := ts.Client()
	dataClient := DataClient{client: *client}
	notification := datafeedNotification{url: ts.URL}
	err := send(dataClient, notification)
	if err == nil {
		t.Error("error got: nil, wanted an error")
	}
}

func TestGetHostAttributesNil(t *testing.T) {
	attributes := make(map[string]interface{})
	ip, mac, hostname := getHostAttributes(attributes)
	if ip != "" {
		t.Logf("expected empty ip address got: %s", ip)
		t.Fail()
	}
	if mac != "" {
		t.Logf("expected empty mac address got: %s", mac)
		t.Fail()
	}
	if hostname != "" {
		t.Logf("expected empty hostanme got: %s", hostname)
		t.Fail()
	}
}

func TestGetHostAttributesEmpty(t *testing.T) {
	attributes := make(map[string]interface{})
	attributes["ipaddress"] = ""
	attributes["macaddress"] = ""
	attributes["hostname"] = ""
	ip, mac, hostname := getHostAttributes(attributes)
	if ip != "" {
		t.Logf("expected empty ip address got: %s", ip)
		t.Fail()
	}
	if mac != "" {
		t.Logf("expected empty mac address got: %s", mac)
		t.Fail()
	}
	if hostname != "" {
		t.Logf("expected empty hostanme got: %s", hostname)
		t.Fail()
	}
}

func TestGetHostAttributes(t *testing.T) {
	attributes := make(map[string]interface{})
	attributes["ipaddress"] = ipAttr
	attributes["macaddress"] = macAttr
	attributes["hostname"] = hostAttr
	ip, mac, hostname := getHostAttributes(attributes)
	if ip != ipAttr {
		t.Logf("expected empty ip address got: %s", ip)
		t.Fail()
	}
	if mac != macAttr {
		t.Logf("expected empty mac address got: %s", mac)
		t.Fail()
	}
	if hostname != hostAttr {
		t.Logf("expected empty hostname got: %s", hostname)
		t.Fail()
	}
}
