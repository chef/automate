package service

import (
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestAssetCreated(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusCreated)
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
