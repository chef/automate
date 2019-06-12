package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"sync"

	"github.com/sirupsen/logrus"
)

type RequestInfo struct {
	Body          string            `json:"body"`
	ContentLength int64             `json:"content_length"`
	ContentType   string            `json:"content_type"`
	Headers       map[string]string `json:"headers"`
}

type EndpointRequests struct {
	RequestCount int           `json:"request_count"`
	Requests     []RequestInfo `json:"requests"`
}

func main() {
	port := 15555

	requests := make(map[string]EndpointRequests)
	mutex := sync.Mutex{}

	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		mutex.Lock()
		defer mutex.Unlock()

		logctx := logrus.WithFields(
			logrus.Fields{
				"method":  r.Method,
				"headers": r.Header,
				"path":    r.URL.EscapedPath(),
			})

		v := requests[r.URL.EscapedPath()]

		if r.Method == "POST" {
			v.RequestCount++
			body, err := ioutil.ReadAll(r.Body)
			if err != nil {
				logrus.WithError(err).Warn("Could not read body")
			}
			headers := make(map[string]string)

			for headerName := range r.Header {
				headers[headerName] = r.Header.Get(headerName)
			}

			requestInfo := RequestInfo{
				ContentLength: r.ContentLength,
				Body:          string(body),
				Headers:       headers,
				ContentType:   r.Header.Get("Content-Type"),
			}

			if v.Requests == nil {
				v.Requests = []RequestInfo{}
			}
			v.Requests = append(v.Requests, requestInfo)
			requests[r.URL.EscapedPath()] = v
			logctx.WithField("requestInfo", requestInfo).Info("POST saved")

			w.Header().Set("Content-Type", "application/javascript")
			w.Write([]byte("{}"))
		} else if r.Method == "GET" {
			logctx.WithField("requests", v).Info("GET request info")
			w.Header().Set("Content-Type", "application/javascript")
			j, err := json.Marshal(v)
			if err != nil {
				logrus.WithError(err).Error("Could not marshal json")
				w.WriteHeader(500)
				return
			}
			w.Write(j)
		} else {
			logrus.Info("Unhandled")
		}

	})
	http.ListenAndServe(fmt.Sprintf(":%d", port), nil)
}
