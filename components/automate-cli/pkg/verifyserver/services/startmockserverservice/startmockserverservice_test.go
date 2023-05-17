package startmockserverservice_test

import (
	"crypto/tls"
	"fmt"
	"io/ioutil"
	"net"
	"net/http"
	"strings"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/startmockserverservice"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

const (
	SERVER_KEY  = "-----BEGIN RSA PRIVATE KEY-----\nMIIEogIBAAKCAQEAr4V0UWr7B4Hu7gTRDYt7FGEBFXC6V39Cf8i4xgtUYZOKkvt/\nMXiYegFWgZPLah0TyhgdyjLpC8FKdpq+FHpksGgfkR8ARyBsp50b4FRfjCSpKNj+\nSpUhHzovYSywLJFykT9Wq00n/9M992Bxn94MJQIfmHfWUrpOl66o0fdv8viblik7\nEh+sPl+Lgg6miTg8ev529e0Lo//LrW5AtHHEEeBvBPWp1kwnWgatECYDZgXQAbvI\nYzNkqpYyE853hbCBzZ1HKsz4WK0poGg3MSl/AbJC36xi1ecP3rCnh5ae123p1V9d\n337YUDxL7Wst9wKulj+uVPgxNchg11JqgNNaawIDAQABAoIBAEV4SbCL6i1vhPTa\nHTACO8W2Gyq0QlytNtHCzTc9drlkHx3Lwuz+sULg0q9YotMuDQ4Y+3lzKwAHEgTd\nfEw4oS+dFplmrsJ4F+lDaqwgWOzr+bP8JrG4UrK8YdJRUK1jJ/hLHG+SizlbD5Sg\nrHg894uSSpUbIU3/BWpNq+3mxH1itofFanP7ePTCV78kpfGHR2Ok2UMHxyQHSk7y\nzii78grmbQObndx2b7bC7cswCa3/pjMN3X6B/Jjy/gn3b50RR77YFqpp7zzrnrZA\nSgx6CrJSBRwDExhCzMPBh3UjtYgohPHlVLjPluUSZheYDZQB1Gr8HwN3iL1dmf1w\n8QYQnCkCgYEA4GgAReXoOzjbjo06lr8TGogavWB2D3Zls3WUGO9FcD/59IJTi1rC\nh13V7gZYRFdc7HH660vqtfqsYtt5akV0FxYfXAMla9fuYxmr7h8OUfs8jTp77Uq8\ntSlQ3VPaas4YTzTReTD+wHvKVQ6uiDAYqjTqF9yJNkMcZ8veNzWtTG8CgYEAyDuN\nnNGY6VD2p5UA24y01yZqNH9WYAwuIYNAqCypZLzJBP8ABVrCOUNuPIaBCxmU8Orq\nYN9FVexY3nBdkDEjEwbXvnYyydoadIN9DMB3YhQ9etf3DuZFI65+DAskHE/cJrpP\neUM/XPpC73pf3U+eqSZwV+7FX8idgsK6Rxnnh8UCgYAyTN2SzV/qtmnwYBO76oR7\ns/pabJ7KBH3zZe2WUTu9V3nNptDXMbbc5NmpCt8KIpL/pOTbjR7FP7UYS53BhmPp\nMNpCo6nlrHcQ25ZAP9HT6n6+IVfZ7qCx8trfYYZZ3mxwhKRXh/Xya00FF89jU3ST\n4lx+kL5o3U4mrfnXYj7AHQKBgC5vERIS0SEaM3j9ZuuDH9TdBbgS55byfCgtZesa\nIFZKKVvNPtX/DBd3ebLzhi1qy01rTNsWK+AXJSzAZhIwMvAQoCt9AZ4pxATNEUzJ\nvWWzR+aa+qIr6FC0AGsOkls2cdlRT2jRnXoUVz1t5ZlPA346eccKih8CSPSv777Z\nVQX5AoGAf4rHCoMGGqlJZxaroCS2AgDtzq5Za9XdwT3RczYnJGCkv7nK4K0e4zxB\nqMxAiUTrzae1Hpf5OV0GXeoiN0taZ9L2Cc56OlaqPRL2BDpYU2K+3okrftXLgFCX\nClDVL9XxSbVT99AUEBw9eH9hYFVKjHIEyJ20Udw0zsR+qU4R6/0=\n-----END RSA PRIVATE KEY-----"
	SERVER_CERT = "-----BEGIN CERTIFICATE-----\nMIIDTTCCAjWgAwIBAgIUNidKDNanRMXILhrf1//SuK7DC3UwDQYJKoZIhvcNAQEL\nBQAwYzELMAkGA1UEBhMCVVMxEzARBgNVBAgMCldhc2hpbmd0b24xEDAOBgNVBAcM\nB1NlYXR0bGUxGjAYBgNVBAoMEUNoZWYgU29mdHdhcmUgSW5jMREwDwYDVQQDDAhw\ncm9ncmVzczAeFw0yMzA1MDkxMjAwMjZaFw0yNjA1MDgxMjAwMjZaMDYxCzAJBgNV\nBAYTAlVTMRMwEQYDVQQIDApDYWxpZm9ybmlhMRIwEAYDVQQDDAlsb2NhbGhvc3Qw\nggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCvhXRRavsHge7uBNENi3sU\nYQEVcLpXf0J/yLjGC1Rhk4qS+38xeJh6AVaBk8tqHRPKGB3KMukLwUp2mr4UemSw\naB+RHwBHIGynnRvgVF+MJKko2P5KlSEfOi9hLLAskXKRP1arTSf/0z33YHGf3gwl\nAh+Yd9ZSuk6XrqjR92/y+JuWKTsSH6w+X4uCDqaJODx6/nb17Quj/8utbkC0ccQR\n4G8E9anWTCdaBq0QJgNmBdABu8hjM2SqljITzneFsIHNnUcqzPhYrSmgaDcxKX8B\nskLfrGLV5w/esKeHlp7XbenVX13ffthQPEvtay33Aq6WP65U+DE1yGDXUmqA01pr\nAgMBAAGjJjAkMCIGA1UdEQQbMBmCCWxvY2FsaG9zdIIMaHR0cHMtc2VydmVyMA0G\nCSqGSIb3DQEBCwUAA4IBAQCxXspmv+BCRVFykb80embuZGCXMh7YmH3j5dJZhaKL\n/PPcUjgJTYRanDSSwt5IFyyYwiYG9qdUrRLxR5pgpdj0vNRaLdabG24UsVQQuK1Y\nrxb/6HF2AwWASiS5YLKoVMwg1sYiskpA7gJ23Xe34BVckqAd+Yoss4zDNR3d/yRM\nQYbnr/STpW4c+7jHL+vlpu/OdHwEtsTNrUG6lk0YO1lGH43a0rmvMzYOCgZEfr3h\nK4zbwy053Vq8PGIH24/bNu67pSfslgGI30bN9PmUdFMwEFuRC7rCFgQ8LpRbJNf1\nf5CUhHWn2nC05XOzKm+Kj/NHPtw5iJkrQvLNtsdiO92O\n-----END CERTIFICATE-----"
)

func TestStartMockServer(t *testing.T) {

	log, err := logger.NewLogger("text", "debug")
	assert.NoError(t, err)
	PORT, err := GetFreePort()
	t.Run("Start TCP server and check response", func(t *testing.T) {
		servers := startmockserverservice.New(log)
		cfg := &models.StartMockServerRequestBody{
			Protocol: constants.TCP,
			Port:     PORT,
		}

		err = servers.StartMockServer(cfg)
		server := servers.GetMockServers()[0]
		require.NoError(t, err)
		require.NotNil(t, server)

		// Check that the server is listening on the correct port
		require.Equal(t, cfg.Protocol, server.ListenerTCP.Addr().Network())
		require.Equal(t, cfg.Port, server.ListenerTCP.Addr().(*net.TCPAddr).Port)
		{
			// create a new TCP connection
			conn1, err := net.Dial(constants.TCP, fmt.Sprintf(":%d", cfg.Port))
			if err != nil {
				t.Errorf("Error dialing TCP connection: %v", err)
				return
			}

			// write a MESSAGE to the connection
			MESSAGE := "test message"
			_, err = conn1.Write([]byte(MESSAGE))
			if err != nil {
				t.Errorf("Error writing message to connection: %v", err)
				return
			}

			// read the response from the connection
			responseBuf := make([]byte, 1024)
			_, err = conn1.Read(responseBuf)
			if err != nil {
				t.Errorf("Error reading response from connection: %v", err)
				return
			}
			response := string(responseBuf[:])

			// verify that the response contains the message
			if !strings.Contains(response, "ok") {
				t.Errorf("Unexpected response. Expected message should contain \"%v\" \nActual message received: %v\n", MESSAGE, response)
			}

			// close the connection and #listener
			defer conn1.Close()
		}
	})

	t.Run("Invalid Port for TCP server", func(t *testing.T) {
		servers := startmockserverservice.New(log)
		cfg := &models.StartMockServerRequestBody{
			Protocol: constants.TCP,
			Port:     80000,
		}

		err := servers.StartMockServer(cfg)
		require.Error(t, err)
	})

	t.Run("Start UDP server", func(t *testing.T) {
		servers := startmockserverservice.New(log)
		cfg := &models.StartMockServerRequestBody{
			Protocol: constants.UDP,
			Port:     PORT,
		}
		err := servers.StartMockServer(cfg)

		server := servers.GetMockServers()[0]
		require.NoError(t, err)
		require.NotNil(t, server)

		// Check that the server is listening on the correct port
		require.Equal(t, cfg.Protocol, server.ListenerUDP.LocalAddr().Network())
		require.Equal(t, cfg.Port, server.ListenerUDP.LocalAddr().(*net.UDPAddr).Port)

		{
			conn1, err := net.Dial(constants.UDP, fmt.Sprintf(":%d", cfg.Port))
			if err != nil {
				t.Errorf("Error dialing TCP connection: %v", err)
				return
			}

			// write a message to the connection
			message := "test message"
			_, err = conn1.Write([]byte(message))
			if err != nil {
				t.Errorf("Error writing message to connection: %v", err)
				return
			}

			// read the response from the connection
			responseBuf := make([]byte, 1024)
			_, err = conn1.Read(responseBuf)
			if err != nil {
				t.Errorf("Error reading response from connection: %v", err)
				return
			}
			response := string(responseBuf[:])

			// verify that the response contains the message
			if !strings.Contains(response, "ok") {
				t.Errorf("Unexpected response. Expected message should contain \"%v\" \nActual message received: %v\n", message, response)
			}

			// close the connection and #listener
			defer conn1.Close()
		}

		// Stop the server and check that it was closed correctly
		err = server.ListenerUDP.Close()
		require.NoError(t, err)
	})

	t.Run("Invalid Port for UDP server", func(t *testing.T) {
		servers := startmockserverservice.New(log)
		cfg := &models.StartMockServerRequestBody{
			Protocol: constants.UDP,
			Port:     800000,
		}

		err := servers.StartMockServer(cfg)

		require.Error(t, err)
	})

	t.Run("Start HTTPS server", func(t *testing.T) {
		servers := startmockserverservice.New(log)
		cfg := &models.StartMockServerRequestBody{
			Protocol: constants.HTTPS,
			Port:     PORT,
			Cert:     SERVER_CERT,
			Key:      SERVER_KEY,
		}
		err := servers.StartMockServer(cfg)
		server := servers.GetMockServers()[0]
		require.NoError(t, err)
		require.NotNil(t, server)

		tr := &http.Transport{TLSClientConfig: &tls.Config{InsecureSkipVerify: true}}

		client := &http.Client{Transport: tr}

		httpReq, err := http.NewRequest("GET", fmt.Sprintf("https://localhost:%d", cfg.Port), nil)

		require.NoError(t, err)

		// Perform the request
		resp, err := client.Do(httpReq)
		if err != nil {
			t.Error("Error sending request:", err)
			return
		}
		defer resp.Body.Close()

		// Read the response body
		body, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			t.Error("Error reading response body:", err)
			return
		}

		// Print the response body
		fmt.Println(string(body))
		fmt.Println(startmockserverservice.GetPrivateIP())
		require.Contains(t, string(body), startmockserverservice.GetPrivateIP())
		// Check that the server is listening on the correct port
		require.Equal(t, fmt.Sprintf(":%d", cfg.Port), server.ListenerHTTP.Addr)

		// Stop the server and check that it was closed correctly
		err = server.ListenerHTTP.Close()
		require.NoError(t, err)
	})

	t.Run("HTTPS server with invalid cert values", func(t *testing.T) {
		servers := startmockserverservice.New(log)
		cfg := &models.StartMockServerRequestBody{
			Protocol: constants.HTTPS,
			Port:     PORT,
			Cert:     "",
			Key:      "",
		}
		err := servers.StartMockServer(cfg)
		require.Error(t, err)

		// Check that the server is listening on the correct port
		require.Contains(t, err.Error(), "certificate input")
	})

	t.Run("HTTPS server with busy port", func(t *testing.T) {

		// Starting a server in port to be checked
		go func() {
			err := http.ListenAndServe(fmt.Sprintf(":%d", PORT), nil)
			if err != nil {
				fmt.Println("Error starting the server:", err)
			}
		}()

		servers := startmockserverservice.New(log)
		cfg := &models.StartMockServerRequestBody{
			Protocol: constants.HTTPS,
			Port:     PORT,
			Cert:     SERVER_CERT,
			Key:      SERVER_KEY,
		}
		err = servers.StartMockServer(cfg)
		require.Error(t, err)

		// Check that the server is listening on the correct port
		require.Contains(t, err.Error(), "address already in use")
	})

	t.Run("Unsupported protocol", func(t *testing.T) {
		servers := startmockserverservice.New(log)
		err := servers.StartMockServer(&models.StartMockServerRequestBody{
			Port:     PORT,
			Protocol: "http",
			Cert:     "",
			Key:      "",
		})

		require.Error(t, err)
		require.Equal(t, "unsupported protocol", err.Error())
	})
}

// GetFreePort asks the kernel for a free open port that is ready to use.
func GetFreePort() (port int, err error) {
	var a *net.TCPAddr
	if a, err = net.ResolveTCPAddr("tcp", "localhost:0"); err == nil {
		var l *net.TCPListener
		if l, err = net.ListenTCP("tcp", a); err == nil {
			defer l.Close()
			return l.Addr().(*net.TCPAddr).Port, nil
		}
	}
	return
}
