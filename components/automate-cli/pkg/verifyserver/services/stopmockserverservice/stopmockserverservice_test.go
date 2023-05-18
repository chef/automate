package stopmockserverservice_test

import (
	"errors"
	"net"
	"testing"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/startmockserverservice"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/stopmockserverservice"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

const (
	SERVER_KEY  = "-----BEGIN RSA PRIVATE KEY-----\nMIIEogIBAAKCAQEAr4V0UWr7B4Hu7gTRDYt7FGEBFXC6V39Cf8i4xgtUYZOKkvt/\nMXiYegFWgZPLah0TyhgdyjLpC8FKdpq+FHpksGgfkR8ARyBsp50b4FRfjCSpKNj+\nSpUhHzovYSywLJFykT9Wq00n/9M992Bxn94MJQIfmHfWUrpOl66o0fdv8viblik7\nEh+sPl+Lgg6miTg8ev529e0Lo//LrW5AtHHEEeBvBPWp1kwnWgatECYDZgXQAbvI\nYzNkqpYyE853hbCBzZ1HKsz4WK0poGg3MSl/AbJC36xi1ecP3rCnh5ae123p1V9d\n337YUDxL7Wst9wKulj+uVPgxNchg11JqgNNaawIDAQABAoIBAEV4SbCL6i1vhPTa\nHTACO8W2Gyq0QlytNtHCzTc9drlkHx3Lwuz+sULg0q9YotMuDQ4Y+3lzKwAHEgTd\nfEw4oS+dFplmrsJ4F+lDaqwgWOzr+bP8JrG4UrK8YdJRUK1jJ/hLHG+SizlbD5Sg\nrHg894uSSpUbIU3/BWpNq+3mxH1itofFanP7ePTCV78kpfGHR2Ok2UMHxyQHSk7y\nzii78grmbQObndx2b7bC7cswCa3/pjMN3X6B/Jjy/gn3b50RR77YFqpp7zzrnrZA\nSgx6CrJSBRwDExhCzMPBh3UjtYgohPHlVLjPluUSZheYDZQB1Gr8HwN3iL1dmf1w\n8QYQnCkCgYEA4GgAReXoOzjbjo06lr8TGogavWB2D3Zls3WUGO9FcD/59IJTi1rC\nh13V7gZYRFdc7HH660vqtfqsYtt5akV0FxYfXAMla9fuYxmr7h8OUfs8jTp77Uq8\ntSlQ3VPaas4YTzTReTD+wHvKVQ6uiDAYqjTqF9yJNkMcZ8veNzWtTG8CgYEAyDuN\nnNGY6VD2p5UA24y01yZqNH9WYAwuIYNAqCypZLzJBP8ABVrCOUNuPIaBCxmU8Orq\nYN9FVexY3nBdkDEjEwbXvnYyydoadIN9DMB3YhQ9etf3DuZFI65+DAskHE/cJrpP\neUM/XPpC73pf3U+eqSZwV+7FX8idgsK6Rxnnh8UCgYAyTN2SzV/qtmnwYBO76oR7\ns/pabJ7KBH3zZe2WUTu9V3nNptDXMbbc5NmpCt8KIpL/pOTbjR7FP7UYS53BhmPp\nMNpCo6nlrHcQ25ZAP9HT6n6+IVfZ7qCx8trfYYZZ3mxwhKRXh/Xya00FF89jU3ST\n4lx+kL5o3U4mrfnXYj7AHQKBgC5vERIS0SEaM3j9ZuuDH9TdBbgS55byfCgtZesa\nIFZKKVvNPtX/DBd3ebLzhi1qy01rTNsWK+AXJSzAZhIwMvAQoCt9AZ4pxATNEUzJ\nvWWzR+aa+qIr6FC0AGsOkls2cdlRT2jRnXoUVz1t5ZlPA346eccKih8CSPSv777Z\nVQX5AoGAf4rHCoMGGqlJZxaroCS2AgDtzq5Za9XdwT3RczYnJGCkv7nK4K0e4zxB\nqMxAiUTrzae1Hpf5OV0GXeoiN0taZ9L2Cc56OlaqPRL2BDpYU2K+3okrftXLgFCX\nClDVL9XxSbVT99AUEBw9eH9hYFVKjHIEyJ20Udw0zsR+qU4R6/0=\n-----END RSA PRIVATE KEY-----"
	SERVER_CERT = "-----BEGIN CERTIFICATE-----\nMIIDTTCCAjWgAwIBAgIUNidKDNanRMXILhrf1//SuK7DC3UwDQYJKoZIhvcNAQEL\nBQAwYzELMAkGA1UEBhMCVVMxEzARBgNVBAgMCldhc2hpbmd0b24xEDAOBgNVBAcM\nB1NlYXR0bGUxGjAYBgNVBAoMEUNoZWYgU29mdHdhcmUgSW5jMREwDwYDVQQDDAhw\ncm9ncmVzczAeFw0yMzA1MDkxMjAwMjZaFw0yNjA1MDgxMjAwMjZaMDYxCzAJBgNV\nBAYTAlVTMRMwEQYDVQQIDApDYWxpZm9ybmlhMRIwEAYDVQQDDAlsb2NhbGhvc3Qw\nggEiMA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQCvhXRRavsHge7uBNENi3sU\nYQEVcLpXf0J/yLjGC1Rhk4qS+38xeJh6AVaBk8tqHRPKGB3KMukLwUp2mr4UemSw\naB+RHwBHIGynnRvgVF+MJKko2P5KlSEfOi9hLLAskXKRP1arTSf/0z33YHGf3gwl\nAh+Yd9ZSuk6XrqjR92/y+JuWKTsSH6w+X4uCDqaJODx6/nb17Quj/8utbkC0ccQR\n4G8E9anWTCdaBq0QJgNmBdABu8hjM2SqljITzneFsIHNnUcqzPhYrSmgaDcxKX8B\nskLfrGLV5w/esKeHlp7XbenVX13ffthQPEvtay33Aq6WP65U+DE1yGDXUmqA01pr\nAgMBAAGjJjAkMCIGA1UdEQQbMBmCCWxvY2FsaG9zdIIMaHR0cHMtc2VydmVyMA0G\nCSqGSIb3DQEBCwUAA4IBAQCxXspmv+BCRVFykb80embuZGCXMh7YmH3j5dJZhaKL\n/PPcUjgJTYRanDSSwt5IFyyYwiYG9qdUrRLxR5pgpdj0vNRaLdabG24UsVQQuK1Y\nrxb/6HF2AwWASiS5YLKoVMwg1sYiskpA7gJ23Xe34BVckqAd+Yoss4zDNR3d/yRM\nQYbnr/STpW4c+7jHL+vlpu/OdHwEtsTNrUG6lk0YO1lGH43a0rmvMzYOCgZEfr3h\nK4zbwy053Vq8PGIH24/bNu67pSfslgGI30bN9PmUdFMwEFuRC7rCFgQ8LpRbJNf1\nf5CUhHWn2nC05XOzKm+Kj/NHPtw5iJkrQvLNtsdiO92O\n-----END CERTIFICATE-----"
)

func StartServer(protocol string, port int, cert string, key string, l logger.Logger) (*models.Server, error) {
	servers := startmockserverservice.New(l)

	cfg := &models.StartMockServerRequestBody{
		Protocol: protocol,
		Port:     port,
		Cert:     cert,
		Key:      key,
	}
	err := servers.StartMockServer(cfg)
	if err != nil {
		return nil, err
	}
	server := servers.GetMockServers()[0]
	return server, nil
}

func ErrorForServer(protocol string, server *models.Server) {
	switch protocol {
	case constants.TCP:
		server.ListenerTCP = &MockListener{}
	case constants.UDP:
		server.ListenerUDP = &MockPacketConn{}
	}

}

func TestStopMockServer(t *testing.T) {
	l, err := logger.NewLogger("text", "debug")
	assert.NoError(t, err)
	tests := []struct {
		name     string
		port     int
		protocol string
		cert     string
		key      string
		wantErr  bool
		err      string
	}{
		{
			name:     "Stop TCP server",
			port:     1223,
			protocol: "tcp",
			cert:     "",
			key:      "",
		},
		{
			name:     "Stop UDP server",
			port:     122,
			protocol: "udp",
			cert:     "",
			key:      "",
		},
		{
			name:     "Stop HTTPS server",
			port:     80,
			protocol: "https",
			cert:     SERVER_CERT,
			key:      SERVER_KEY,
		},
		{
			name:     "Error while stopping UDP server",
			port:     123,
			protocol: "udp",
			cert:     "",
			key:      "",
			wantErr:  true,
			err:      "Error while shutting UDP server",
		},
		{
			name:     "Error while stopping TCP server",
			port:     1224,
			protocol: "tcp",
			cert:     "",
			key:      "",
			wantErr:  true,
			err:      "Error while shutting TCP server",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			s := stopmockserverservice.NewStopMockServerService(l)

			server, err := StartServer(tt.protocol, tt.port, tt.cert, tt.key, l)
			assert.NoError(t, err)
			if tt.wantErr {
				ErrorForServer(tt.protocol, server)
				err = s.StopMockServer(server)
				assert.Error(t, err)
				assert.Equal(t, tt.err, err.Error())
			} else {
				err = s.StopMockServer(server)
				assert.NoError(t, err)
			}
		})
	}
}

type MockListener struct{}

func (l *MockListener) Accept() (net.Conn, error) {
	return nil, nil
}

func (l *MockListener) Close() error {
	return errors.New("Error while shutting TCP server")
}

func (l *MockListener) Addr() net.Addr {
	return nil
}

type MockPacketConn struct{}

func (l *MockPacketConn) Close() error {
	return errors.New("Error while shutting UDP server")
}

func (l *MockPacketConn) ReadFrom(p []byte) (n int, addr net.Addr, err error) {
	return 0, nil, nil
}

func (l *MockPacketConn) WriteTo(p []byte, addr net.Addr) (n int, err error) {
	return 0, nil
}

func (l *MockPacketConn) LocalAddr() net.Addr {
	return nil
}

func (l *MockPacketConn) SetDeadline(t time.Time) error {
	return nil
}

func (l *MockPacketConn) SetReadDeadline(t time.Time) error {
	return nil
}

func (l *MockPacketConn) SetWriteDeadline(t time.Time) error {
	return nil
}
