package preflight

import (
	"net"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

var ipv6Example = strings.TrimSpace(`
sl  local_address                         remote_address                        st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode
0:  B80D01200000000067452301EFCDAB89:279D 00000000000000000000000000000000:0000 0A 00000000:00000000 00:00000000 00000000     0        0 12089 1 0000000000000000 99 0 0 10 0
`)

func TestNetstatIPv6(t *testing.T) {
	r := strings.NewReader(ipv6Example)
	tcpinfo, err := parseNetTCP(r)
	require.NoError(t, err)
	require.Len(t, tcpinfo, 1)
	assert.Equal(t, TCPInfo{
		LocalIP:   net.ParseIP("2001:0db8::0123:4567:89ab:cdef"),
		LocalPort: 10141,
		State:     TCPStateListen,
	}, tcpinfo[0])
}

var ipv4Example = strings.TrimSpace(`
sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode
1: 00000000:279D 00000000:0000 0A 00000000:00000000 00:00000000 00000000    42        0 9064307 1 0000000000000000 99 0 0 10 0
2: 7501A8C0:BB08 7501A8C0:2790 01 00000000:00000000 00:00000000 00000000    42        0 10309977 1 0000000000000000 20 3 31 10 -1
`)

func TestNetstatIPv4(t *testing.T) {
	r := strings.NewReader(ipv4Example)
	tcpinfo, err := parseNetTCP(r)
	require.NoError(t, err)
	require.Len(t, tcpinfo, 2)
	assert.Equal(t, TCPInfo{
		LocalIP:   net.ParseIP("0.0.0.0"),
		LocalPort: 10141,
		State:     TCPStateListen,
	}, tcpinfo[0])

	assert.Equal(t, TCPInfo{
		LocalIP:   net.ParseIP("192.168.1.117"),
		LocalPort: 47880,
		State:     TCPStateEstablished,
	}, tcpinfo[1])
}
