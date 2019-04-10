package preflight

import (
	"bufio"
	"encoding/hex"
	"io"
	"net"
	"strconv"
	"strings"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

type TCPInfo struct {
	LocalIP   net.IP
	LocalPort int
	State     TCPState
}

type TCPState int

func (tcpState TCPState) String() string {
	s := TCPState_name[tcpState]
	if s == "" {
		return "UNKNOWN"
	}
	return s
}

const (
	TCPStateEstablished TCPState = 1
	TCPStateSynSent     TCPState = 2
	TCPStateSynRecv     TCPState = 3
	TCPStateFinWait1    TCPState = 4
	TCPStateFinWait2    TCPState = 5
	TCPStateTimeWait    TCPState = 6
	TCPStateClose       TCPState = 7
	TCPStateCloseWait   TCPState = 8
	TCPStateLastAck     TCPState = 9
	TCPStateListen      TCPState = 10
	TCPStateClosing     TCPState = 11
	TCPStateUnknown     TCPState = 12
)

var TCPState_name = map[TCPState]string{
	TCPStateEstablished: "ESTABLISHED",
	TCPStateSynSent:     "SYN_SENT",
	TCPStateSynRecv:     "SYN_RECV",
	TCPStateFinWait1:    "FIN_WAIT1",
	TCPStateFinWait2:    "FIN_WAIT2",
	TCPStateTimeWait:    "TIME_WAIT",
	TCPStateClose:       "CLOSE",
	TCPStateCloseWait:   "CLOSE_WAIT",
	TCPStateLastAck:     "LAST_ACK",
	TCPStateListen:      "LISTEN",
	TCPStateClosing:     "CLOSING",
	TCPStateUnknown:     "UNKNOWN",
}

func parseNetTCP(reader io.Reader) ([]TCPInfo, error) {
	scanner := bufio.NewScanner(reader)
	isFirst := true
	tcpInfo := []TCPInfo{}
	for scanner.Scan() {
		line := scanner.Text()
		logrus.Debug(line)

		if isFirst {
			isFirst = false
			continue
		}

		fields := strings.Fields(line)

		if len(fields) < 4 {
			return nil, errors.Errorf("net tcp line does not have enough fields: %q", line)
		}

		ip, port, err := parseIPAndPort(fields[1])
		if err != nil {
			return nil, err
		}

		tcpState, err := parseTCPState(fields[3])
		if err != nil {
			return nil, err
		}

		tcpInfo = append(tcpInfo, TCPInfo{
			LocalIP:   ip,
			LocalPort: port,
			State:     tcpState,
		})
	}
	return tcpInfo, nil
}

func parseIPAndPort(field string) (net.IP, int, error) {
	address := strings.Split(field, ":")
	if len(address) != 2 {
		return nil, 0, errors.Errorf("failed to parse address field %q", field)
	}
	ipBytes, err := hex.DecodeString(address[0])
	if err != nil {
		return nil, 0, errors.Wrapf(err, "failed to decode address field %q", field)
	}

	var ip net.IP

	if len(ipBytes) == 4 {
		ip = net.IPv4(ipBytes[3], ipBytes[2], ipBytes[1], ipBytes[0])
	} else if len(ipBytes) == 16 {
		reverse4(ipBytes[0:4])
		reverse4(ipBytes[4:8])
		reverse4(ipBytes[8:12])
		reverse4(ipBytes[12:16])
		ip = net.IP(ipBytes)
	} else {
		return nil, 0, errors.New("parsed invalid ip address")
	}

	port, err := strconv.ParseInt(address[1], 16, 32)
	if err != nil {
		return nil, 0, errors.Wrapf(err, "failed to parse port for address field %q", field)
	}

	return ip, int(port), nil
}

func reverse4(bytes []byte) {
	bytes[0], bytes[1], bytes[2], bytes[3] = bytes[3], bytes[2], bytes[1], bytes[0]
}

func parseTCPState(field string) (TCPState, error) {
	stateBytes, err := hex.DecodeString(field)
	if err != nil {
		return 0, errors.Wrapf(err, "failed to decode tcp state field %q", field)
	}

	if len(stateBytes) != 1 {
		return 0, errors.Wrapf(err, "invalid state %q", field)
	}

	stateInt := int(stateBytes[0])
	if stateInt >= int(TCPStateUnknown) {
		stateInt = int(TCPStateUnknown)
	}

	return TCPState(stateInt), nil
}
