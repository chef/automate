package firewallchecktrigger

import (
	"fmt"
	"testing"
)

func Test_constructPorts(t *testing.T) {
	ports := constructPorts()
	fmt.Printf("ports: %+v\n", ports)
}
