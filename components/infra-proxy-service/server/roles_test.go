package server_test

import (
	"fmt"
	"testing"
)

func Test(t *testing.T) {
	var result RoleListResult

	a := fromAPIToListRoles(result)
	fmt.Fprint(a)
}
