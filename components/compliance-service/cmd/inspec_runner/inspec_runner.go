// A small shim program to allow running inspec with reduced privileges.

package main

import (
	"log"
	"os"
	"os/user"
	"path"
	"strconv"
	"syscall"

	"github.com/chef/automate/components/compliance-service/cmd/inspec_runner/platform"
)

// Set at build time via linker flags.
var EXECUTABLE_PATH string

func main() {
	if len(EXECUTABLE_PATH) == 0 {
		log.Fatal("No value present for executable path.")
	}

	err := changeToUser("nobody")
	if err != nil {
		log.Fatal(err)
	}

	cmd := path.Base(EXECUTABLE_PATH)

	args := append([]string{cmd}, os.Args[1:]...)

	if err := syscall.Exec(EXECUTABLE_PATH, args, os.Environ()); err != nil {
		log.Fatal(err)
	}
}

// changeToUser changes the uid and gid of the current process to that of the
// passed in username.
func changeToUser(username string) error {
	user, err := user.Lookup(username)
	if err != nil {
		return err
	}

	gid, err := strconv.Atoi(user.Gid)
	if err != nil {
		return err
	}

	uid, err := strconv.Atoi(user.Uid)
	if err != nil {
		return err
	}

	err = platform.Setgid(gid)
	if err != nil {
		return err
	}

	err = platform.Setuid(uid)
	if err != nil {
		return err
	}

	return nil
}
