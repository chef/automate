// A small shim program to allow running inspec with reduced privileges.

package main

import (
	"fmt"
	"log"
	"os"
	"path"
	"strconv"
	"syscall"

	"github.com/chef/automate/components/compliance-service/cmd/inspec_runner/platform"
	"github.com/chef/automate/lib/user"
	"github.com/sirupsen/logrus"
)

// Set at build time via linker flags.
var EXECUTABLE_PATH string

func main() {

	logrus.Println("Inside the main method ----- main()")
	if len(EXECUTABLE_PATH) == 0 {
		log.Fatal("No value present for executable path.")
	}

	err := changeToUser("nobody")
	if err != nil {
		log.Fatal(err)
	}

	cmd := path.Base(EXECUTABLE_PATH)

	args := append([]string{cmd}, os.Args[1:]...)

	logrus.Println("Inside the args method ----- main()")

	if err := syscall.Exec(EXECUTABLE_PATH, args, os.Environ()); err != nil {
		log.Fatal(fmt.Errorf("inspec_runner unable to complete with executable path: %s, args: %v, env: %s - error %w", EXECUTABLE_PATH, args, os.Environ(), err))
	}
}

// changeToUser changes the uid and gid of the current process to that of the
// passed in username.
func changeToUser(username string) error {
	user, err := user.Lookup(username)
	if err != nil {
		return fmt.Errorf("inspec_runner unable to look up user %s: %w", username, err)
	}

	gid, err := strconv.Atoi(user.Gid)
	if err != nil {
		return fmt.Errorf("inspec_runner unable to convert gid string to int %s: %w", user.Gid, err)
	}

	uid, err := strconv.Atoi(user.Uid)
	if err != nil {
		return fmt.Errorf("inspec_runner unable to convert uid string to int %s: %w", user.Uid, err)
	}

	err = platform.Setgid(gid)
	if err != nil {
		return fmt.Errorf("inspec_runner unable to set gid %d: %w", gid, err)
	}

	err = platform.Setuid(uid)
	if err != nil {
		return fmt.Errorf("inspec_runner unable to set uid %d: %w", uid, err)
	}

	return nil
}
