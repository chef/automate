package main

import "os"

func isDevMode() bool {
	return os.Getenv("CHEF_DEV_ENVIRONMENT") == "true"
}
