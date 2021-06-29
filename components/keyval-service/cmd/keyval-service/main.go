package main

import (
	"fmt"
	"time"
)

func main() {

	for {
		time.Sleep(10 * time.Second)
		fmt.Println(" this is the keyval-service ")
	}
}
