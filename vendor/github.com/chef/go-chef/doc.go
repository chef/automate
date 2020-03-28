/*
This is a Chef Infra Server API client. This Library can be used to write tools to
interact with the chef server.

The testing can be run with go test, and the client can be used as per normal via:

		go get github.com/chef/go-chef

Documentation can be found on GoDoc at http://godoc.org/github.com/chef/go-chef

This is an example code generating a new node on a Chef Infra Server:


		package main

		import (
			"encoding/json"
			"fmt"
			"io/ioutil"
			"log"
			"os"

			chef "github.com/chef/go-chef
		)

		func main() {
			// read a client key
			key, err := ioutil.ReadFile("key.pem")
			if err != nil {
				fmt.Println("Couldn't read key.pem:", err)
				os.Exit(1)
			}

			// build a client
			client, err := chef.NewClient(&chef.Config{
				Name: "foo",
				Key:  string(key),
				// goiardi is on port 4545 by default. chef-zero is 8889
				BaseURL: "http://localhost:4545",
			})
			if err != nil {
				fmt.Println("unable to setup client:", err)
				os.Exit(1)
			}

			// create a Node object
			ranjib := chef.NewNode("ranjib")

			// create the node on the Chef Infra Server
			_, err = client.Nodes.Post(ranjib)
			if err != nil {
				log.Fatal("couldn't create node. ", err)
			}

			// list nodes
			nodeList, err := client.Nodes.List()
			if err != nil {
				log.Fatal("couldn't list nodes: ", err)
			}

			// dump the node list in Json
			jsonData, err := json.MarshalIndent(nodeList, "", "\t")
			if err != nil {
				log.Fatal("couldn't marshal nodes list: ", err)
			}
			fmt.Println(jsonData)

			// dump the ranjib node we got from server in JSON!
			serverNode, _ := client.Nodes.Get("ranjib")
			if err != nil {
				log.Fatal("couldn't get node: ", err)
			}
			jsonData, err = json.MarshalIndent(serverNode, "", "\t")
			if err != nil {
				log.Fatal("couldn't marshal node: ", err)
			}
			fmt.Println(jsonData)

			// update node
			ranjib.RunList = append(ranjib.RunList, "recipe[works]")
			jsonData, err = json.MarshalIndent(ranjib, "", "\t")
			if err != nil {
				log.Fatal("couldn't marshal node: ", err)
			}
			fmt.Println(jsonData)

			_, err = client.Nodes.Put(ranjib)
			if err != nil {
				log.Fatal("couldn't update node: ", err)
			}

			// delete node
			client.Nodes.Delete(ranjib.Name)
			if err != nil {
				fmt.Println("unable to delete node:", err)
				os.Exit(1)
      }
		}

*/
package chef
