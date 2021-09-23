package main

type deployer interface {
	deploy() interface{}
}
