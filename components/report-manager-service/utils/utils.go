package utils

import "fmt"

func GetObjName(name string) string {
	return fmt.Sprintf("%s.json", name)
}

func GetCSVObjName(name string) string {
	return fmt.Sprintf("%s.csv", name)
}
