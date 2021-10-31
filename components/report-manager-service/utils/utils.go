package utils

import "fmt"

func GetObjName(name string) string {
	return fmt.Sprintf("%s.json", name)
}
