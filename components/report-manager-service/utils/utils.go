package utils

import (
	"fmt"
	"time"
)

func GetObjName(name string) string {
	return fmt.Sprintf("%s.json", name)
}

func GetCSVObjName(name string) string {
	return fmt.Sprintf("%s.csv", name)
}

//ComputeDuration returns the time difference between startTime and endTime in user readable format.
//if startTime is after endTime the method returns 0s
func ComputeDuration(startTime, endTime time.Time) string {
	if startTime.After(endTime) {
		return "0s"
	}
	diff := endTime.Sub(startTime).Seconds()
	if diff == 0 {
		return "0s"
	} else if diff > 0 && diff < 1 {
		return "1s"
	} else if diff < 60 {
		return fmt.Sprintf("%ds", int(diff+0.5))
	} else if diff >= 60 && diff < 3600 {
		return fmt.Sprintf("%dm", int((diff/60)+0.5))
	}
	diffH := endTime.Sub(startTime).Hours()
	hoursPart := int(diffH)
	minPart := int((diffH - float64(int(diffH))) * 60)
	if minPart == 0 {
		return fmt.Sprintf("%dH", hoursPart)
	}
	return fmt.Sprintf("%dH%dm", hoursPart, minPart)
}
