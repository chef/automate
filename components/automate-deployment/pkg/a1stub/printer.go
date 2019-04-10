package a1stub

import (
	"fmt"

	"github.com/fatih/color"
)

var thMsg = color.New(color.FgWhite, color.BgRed)

func thPrint(s string) {
	fmt.Printf("%s %s", thMsg.SprintFunc()("[test harness]"), s)
}

func thPrintf(fmtString string, f ...interface{}) {
	thPrint(fmt.Sprintf(fmtString, f...))
}
