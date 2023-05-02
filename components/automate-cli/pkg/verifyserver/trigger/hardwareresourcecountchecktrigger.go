package trigger

type IHardwareResourceCountCheckTrigger interface {
	Run()
}

type HardwareResourceCountCheck struct {
}

func(hrc *HardwareResourceCountCheck) Run() {

}

func NewHardwareResourceCountCheck() IHardwareResourceCountCheckTrigger{
	return &HardwareResourceCountCheck{}
}


