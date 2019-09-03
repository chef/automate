package cerealtest

import "github.com/chef/automate/lib/cereal"

func NewStartEvent() cereal.StartEvent {
	return cereal.StartEvent{}
}

func NewTaskCompleteEvent(taskName string, tr cereal.TaskResult) cereal.TaskCompleteEvent {
	return cereal.TaskCompleteEvent{
		TaskName: taskName,
		Result:   tr,
	}
}

func NewCancelEvent() cereal.CancelEvent {
	return cereal.CancelEvent{}
}
