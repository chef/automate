package history

type Tag struct {
	Key   string
	Value string
}

func ServiceTag(serviceName string) Tag {
	return Tag{
		"service",
		serviceName,
	}
}
