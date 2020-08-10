package toml

import "google.golang.org/protobuf/reflect/protoreflect"

type wkt interface {
	XXX_WellKnownType() string
}

type protoreflector interface {
	ProtoReflect() protoreflect.Message
}
