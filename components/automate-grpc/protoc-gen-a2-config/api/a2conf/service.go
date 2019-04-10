package a2conf

type A2ServiceConfig interface {
	PortBind
	ServiceName() string
}
