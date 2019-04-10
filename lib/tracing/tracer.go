package tracing

import (
	"io"

	"github.com/grpc-ecosystem/grpc-opentracing/go/otgrpc"
	opentracing "github.com/opentracing/opentracing-go"
	jaegercfg "github.com/uber/jaeger-client-go/config"
	"golang.org/x/net/context"
	"google.golang.org/grpc"
)

func NewTracer(service string) (opentracing.Tracer, io.Closer, error) {
	// See Jaeger documentation for how to control sampler, reporter, logger,
	// metrics, etc. For now we go with the safe defaults.
	cfg := jaegercfg.Configuration{}

	return cfg.New(service)
}

func NewGlobalTracer(service string) (io.Closer, error) {
	tracer, closer, err := NewTracer(service)
	opentracing.SetGlobalTracer(tracer)

	return closer, err
}

func CloseQuietly(closer io.Closer) {
	_ = closer.Close()
}

func SetGlobalTracer(tracer opentracing.Tracer) {
	opentracing.SetGlobalTracer(tracer)
}

func GlobalTracer() opentracing.Tracer {
	return opentracing.GlobalTracer()
}

func ServerInterceptor(tracer opentracing.Tracer) grpc.UnaryServerInterceptor {
	return otgrpc.OpenTracingServerInterceptor(tracer)
}

func GlobalServerInterceptor() grpc.ServerOption {
	return grpc.UnaryInterceptor(ServerInterceptor(GlobalTracer()))
}

func ClientInterceptor(tracer opentracing.Tracer) grpc.UnaryClientInterceptor {
	return otgrpc.OpenTracingClientInterceptor(tracer)
}

func GlobalClientInterceptor() grpc.DialOption {
	return grpc.WithUnaryInterceptor(ClientInterceptor(opentracing.GlobalTracer()))
}

func StartSpan(
	operationName string, opts ...opentracing.StartSpanOption,
) opentracing.Span {
	return opentracing.StartSpan(operationName, opts...)
}

func StartSpanFromContext(
	ctx context.Context, operationName string, opts ...opentracing.StartSpanOption,
) (opentracing.Span, context.Context) {
	return opentracing.StartSpanFromContext(ctx, operationName, opts...)
}

func ChildOf(parentCtx opentracing.SpanContext) opentracing.SpanReference {
	return opentracing.ChildOf(parentCtx)
}
