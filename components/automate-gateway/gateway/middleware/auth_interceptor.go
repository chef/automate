package middleware

import (
	"context"
	"crypto/x509"
	"encoding/pem"
	"errors"
	"net/url"
	"strings"

	"github.com/grpc-ecosystem/go-grpc-middleware/logging/logrus/ctxlogrus"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/credentials"
	"google.golang.org/grpc/metadata"
	"google.golang.org/grpc/peer"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/components/automate-gateway/api/authz/pairs"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/grpc/service_authn"
)

// NewAuthInterceptor returns an AuthInterceptor that performs
// authentication and authorization for incoming requests.
// The return function uses the incoming request metadata (from its context),
// and constructs an authentication request to authn-service's authentication
// service that uses this metadata as outgoing metadata.
// If the inquiry's result is an error, it is returned as-is. If it's not, this
// function returns only the context, and allows for further request processing.
func NewAuthInterceptor(
	authn authn.AuthenticationClient,
	authz GRPCAuthorizationHandler,
) AuthorizationInterceptor {
	return &authInterceptor{authn: authn, authz: authz}
}

type SwitchingAuthorizationHandler interface {
	GRPCAuthorizationHandler
	SwitchingFilterHandler
	IsAuthorized(ctx context.Context, subjects []string,
		resourceV1, actionV1, resourceV2, actionV2 string, projects []string) (AnnotatedAuthorizationResponse, error)
}

type SwitchingFilterHandler interface {
	FilterAuthorizedPairs(ctx context.Context, subjects []string,
		mapByResourceAndActionV1, mapByResourceAndActionV2 map[pairs.Pair][]string,
		methodsInfoV1, methodsInfoV2 map[string]pairs.Info,
	) (*FilterPairsResponse, error)
}

// FilterPairsResponse includes the "used" half of the inputs, according to
// v1/v2
type FilterPairsResponse struct {
	Pairs                  []*pairs.Pair
	MethodsInfo            map[string]pairs.Info
	MapByResourceAndAction map[pairs.Pair][]string
}

type FilterProjectsResponse struct {
	Projects               []string
	MethodsInfo            map[string]pairs.Info
	MapByResourceAndAction map[pairs.Pair][]string
}

type GRPCAuthorizationHandler interface {
	Handle(ctx context.Context, subjects []string, projects []string, req interface{}) (context.Context, error)
}

type HTTPAuthorizationHandler interface {
	IsAuthorized(ctx context.Context, subjects []string, resource, action string, projects []string) (AuthorizationResponse, error)
}

type IntrospectionHandler interface {
	FilterAuthorizedPairs(ctx context.Context, subjects []string, pairs []*pairs.Pair) ([]*pairs.Pair, error)
}

type AuthorizationHandler interface {
	HTTPAuthorizationHandler
	GRPCAuthorizationHandler
	IntrospectionHandler
}

type AuthorizationResponse interface {
	Ctx() context.Context
	GetAuthorized() bool
}

type AnnotatedAuthorizationResponse interface {
	AuthorizationResponse
	Err() error
}

type authInterceptor struct {
	authn authn.AuthenticationClient
	authz GRPCAuthorizationHandler
}

// UnaryInterceptor returns a grpc UnaryServerInterceptor that performs AuthN/Z.
func (a *authInterceptor) UnaryServerInterceptor() grpc.UnaryServerInterceptor {
	return func(
		ctx context.Context,
		req interface{},
		info *grpc.UnaryServerInfo,
		handler grpc.UnaryHandler) (interface{}, error) {

		// extract request-scoped logger
		log := ctxlogrus.Extract(ctx)

		// this context is only used for authenticating the request: we need
		// headers!
		// grpc-gateway translates these into metadata, and we'll forward that to
		// authn-service below. we don't want that metadata on _every_ request,
		// though, so this context is separated out
		authCtx := ctx

		// transfer incoming metadata to outgoing metadata
		// if !ok, this will default to the ctx set above, and most likely be
		// rejected by authn-service. However, we don't want to duplicate the
		// "when is a request potentially good for authentication" logic here,
		// so this will just go to authn-service as-is.
		var md metadata.MD
		var ok bool
		if md, ok = metadata.FromIncomingContext(authCtx); ok {
			authCtx = metadata.NewOutgoingContext(authCtx, md)
		}

		var subs []string
		var authResponse *authn.AuthenticateResponse
		var err error

		certSubject, ok := headerAuthValidForClientAndPeer(ctx)
		if ok && !fromGateway(certSubject) {
			subs = []string{certSubject}
			log.Debugf("using client cert to authenticate request: %q", certSubject)
		} else { // ordinary authn by calling authn-service
			authResponse, err = a.authn.Authenticate(authCtx, &authn.AuthenticateRequest{})
			if err != nil {
				log.Debugf("error authenticating request: %s", err)
				return nil, err
			}
			subs = append(authResponse.Teams, authResponse.Subject)
		}

		projects := auth_context.ProjectsFromMetadata(md)

		ctx, err = a.authz.Handle(authCtx, subs, projects, req)
		if err != nil {
			return nil, err
		}

		// This is similar to AuthorizationBypasser, to let services opt-in if they
		// want the auth context injected. Note that this happens after authorization,
		// so, nothing is bypassed.
		//
		// However, every domain service will need the projects eventually,
		// so we'll at least always inject project metadata.
		if _, ok := info.Server.(AuthContextReader); ok {
			log.Debugf("injecting auth context for method %q to downstream", info.FullMethod)
			ctx = auth_context.NewOutgoingContext(ctx)
		} else {
			ctx = auth_context.NewOutgoingProjectsContext(ctx)
		}

		return handler(ctx, req)
	}
}

func getProjectsFromMetadata(projectHeaderEntries []string) []string {
	if projectHeaderEntries == nil {
		projectHeaderEntries = []string{}
	}
	projects := []string{}
	keys := make(map[string]bool)
	for _, entry := range projectHeaderEntries {
		for _, project := range strings.Split(entry, ",") {
			newProject := strings.TrimSpace(project)
			if !keys[newProject] {
				keys[newProject] = true
				projects = append(projects, newProject)
			}
		}
	}
	return projects
}

func (a *authInterceptor) StreamServerInterceptor() grpc.StreamServerInterceptor {
	return func(srv interface{}, ss grpc.ServerStream, info *grpc.StreamServerInfo, handler grpc.StreamHandler) error {
		// This is the only method of the reflection service, see
		// https://github.com/grpc/grpc/blob/aef957950/src/proto/grpc/reflection/v1alpha/reflection.proto#L21-L26
		if info.FullMethod == "/grpc.reflection.v1alpha.ServerReflection/ServerReflectionInfo" {
			return handler(srv, ss)
		}

		//!\\ Note: this ^ is a stop-gap while we haven't (conceptually) figured out
		//          authz for streaming services. Do not add further methods please.

		// This is a hack. Since streaming is not supported, we want to make sure you still have some valid cert
		// when trying to do things that are streaming based. This is currently only used for the debug endpoint.
		// I think this is ok because we had an http endpoint that was not auth'd at all. Also, we don't expose
		// gateway over grpc, and connecting to grpc requires a signed cert. So this is probably overkill and
		// simply skipping the check would be fine, but we'll go and make sure the cert is the one the CLI is
		// using
		if _, ok := srv.(DeploymentCertAuthOnly); ok {
			certSubject, ok := headerAuthValidForClientAndPeer(ss.Context())
			if ok && fromDeployment(certSubject) {
				return handler(srv, ss)
			}
			return status.Errorf(codes.PermissionDenied, "unauthorized access to %s", info.FullMethod)

		}
		return errors.New("to be implemented")
	}
}

// There's some stuff going on here: we only trust the headers if they've
// come in through a path we trust.
// 1. If the GRPC peer is NOT automate-gateway, we return that and don't inspect
//    any other headers
// 2. If the GRPC peer is automate-gateway, we look at the client cert of the
//    client that has sent a HTTPS request to grpc-gateway (this is
//    x-client-cert)
// 3. If that client is NOT automate-load-balancer, we return that and don't
//    inspect the other headers
// 4. If the client is automate-load-balancer, we look at the header injected
//    by automate-load-balancer as the (verified) cert of the client that has
//    sent the request to automate-load-balancer.
func headerAuthValidForClientAndPeer(ctx context.Context) (string, bool) {
	// what follows is what we get from DIRECT GRPC connections
	peer, ok := peer.FromContext(ctx)
	if ok {
		if tlsInfo, ok := peer.AuthInfo.(credentials.TLSInfo); ok {
			if len(tlsInfo.State.VerifiedChains) > 0 && len(tlsInfo.State.VerifiedChains[0]) > 0 {
				sub, ok := service_authn.ServiceSubjectFromCert(tlsInfo.State.VerifiedChains[0][0])
				if ok && !fromGateway(sub) {
					return sub, true // any other service => don't inspect metadata
				}
			}
		}
	}

	// only attempt this if we're getting the request from automate-gateway => so
	// it's coming in through grpc-gateway'ed endpoint
	if md, ok := metadata.FromIncomingContext(ctx); ok {

		// this was injected into metadata for requests via HTTPS (grpc-gateway)
		if vals := md.Get("x-client-cert"); len(vals) == 1 && vals[0] != "" {
			sub, ok := subjFromCertString(vals[0])
			if !ok {
				return "", false
			}
			if !strings.HasPrefix(sub, "tls:service:automate-load-balancer:") {
				return sub, true // any other service => don't go on
			}

			// this was checked by automate-load-balancer, passed on as headers, and
			// injected into metadata by grpc-gateway
			if vals := md.Get("grpcgateway-x-client-cert"); len(vals) == 1 && vals[0] != "" {
				return subjFromCertString(vals[0])
			}
		}
	}

	return "", false
}

func subjFromCertString(escaped string) (string, bool) {
	bs, err := url.QueryUnescape(escaped)
	if err != nil {
		logrus.WithError(err).Error("Failed to decode header container cert")
		return "", false
	}
	block, _ := pem.Decode([]byte(bs))
	if block == nil {
		logrus.WithError(err).Error("Failed to decode cert")
		return "", false
	}
	cert, err := x509.ParseCertificate(block.Bytes)
	if err != nil {
		logrus.WithError(err).Error("Failed to parse cert")
		return "", false
	}
	return service_authn.ServiceSubjectFromCert(cert)
}

// fromGateway checks if the passed subject indicates the request came from
// automate-gateway itself
func fromGateway(subj string) bool {
	return strings.HasPrefix(subj, "tls:service:automate-gateway:")
}

// fromDeployment checks if the passed subject indicates the request came from
// deployment-service
func fromDeployment(subj string) bool {
	return strings.HasPrefix(subj, "tls:service:deployment-service:")
}
