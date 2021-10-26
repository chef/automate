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
	authn authn.AuthenticationServiceClient,
	authz GRPCAuthorizationHandler,
) AuthorizationInterceptor {
	return &authInterceptor{authn: authn, authz: authz}
}

type authInterceptor struct {
	authn authn.AuthenticationServiceClient
	authz GRPCAuthorizationHandler
}

func (a *authInterceptor) combinedAuth(ctxIn context.Context, req interface{}) (context.Context, error) {
	// extract request-scoped logger
	log := ctxlogrus.Extract(ctxIn)

	// this context is only used for authenticating the request: we need
	// headers!
	// grpc-gateway translates these into metadata, and we'll forward that to
	// authn-service below. we don't want that metadata on _every_ request,
	// though, so this context is separated out
	authCtx := ctxIn

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

	certSubject, ok := headerAuthValidForClientAndPeer(ctxIn)
	if ok && !fromGateway(certSubject) {
		subs = []string{certSubject}
		log.Debugf("using client cert to authenticate request: %q", certSubject)
	} else { // ordinary authn by calling authn-service
		authResponse, err = a.authn.Authenticate(authCtx, &authn.AuthenticateRequest{})
		if err != nil {
			log.Debugf("error authenticating request: %s", err)
			return nil, err
		}
		authCtx = context.WithValue(authCtx, "requestorID", authResponse.Requestor)
		subs = append(authResponse.Teams, authResponse.Subject)
	}

	projects := auth_context.ProjectsFromMetadata(md)

	ctxOut, err := a.authz.Handle(authCtx, subs, projects, req)
	if err != nil {
		return nil, err
	}

	// pass on all auth metadata to domain services
	// services can use that metadata in authz-related decisions
	// like allowing project assignment
	log.Debug("injecting auth context to downstream")
	ctxOut = auth_context.NewOutgoingContext(ctxOut)
	return ctxOut, nil
}

// UnaryInterceptor returns a grpc UnaryServerInterceptor that performs AuthN/Z.
func (a *authInterceptor) UnaryServerInterceptor() grpc.UnaryServerInterceptor {
	return func(
		ctx context.Context,
		req interface{},
		info *grpc.UnaryServerInfo,
		handler grpc.UnaryHandler) (interface{}, error) {

		ctxForDownstream, err := a.combinedAuth(ctx, req)
		if err != nil {
			return nil, err
		}

		return handler(ctxForDownstream, req)
	}
}

// interceptedServerStream wraps a grpc.ServerStream in order to allow an
// updated context to be given to the handler. The auth process updates the
// context with auth metadata which should be passed along to the domain
// service that ultimately handles the request.
type interceptedServerStream struct {
	ctx context.Context
	grpc.ServerStream
}

func (i *interceptedServerStream) Context() context.Context {
	return i.ctx
}

func (a *authInterceptor) StreamServerInterceptor() grpc.StreamServerInterceptor {
	return func(req interface{}, ss grpc.ServerStream, info *grpc.StreamServerInfo, handler grpc.StreamHandler) error {

		// This is the only method of the reflection service, see
		// https://github.com/grpc/grpc/blob/aef957950/src/proto/grpc/reflection/v1alpha/reflection.proto#L21-L26
		if info.FullMethod == "/grpc.reflection.v1alpha.ServerReflection/ServerReflectionInfo" {
			return handler(req, ss)
		}

		//!\\ Note: this ^ is a stop-gap while we haven't (conceptually) figured out
		//          authz for streaming services. Do not add further methods please.

		// Special case for the debug server; requires the deployment service's
		// cert. At the time when this case was added, we didn't support auth on
		// streaming services but we needed it for this one thing.
		if _, ok := req.(DeploymentCertAuthOnly); ok {
			certSubject, ok := headerAuthValidForClientAndPeer(ss.Context())
			if ok && fromDeployment(certSubject) {
				return handler(req, ss)
			}
			return status.Errorf(codes.PermissionDenied, "unauthorized access to %s", info.FullMethod)

		}

		// A client stream kinda resembles a sequence of several requests. We do
		// not have consensus about how we want do deal with that so these are
		// disallowed for now.
		if info.IsClientStream {
			return errors.New("to be implemented")
		}

		ctxForDownstream, err := a.combinedAuth(ss.Context(), req)
		if err != nil {
			return err
		}

		i := interceptedServerStream{ctx: ctxForDownstream, ServerStream: ss}

		return handler(req, &i)
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
