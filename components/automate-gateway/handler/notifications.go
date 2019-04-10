package handler

import (
	"context"
	"reflect"
	"strings"

	"github.com/golang/protobuf/jsonpb"
	"github.com/golang/protobuf/proto"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	notifications "github.com/chef/automate/components/notifications-client/api"

	pb "github.com/chef/automate/components/automate-gateway/api/notifications"
)

type notificationsServer struct {
	notificationsClient notifications.NotificationsClient
}

func NewNotificationsServer(notificationsClient notifications.NotificationsClient) *notificationsServer {
	return &notificationsServer{notificationsClient}
}

func (s *notificationsServer) AddRule(ctx context.Context, in *pb.RuleAddRequest) (*pb.RuleAddResponse, error) {
	var transformed notifications.Rule
	err := transform(in.Rule, &transformed)
	if err != nil {
		return nil, err
	}

	resp, err := s.notificationsClient.AddRule(ctx, &transformed)
	if err != nil {
		return nil, err
	}

	switch resp.GetCode() {
	case notifications.RuleAddResponse_ADDED:
		return &pb.RuleAddResponse{Messages: resp.Messages, Id: resp.Id}, nil
	case notifications.RuleAddResponse_DUPLICATE_NAME:
		return nil, status.Error(codes.AlreadyExists, strings.Join(resp.GetMessages(), "\n"))
	case notifications.RuleAddResponse_INVALID_ACTION_CONFIG,
		notifications.RuleAddResponse_VALIDATION_ERROR:
		return nil, status.Error(codes.InvalidArgument, strings.Join(resp.GetMessages(), "\n"))
	default:
		return nil, status.Error(codes.Internal, "Internal Error")
	}
}

func (s *notificationsServer) UpdateRule(ctx context.Context, in *pb.RuleUpdateRequest) (*pb.RuleUpdateResponse, error) {
	var transformed notifications.Rule
	err := transform(in.GetRule(), &transformed)
	if err != nil {
		return nil, err
	}

	transformed.Id = in.GetId()
	resp, err := s.notificationsClient.UpdateRule(ctx, &transformed)
	if err != nil {
		return nil, err
	}

	switch resp.GetCode() {
	case notifications.RuleUpdateResponse_OK:
		return &pb.RuleUpdateResponse{Messages: resp.Messages}, nil
	case notifications.RuleUpdateResponse_DUPLICATE_NAME:
		return nil, status.Error(codes.AlreadyExists, strings.Join(resp.GetMessages(), "\n"))
	case notifications.RuleUpdateResponse_VALIDATION_ERROR:
		return nil, status.Error(codes.InvalidArgument, strings.Join(resp.GetMessages(), "\n"))
	case notifications.RuleUpdateResponse_NOT_FOUND:
		return nil, status.Error(codes.NotFound, strings.Join(resp.GetMessages(), "\n"))
	default:
		return nil, status.Error(codes.Internal, "Internal Error")
	}
}

func (s *notificationsServer) DeleteRule(ctx context.Context, in *pb.RuleIdentifier) (*pb.RuleDeleteResponse, error) {

	resp, err := s.notificationsClient.DeleteRule(ctx, &notifications.RuleIdentifier{Id: in.GetId()})
	if err != nil {
		return nil, err
	}

	switch resp.GetCode() {
	case notifications.RuleDeleteResponse_DELETED:
		return &pb.RuleDeleteResponse{Messages: resp.Messages}, nil
	case notifications.RuleDeleteResponse_NOT_FOUND:
		return nil, status.Error(codes.NotFound, strings.Join(resp.GetMessages(), "\n"))
	default:
		return nil, status.Error(codes.Internal, "Internal Error")
	}
}

func (s *notificationsServer) GetRule(ctx context.Context, in *pb.RuleIdentifier) (*pb.RuleGetResponse, error) {
	resp, err := s.notificationsClient.GetRule(ctx, &notifications.RuleIdentifier{Id: in.GetId()})
	if err != nil {
		return nil, err
	}

	switch resp.GetCode() {
	case notifications.RuleGetResponse_OK:
		var transformedResp pb.RuleGetResponse
		err := transform(resp, &transformedResp)
		if err != nil {
			return nil, err
		}
		return &transformedResp, nil
	case notifications.RuleGetResponse_NOT_FOUND:
		return nil, status.Error(codes.NotFound, strings.Join(resp.GetMessages(), "\n"))
	default:
		return nil, status.Error(codes.Internal, "Internal Error")
	}
}

func (s *notificationsServer) ListRules(ctx context.Context, _ *pb.RuleListRequest) (*pb.RuleListResponse, error) {
	resp, err := s.notificationsClient.ListRules(ctx, &notifications.Empty{})
	if err != nil {
		return nil, err
	}

	switch resp.GetCode() {
	case notifications.RuleListResponse_OK:
		var transformedResp pb.RuleListResponse
		err := transform(resp, &transformedResp)
		if err != nil {
			return nil, err
		}
		return &transformedResp, nil
	default:
		return nil, status.Error(codes.Internal, "Internal Error")
	}
}

func (s *notificationsServer) ValidateWebhook(ctx context.Context,
	in *pb.URLValidationRequest) (*pb.URLValidationResponse, error) {
	var transformUVR notifications.URLValidationRequest
	err := transform(in, &transformUVR)
	if err != nil {
		return nil, err
	}

	resp, err := s.notificationsClient.ValidateWebhook(ctx, &transformUVR)
	if err != nil {
		return nil, err
	}

	switch resp.GetCode() {
	case notifications.URLValidationResponse_OK:
		return &pb.URLValidationResponse{}, nil
	case notifications.URLValidationResponse_ERROR, notifications.URLValidationResponse_INVALID_URL:
		return nil, status.Error(codes.InvalidArgument, "Could not validate url")
	default:
		return nil, status.Error(codes.Internal, "Internal Error")
	}
}

func (s *notificationsServer) Version(ctx context.Context, _ *pb.VersionRequest) (*pb.VersionResponse, error) {
	resp, err := s.notificationsClient.Version(ctx, &notifications.VersionRequest{})
	if err != nil {
		return nil, err
	}

	return &pb.VersionResponse{Version: resp.Version}, nil
}

func transform(in proto.Message, out proto.Message) error {
	if reflect.ValueOf(in).IsNil() {
		return status.Error(codes.InvalidArgument, "Invalid request")
	}

	str, err := (&jsonpb.Marshaler{OrigName: true}).MarshalToString(in)

	if err != nil {
		return err
	}

	unmarshaler := &jsonpb.Unmarshaler{AllowUnknownFields: true}
	err = unmarshaler.Unmarshal(strings.NewReader(str), out)

	if err != nil {
		return err
	}

	return nil
}
