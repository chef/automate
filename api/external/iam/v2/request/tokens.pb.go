// Code generated by protoc-gen-go. DO NOT EDIT.
// versions:
// 	protoc-gen-go v1.36.0
// 	protoc        v3.19.0
// source: external/iam/v2/request/tokens.proto

package request

import (
	_ "github.com/grpc-ecosystem/grpc-gateway/protoc-gen-swagger/options"
	protoreflect "google.golang.org/protobuf/reflect/protoreflect"
	protoimpl "google.golang.org/protobuf/runtime/protoimpl"
	wrapperspb "google.golang.org/protobuf/types/known/wrapperspb"
	reflect "reflect"
	sync "sync"
)

const (
	// Verify that this generated code is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(20 - protoimpl.MinVersion)
	// Verify that runtime/protoimpl is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(protoimpl.MaxVersion - 20)
)

type CreateTokenReq struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// Unique ID. Cannot be changed.
	Id string `protobuf:"bytes,1,opt,name=id,proto3" json:"id,omitempty"`
	// Name for the token.
	Name string `protobuf:"bytes,2,opt,name=name,proto3" json:"name,omitempty"`
	// Active state. Defaults to true.
	// If set to false, token will not be authenticated or authorized.
	Active *wrapperspb.BoolValue `protobuf:"bytes,3,opt,name=active,proto3" json:"active,omitempty"`
	// Unique value for the token; if omitted the system will generate this.
	Value string `protobuf:"bytes,4,opt,name=value,proto3" json:"value,omitempty"`
	// List of projects this token belongs to.
	Projects      []string `protobuf:"bytes,5,rep,name=projects,proto3" json:"projects,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *CreateTokenReq) Reset() {
	*x = CreateTokenReq{}
	mi := &file_external_iam_v2_request_tokens_proto_msgTypes[0]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *CreateTokenReq) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*CreateTokenReq) ProtoMessage() {}

func (x *CreateTokenReq) ProtoReflect() protoreflect.Message {
	mi := &file_external_iam_v2_request_tokens_proto_msgTypes[0]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use CreateTokenReq.ProtoReflect.Descriptor instead.
func (*CreateTokenReq) Descriptor() ([]byte, []int) {
	return file_external_iam_v2_request_tokens_proto_rawDescGZIP(), []int{0}
}

func (x *CreateTokenReq) GetId() string {
	if x != nil {
		return x.Id
	}
	return ""
}

func (x *CreateTokenReq) GetName() string {
	if x != nil {
		return x.Name
	}
	return ""
}

func (x *CreateTokenReq) GetActive() *wrapperspb.BoolValue {
	if x != nil {
		return x.Active
	}
	return nil
}

func (x *CreateTokenReq) GetValue() string {
	if x != nil {
		return x.Value
	}
	return ""
}

func (x *CreateTokenReq) GetProjects() []string {
	if x != nil {
		return x.Projects
	}
	return nil
}

type GetTokenReq struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// ID of the token.
	Id            string `protobuf:"bytes,1,opt,name=id,proto3" json:"id,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *GetTokenReq) Reset() {
	*x = GetTokenReq{}
	mi := &file_external_iam_v2_request_tokens_proto_msgTypes[1]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *GetTokenReq) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*GetTokenReq) ProtoMessage() {}

func (x *GetTokenReq) ProtoReflect() protoreflect.Message {
	mi := &file_external_iam_v2_request_tokens_proto_msgTypes[1]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use GetTokenReq.ProtoReflect.Descriptor instead.
func (*GetTokenReq) Descriptor() ([]byte, []int) {
	return file_external_iam_v2_request_tokens_proto_rawDescGZIP(), []int{1}
}

func (x *GetTokenReq) GetId() string {
	if x != nil {
		return x.Id
	}
	return ""
}

type UpdateTokenReq struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// Unique ID. Cannot be changed.
	Id string `protobuf:"bytes,1,opt,name=id,proto3" json:"id,omitempty"`
	// Name for the token.
	Name string `protobuf:"bytes,2,opt,name=name,proto3" json:"name,omitempty"`
	// Active state. Defaults to true.
	// If set to false, token will not be authenticated or authorized.
	Active *wrapperspb.BoolValue `protobuf:"bytes,3,opt,name=active,proto3" json:"active,omitempty"`
	// List of projects this token belongs to.
	Projects      []string `protobuf:"bytes,5,rep,name=projects,proto3" json:"projects,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *UpdateTokenReq) Reset() {
	*x = UpdateTokenReq{}
	mi := &file_external_iam_v2_request_tokens_proto_msgTypes[2]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *UpdateTokenReq) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*UpdateTokenReq) ProtoMessage() {}

func (x *UpdateTokenReq) ProtoReflect() protoreflect.Message {
	mi := &file_external_iam_v2_request_tokens_proto_msgTypes[2]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use UpdateTokenReq.ProtoReflect.Descriptor instead.
func (*UpdateTokenReq) Descriptor() ([]byte, []int) {
	return file_external_iam_v2_request_tokens_proto_rawDescGZIP(), []int{2}
}

func (x *UpdateTokenReq) GetId() string {
	if x != nil {
		return x.Id
	}
	return ""
}

func (x *UpdateTokenReq) GetName() string {
	if x != nil {
		return x.Name
	}
	return ""
}

func (x *UpdateTokenReq) GetActive() *wrapperspb.BoolValue {
	if x != nil {
		return x.Active
	}
	return nil
}

func (x *UpdateTokenReq) GetProjects() []string {
	if x != nil {
		return x.Projects
	}
	return nil
}

type DeleteTokenReq struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// ID of the token.
	Id            string `protobuf:"bytes,1,opt,name=id,proto3" json:"id,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *DeleteTokenReq) Reset() {
	*x = DeleteTokenReq{}
	mi := &file_external_iam_v2_request_tokens_proto_msgTypes[3]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *DeleteTokenReq) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*DeleteTokenReq) ProtoMessage() {}

func (x *DeleteTokenReq) ProtoReflect() protoreflect.Message {
	mi := &file_external_iam_v2_request_tokens_proto_msgTypes[3]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use DeleteTokenReq.ProtoReflect.Descriptor instead.
func (*DeleteTokenReq) Descriptor() ([]byte, []int) {
	return file_external_iam_v2_request_tokens_proto_rawDescGZIP(), []int{3}
}

func (x *DeleteTokenReq) GetId() string {
	if x != nil {
		return x.Id
	}
	return ""
}

type ListTokensReq struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *ListTokensReq) Reset() {
	*x = ListTokensReq{}
	mi := &file_external_iam_v2_request_tokens_proto_msgTypes[4]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ListTokensReq) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ListTokensReq) ProtoMessage() {}

func (x *ListTokensReq) ProtoReflect() protoreflect.Message {
	mi := &file_external_iam_v2_request_tokens_proto_msgTypes[4]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ListTokensReq.ProtoReflect.Descriptor instead.
func (*ListTokensReq) Descriptor() ([]byte, []int) {
	return file_external_iam_v2_request_tokens_proto_rawDescGZIP(), []int{4}
}

var File_external_iam_v2_request_tokens_proto protoreflect.FileDescriptor

var file_external_iam_v2_request_tokens_proto_rawDesc = []byte{
	0x0a, 0x24, 0x65, 0x78, 0x74, 0x65, 0x72, 0x6e, 0x61, 0x6c, 0x2f, 0x69, 0x61, 0x6d, 0x2f, 0x76,
	0x32, 0x2f, 0x72, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x2f, 0x74, 0x6f, 0x6b, 0x65, 0x6e, 0x73,
	0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x12, 0x18, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x61, 0x75, 0x74,
	0x6f, 0x6d, 0x61, 0x74, 0x65, 0x2e, 0x61, 0x70, 0x69, 0x2e, 0x69, 0x61, 0x6d, 0x2e, 0x76, 0x32,
	0x1a, 0x1e, 0x67, 0x6f, 0x6f, 0x67, 0x6c, 0x65, 0x2f, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x62, 0x75,
	0x66, 0x2f, 0x77, 0x72, 0x61, 0x70, 0x70, 0x65, 0x72, 0x73, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f,
	0x1a, 0x2c, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x63, 0x2d, 0x67, 0x65, 0x6e, 0x2d, 0x73, 0x77, 0x61,
	0x67, 0x67, 0x65, 0x72, 0x2f, 0x6f, 0x70, 0x74, 0x69, 0x6f, 0x6e, 0x73, 0x2f, 0x61, 0x6e, 0x6e,
	0x6f, 0x74, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x73, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x22, 0x91,
	0x02, 0x0a, 0x0e, 0x43, 0x72, 0x65, 0x61, 0x74, 0x65, 0x54, 0x6f, 0x6b, 0x65, 0x6e, 0x52, 0x65,
	0x71, 0x12, 0x0e, 0x0a, 0x02, 0x69, 0x64, 0x18, 0x01, 0x20, 0x01, 0x28, 0x09, 0x52, 0x02, 0x69,
	0x64, 0x12, 0x12, 0x0a, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x18, 0x02, 0x20, 0x01, 0x28, 0x09, 0x52,
	0x04, 0x6e, 0x61, 0x6d, 0x65, 0x12, 0x32, 0x0a, 0x06, 0x61, 0x63, 0x74, 0x69, 0x76, 0x65, 0x18,
	0x03, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x1a, 0x2e, 0x67, 0x6f, 0x6f, 0x67, 0x6c, 0x65, 0x2e, 0x70,
	0x72, 0x6f, 0x74, 0x6f, 0x62, 0x75, 0x66, 0x2e, 0x42, 0x6f, 0x6f, 0x6c, 0x56, 0x61, 0x6c, 0x75,
	0x65, 0x52, 0x06, 0x61, 0x63, 0x74, 0x69, 0x76, 0x65, 0x12, 0x14, 0x0a, 0x05, 0x76, 0x61, 0x6c,
	0x75, 0x65, 0x18, 0x04, 0x20, 0x01, 0x28, 0x09, 0x52, 0x05, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x12,
	0x1a, 0x0a, 0x08, 0x70, 0x72, 0x6f, 0x6a, 0x65, 0x63, 0x74, 0x73, 0x18, 0x05, 0x20, 0x03, 0x28,
	0x09, 0x52, 0x08, 0x70, 0x72, 0x6f, 0x6a, 0x65, 0x63, 0x74, 0x73, 0x3a, 0x75, 0x92, 0x41, 0x72,
	0x0a, 0x0c, 0xd2, 0x01, 0x02, 0x69, 0x64, 0xd2, 0x01, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x32, 0x62,
	0x12, 0x60, 0x7b, 0x22, 0x6e, 0x61, 0x6d, 0x65, 0x22, 0x3a, 0x20, 0x22, 0x74, 0x6f, 0x6b, 0x65,
	0x6e, 0x20, 0x31, 0x22, 0x2c, 0x20, 0x22, 0x69, 0x64, 0x22, 0x3a, 0x20, 0x22, 0x74, 0x6f, 0x6b,
	0x65, 0x6e, 0x2d, 0x31, 0x22, 0x2c, 0x20, 0x22, 0x61, 0x63, 0x74, 0x69, 0x76, 0x65, 0x22, 0x3a,
	0x20, 0x74, 0x72, 0x75, 0x65, 0x2c, 0x20, 0x22, 0x70, 0x72, 0x6f, 0x6a, 0x65, 0x63, 0x74, 0x73,
	0x22, 0x3a, 0x20, 0x5b, 0x22, 0x65, 0x61, 0x73, 0x74, 0x2d, 0x72, 0x65, 0x67, 0x69, 0x6f, 0x6e,
	0x22, 0x2c, 0x20, 0x22, 0x77, 0x65, 0x73, 0x74, 0x2d, 0x72, 0x65, 0x67, 0x69, 0x6f, 0x6e, 0x22,
	0x5d, 0x7d, 0x22, 0x1d, 0x0a, 0x0b, 0x47, 0x65, 0x74, 0x54, 0x6f, 0x6b, 0x65, 0x6e, 0x52, 0x65,
	0x71, 0x12, 0x0e, 0x0a, 0x02, 0x69, 0x64, 0x18, 0x01, 0x20, 0x01, 0x28, 0x09, 0x52, 0x02, 0x69,
	0x64, 0x22, 0xf1, 0x01, 0x0a, 0x0e, 0x55, 0x70, 0x64, 0x61, 0x74, 0x65, 0x54, 0x6f, 0x6b, 0x65,
	0x6e, 0x52, 0x65, 0x71, 0x12, 0x0e, 0x0a, 0x02, 0x69, 0x64, 0x18, 0x01, 0x20, 0x01, 0x28, 0x09,
	0x52, 0x02, 0x69, 0x64, 0x12, 0x12, 0x0a, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x18, 0x02, 0x20, 0x01,
	0x28, 0x09, 0x52, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x12, 0x32, 0x0a, 0x06, 0x61, 0x63, 0x74, 0x69,
	0x76, 0x65, 0x18, 0x03, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x1a, 0x2e, 0x67, 0x6f, 0x6f, 0x67, 0x6c,
	0x65, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x62, 0x75, 0x66, 0x2e, 0x42, 0x6f, 0x6f, 0x6c, 0x56,
	0x61, 0x6c, 0x75, 0x65, 0x52, 0x06, 0x61, 0x63, 0x74, 0x69, 0x76, 0x65, 0x12, 0x1a, 0x0a, 0x08,
	0x70, 0x72, 0x6f, 0x6a, 0x65, 0x63, 0x74, 0x73, 0x18, 0x05, 0x20, 0x03, 0x28, 0x09, 0x52, 0x08,
	0x70, 0x72, 0x6f, 0x6a, 0x65, 0x63, 0x74, 0x73, 0x3a, 0x6b, 0x92, 0x41, 0x68, 0x0a, 0x07, 0xd2,
	0x01, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x32, 0x5d, 0x12, 0x5b, 0x7b, 0x22, 0x6e, 0x61, 0x6d, 0x65,
	0x22, 0x3a, 0x20, 0x22, 0x75, 0x70, 0x64, 0x61, 0x74, 0x65, 0x64, 0x20, 0x74, 0x6f, 0x6b, 0x65,
	0x6e, 0x20, 0x6e, 0x61, 0x6d, 0x65, 0x22, 0x2c, 0x20, 0x22, 0x61, 0x63, 0x74, 0x69, 0x76, 0x65,
	0x22, 0x3a, 0x20, 0x74, 0x72, 0x75, 0x65, 0x2c, 0x20, 0x22, 0x70, 0x72, 0x6f, 0x6a, 0x65, 0x63,
	0x74, 0x73, 0x22, 0x3a, 0x20, 0x5b, 0x22, 0x65, 0x61, 0x73, 0x74, 0x2d, 0x72, 0x65, 0x67, 0x69,
	0x6f, 0x6e, 0x22, 0x2c, 0x20, 0x22, 0x73, 0x6f, 0x75, 0x74, 0x68, 0x2d, 0x72, 0x65, 0x67, 0x69,
	0x6f, 0x6e, 0x22, 0x5d, 0x7d, 0x22, 0x20, 0x0a, 0x0e, 0x44, 0x65, 0x6c, 0x65, 0x74, 0x65, 0x54,
	0x6f, 0x6b, 0x65, 0x6e, 0x52, 0x65, 0x71, 0x12, 0x0e, 0x0a, 0x02, 0x69, 0x64, 0x18, 0x01, 0x20,
	0x01, 0x28, 0x09, 0x52, 0x02, 0x69, 0x64, 0x22, 0x0f, 0x0a, 0x0d, 0x4c, 0x69, 0x73, 0x74, 0x54,
	0x6f, 0x6b, 0x65, 0x6e, 0x73, 0x52, 0x65, 0x71, 0x42, 0x36, 0x5a, 0x34, 0x67, 0x69, 0x74, 0x68,
	0x75, 0x62, 0x2e, 0x63, 0x6f, 0x6d, 0x2f, 0x63, 0x68, 0x65, 0x66, 0x2f, 0x61, 0x75, 0x74, 0x6f,
	0x6d, 0x61, 0x74, 0x65, 0x2f, 0x61, 0x70, 0x69, 0x2f, 0x65, 0x78, 0x74, 0x65, 0x72, 0x6e, 0x61,
	0x6c, 0x2f, 0x69, 0x61, 0x6d, 0x2f, 0x76, 0x32, 0x2f, 0x72, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74,
	0x62, 0x06, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x33,
}

var (
	file_external_iam_v2_request_tokens_proto_rawDescOnce sync.Once
	file_external_iam_v2_request_tokens_proto_rawDescData = file_external_iam_v2_request_tokens_proto_rawDesc
)

func file_external_iam_v2_request_tokens_proto_rawDescGZIP() []byte {
	file_external_iam_v2_request_tokens_proto_rawDescOnce.Do(func() {
		file_external_iam_v2_request_tokens_proto_rawDescData = protoimpl.X.CompressGZIP(file_external_iam_v2_request_tokens_proto_rawDescData)
	})
	return file_external_iam_v2_request_tokens_proto_rawDescData
}

var file_external_iam_v2_request_tokens_proto_msgTypes = make([]protoimpl.MessageInfo, 5)
var file_external_iam_v2_request_tokens_proto_goTypes = []any{
	(*CreateTokenReq)(nil),       // 0: chef.automate.api.iam.v2.CreateTokenReq
	(*GetTokenReq)(nil),          // 1: chef.automate.api.iam.v2.GetTokenReq
	(*UpdateTokenReq)(nil),       // 2: chef.automate.api.iam.v2.UpdateTokenReq
	(*DeleteTokenReq)(nil),       // 3: chef.automate.api.iam.v2.DeleteTokenReq
	(*ListTokensReq)(nil),        // 4: chef.automate.api.iam.v2.ListTokensReq
	(*wrapperspb.BoolValue)(nil), // 5: google.protobuf.BoolValue
}
var file_external_iam_v2_request_tokens_proto_depIdxs = []int32{
	5, // 0: chef.automate.api.iam.v2.CreateTokenReq.active:type_name -> google.protobuf.BoolValue
	5, // 1: chef.automate.api.iam.v2.UpdateTokenReq.active:type_name -> google.protobuf.BoolValue
	2, // [2:2] is the sub-list for method output_type
	2, // [2:2] is the sub-list for method input_type
	2, // [2:2] is the sub-list for extension type_name
	2, // [2:2] is the sub-list for extension extendee
	0, // [0:2] is the sub-list for field type_name
}

func init() { file_external_iam_v2_request_tokens_proto_init() }
func file_external_iam_v2_request_tokens_proto_init() {
	if File_external_iam_v2_request_tokens_proto != nil {
		return
	}
	type x struct{}
	out := protoimpl.TypeBuilder{
		File: protoimpl.DescBuilder{
			GoPackagePath: reflect.TypeOf(x{}).PkgPath(),
			RawDescriptor: file_external_iam_v2_request_tokens_proto_rawDesc,
			NumEnums:      0,
			NumMessages:   5,
			NumExtensions: 0,
			NumServices:   0,
		},
		GoTypes:           file_external_iam_v2_request_tokens_proto_goTypes,
		DependencyIndexes: file_external_iam_v2_request_tokens_proto_depIdxs,
		MessageInfos:      file_external_iam_v2_request_tokens_proto_msgTypes,
	}.Build()
	File_external_iam_v2_request_tokens_proto = out.File
	file_external_iam_v2_request_tokens_proto_rawDesc = nil
	file_external_iam_v2_request_tokens_proto_goTypes = nil
	file_external_iam_v2_request_tokens_proto_depIdxs = nil
}
