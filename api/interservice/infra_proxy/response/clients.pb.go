// Code generated by protoc-gen-go. DO NOT EDIT.
// versions:
// 	protoc-gen-go v1.36.0
// 	protoc        v3.19.0
// source: interservice/infra_proxy/response/clients.proto

package response

import (
	protoreflect "google.golang.org/protobuf/reflect/protoreflect"
	protoimpl "google.golang.org/protobuf/runtime/protoimpl"
	reflect "reflect"
	sync "sync"
)

const (
	// Verify that this generated code is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(20 - protoimpl.MinVersion)
	// Verify that runtime/protoimpl is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(protoimpl.MaxVersion - 20)
)

type Clients struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// Client list.
	Clients []*ClientListItem `protobuf:"bytes,1,rep,name=clients,proto3" json:"clients,omitempty" toml:"clients,omitempty" mapstructure:"clients,omitempty"`
	// Starting page for the results.
	Page int32 `protobuf:"varint,2,opt,name=page,proto3" json:"page,omitempty" toml:"page,omitempty" mapstructure:"page,omitempty"`
	// Total number of records.
	Total         int32 `protobuf:"varint,3,opt,name=total,proto3" json:"total,omitempty" toml:"total,omitempty" mapstructure:"total,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *Clients) Reset() {
	*x = Clients{}
	mi := &file_interservice_infra_proxy_response_clients_proto_msgTypes[0]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *Clients) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*Clients) ProtoMessage() {}

func (x *Clients) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_infra_proxy_response_clients_proto_msgTypes[0]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use Clients.ProtoReflect.Descriptor instead.
func (*Clients) Descriptor() ([]byte, []int) {
	return file_interservice_infra_proxy_response_clients_proto_rawDescGZIP(), []int{0}
}

func (x *Clients) GetClients() []*ClientListItem {
	if x != nil {
		return x.Clients
	}
	return nil
}

func (x *Clients) GetPage() int32 {
	if x != nil {
		return x.Page
	}
	return 0
}

func (x *Clients) GetTotal() int32 {
	if x != nil {
		return x.Total
	}
	return 0
}

type ClientListItem struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// Client name.
	Name string `protobuf:"bytes,1,opt,name=name,proto3" json:"name,omitempty" toml:"name,omitempty" mapstructure:"name,omitempty"`
	// Boolean indicates client type is validator or not.
	Validator     bool `protobuf:"varint,2,opt,name=validator,proto3" json:"validator,omitempty" toml:"validator,omitempty" mapstructure:"validator,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *ClientListItem) Reset() {
	*x = ClientListItem{}
	mi := &file_interservice_infra_proxy_response_clients_proto_msgTypes[1]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ClientListItem) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ClientListItem) ProtoMessage() {}

func (x *ClientListItem) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_infra_proxy_response_clients_proto_msgTypes[1]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ClientListItem.ProtoReflect.Descriptor instead.
func (*ClientListItem) Descriptor() ([]byte, []int) {
	return file_interservice_infra_proxy_response_clients_proto_rawDescGZIP(), []int{1}
}

func (x *ClientListItem) GetName() string {
	if x != nil {
		return x.Name
	}
	return ""
}

func (x *ClientListItem) GetValidator() bool {
	if x != nil {
		return x.Validator
	}
	return false
}

type Client struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// Client name.
	Name string `protobuf:"bytes,1,opt,name=name,proto3" json:"name,omitempty" toml:"name,omitempty" mapstructure:"name,omitempty"`
	// Client name return by Chef Infra Server API.
	ClientName string `protobuf:"bytes,2,opt,name=client_name,json=clientName,proto3" json:"client_name,omitempty" toml:"client_name,omitempty" mapstructure:"client_name,omitempty"`
	// Chef organization name.
	OrgName string `protobuf:"bytes,3,opt,name=org_name,json=orgName,proto3" json:"org_name,omitempty" toml:"org_name,omitempty" mapstructure:"org_name,omitempty"`
	// Boolean indicates client type is validator or not.
	Validator bool `protobuf:"varint,4,opt,name=validator,proto3" json:"validator,omitempty" toml:"validator,omitempty" mapstructure:"validator,omitempty"`
	// Client JSON class.
	JsonClass string `protobuf:"bytes,5,opt,name=json_class,json=jsonClass,proto3" json:"json_class,omitempty" toml:"json_class,omitempty" mapstructure:"json_class,omitempty"`
	// Chef object type.
	ChefType string `protobuf:"bytes,6,opt,name=chef_type,json=chefType,proto3" json:"chef_type,omitempty" toml:"chef_type,omitempty" mapstructure:"chef_type,omitempty"`
	// Client key detail.
	ClientKey     *ClientAccessKey `protobuf:"bytes,7,opt,name=client_key,json=clientKey,proto3" json:"client_key,omitempty" toml:"client_key,omitempty" mapstructure:"client_key,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *Client) Reset() {
	*x = Client{}
	mi := &file_interservice_infra_proxy_response_clients_proto_msgTypes[2]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *Client) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*Client) ProtoMessage() {}

func (x *Client) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_infra_proxy_response_clients_proto_msgTypes[2]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use Client.ProtoReflect.Descriptor instead.
func (*Client) Descriptor() ([]byte, []int) {
	return file_interservice_infra_proxy_response_clients_proto_rawDescGZIP(), []int{2}
}

func (x *Client) GetName() string {
	if x != nil {
		return x.Name
	}
	return ""
}

func (x *Client) GetClientName() string {
	if x != nil {
		return x.ClientName
	}
	return ""
}

func (x *Client) GetOrgName() string {
	if x != nil {
		return x.OrgName
	}
	return ""
}

func (x *Client) GetValidator() bool {
	if x != nil {
		return x.Validator
	}
	return false
}

func (x *Client) GetJsonClass() string {
	if x != nil {
		return x.JsonClass
	}
	return ""
}

func (x *Client) GetChefType() string {
	if x != nil {
		return x.ChefType
	}
	return ""
}

func (x *Client) GetClientKey() *ClientAccessKey {
	if x != nil {
		return x.ClientKey
	}
	return nil
}

type CreateClient struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// Client name.
	Name string `protobuf:"bytes,1,opt,name=name,proto3" json:"name,omitempty" toml:"name,omitempty" mapstructure:"name,omitempty"`
	// Client key detail.
	ClientKey     *ClientKey `protobuf:"bytes,2,opt,name=client_key,json=clientKey,proto3" json:"client_key,omitempty" toml:"client_key,omitempty" mapstructure:"client_key,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *CreateClient) Reset() {
	*x = CreateClient{}
	mi := &file_interservice_infra_proxy_response_clients_proto_msgTypes[3]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *CreateClient) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*CreateClient) ProtoMessage() {}

func (x *CreateClient) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_infra_proxy_response_clients_proto_msgTypes[3]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use CreateClient.ProtoReflect.Descriptor instead.
func (*CreateClient) Descriptor() ([]byte, []int) {
	return file_interservice_infra_proxy_response_clients_proto_rawDescGZIP(), []int{3}
}

func (x *CreateClient) GetName() string {
	if x != nil {
		return x.Name
	}
	return ""
}

func (x *CreateClient) GetClientKey() *ClientKey {
	if x != nil {
		return x.ClientKey
	}
	return nil
}

type ResetClient struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// Client name.
	Name string `protobuf:"bytes,1,opt,name=name,proto3" json:"name,omitempty" toml:"name,omitempty" mapstructure:"name,omitempty"`
	// Client key detail.
	ClientKey     *ClientKey `protobuf:"bytes,2,opt,name=client_key,json=clientKey,proto3" json:"client_key,omitempty" toml:"client_key,omitempty" mapstructure:"client_key,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *ResetClient) Reset() {
	*x = ResetClient{}
	mi := &file_interservice_infra_proxy_response_clients_proto_msgTypes[4]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ResetClient) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ResetClient) ProtoMessage() {}

func (x *ResetClient) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_infra_proxy_response_clients_proto_msgTypes[4]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ResetClient.ProtoReflect.Descriptor instead.
func (*ResetClient) Descriptor() ([]byte, []int) {
	return file_interservice_infra_proxy_response_clients_proto_rawDescGZIP(), []int{4}
}

func (x *ResetClient) GetName() string {
	if x != nil {
		return x.Name
	}
	return ""
}

func (x *ResetClient) GetClientKey() *ClientKey {
	if x != nil {
		return x.ClientKey
	}
	return nil
}

type ClientKey struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// Client key name.
	Name string `protobuf:"bytes,1,opt,name=name,proto3" json:"name,omitempty" toml:"name,omitempty" mapstructure:"name,omitempty"`
	// Client public key.
	PublicKey string `protobuf:"bytes,2,opt,name=public_key,json=publicKey,proto3" json:"public_key,omitempty" toml:"public_key,omitempty" mapstructure:"public_key,omitempty"`
	// Client key expiration date string.
	ExpirationDate string `protobuf:"bytes,3,opt,name=expiration_date,json=expirationDate,proto3" json:"expiration_date,omitempty" toml:"expiration_date,omitempty" mapstructure:"expiration_date,omitempty"`
	// Client private key.
	PrivateKey    string `protobuf:"bytes,4,opt,name=private_key,json=privateKey,proto3" json:"private_key,omitempty" toml:"private_key,omitempty" mapstructure:"private_key,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *ClientKey) Reset() {
	*x = ClientKey{}
	mi := &file_interservice_infra_proxy_response_clients_proto_msgTypes[5]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ClientKey) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ClientKey) ProtoMessage() {}

func (x *ClientKey) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_infra_proxy_response_clients_proto_msgTypes[5]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ClientKey.ProtoReflect.Descriptor instead.
func (*ClientKey) Descriptor() ([]byte, []int) {
	return file_interservice_infra_proxy_response_clients_proto_rawDescGZIP(), []int{5}
}

func (x *ClientKey) GetName() string {
	if x != nil {
		return x.Name
	}
	return ""
}

func (x *ClientKey) GetPublicKey() string {
	if x != nil {
		return x.PublicKey
	}
	return ""
}

func (x *ClientKey) GetExpirationDate() string {
	if x != nil {
		return x.ExpirationDate
	}
	return ""
}

func (x *ClientKey) GetPrivateKey() string {
	if x != nil {
		return x.PrivateKey
	}
	return ""
}

type ClientAccessKey struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// Client key name.
	Name string `protobuf:"bytes,1,opt,name=name,proto3" json:"name,omitempty" toml:"name,omitempty" mapstructure:"name,omitempty"`
	// Client public key.
	PublicKey string `protobuf:"bytes,2,opt,name=public_key,json=publicKey,proto3" json:"public_key,omitempty" toml:"public_key,omitempty" mapstructure:"public_key,omitempty"`
	// Client key expiration date string.
	ExpirationDate string `protobuf:"bytes,3,opt,name=expiration_date,json=expirationDate,proto3" json:"expiration_date,omitempty" toml:"expiration_date,omitempty" mapstructure:"expiration_date,omitempty"`
	unknownFields  protoimpl.UnknownFields
	sizeCache      protoimpl.SizeCache
}

func (x *ClientAccessKey) Reset() {
	*x = ClientAccessKey{}
	mi := &file_interservice_infra_proxy_response_clients_proto_msgTypes[6]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ClientAccessKey) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ClientAccessKey) ProtoMessage() {}

func (x *ClientAccessKey) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_infra_proxy_response_clients_proto_msgTypes[6]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ClientAccessKey.ProtoReflect.Descriptor instead.
func (*ClientAccessKey) Descriptor() ([]byte, []int) {
	return file_interservice_infra_proxy_response_clients_proto_rawDescGZIP(), []int{6}
}

func (x *ClientAccessKey) GetName() string {
	if x != nil {
		return x.Name
	}
	return ""
}

func (x *ClientAccessKey) GetPublicKey() string {
	if x != nil {
		return x.PublicKey
	}
	return ""
}

func (x *ClientAccessKey) GetExpirationDate() string {
	if x != nil {
		return x.ExpirationDate
	}
	return ""
}

var File_interservice_infra_proxy_response_clients_proto protoreflect.FileDescriptor

var file_interservice_infra_proxy_response_clients_proto_rawDesc = []byte{
	0x0a, 0x2f, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x73, 0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x2f, 0x69,
	0x6e, 0x66, 0x72, 0x61, 0x5f, 0x70, 0x72, 0x6f, 0x78, 0x79, 0x2f, 0x72, 0x65, 0x73, 0x70, 0x6f,
	0x6e, 0x73, 0x65, 0x2f, 0x63, 0x6c, 0x69, 0x65, 0x6e, 0x74, 0x73, 0x2e, 0x70, 0x72, 0x6f, 0x74,
	0x6f, 0x12, 0x29, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x61, 0x75, 0x74, 0x6f, 0x6d, 0x61, 0x74, 0x65,
	0x2e, 0x64, 0x6f, 0x6d, 0x61, 0x69, 0x6e, 0x2e, 0x69, 0x6e, 0x66, 0x72, 0x61, 0x5f, 0x70, 0x72,
	0x6f, 0x78, 0x79, 0x2e, 0x72, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x22, 0x88, 0x01, 0x0a,
	0x07, 0x43, 0x6c, 0x69, 0x65, 0x6e, 0x74, 0x73, 0x12, 0x53, 0x0a, 0x07, 0x63, 0x6c, 0x69, 0x65,
	0x6e, 0x74, 0x73, 0x18, 0x01, 0x20, 0x03, 0x28, 0x0b, 0x32, 0x39, 0x2e, 0x63, 0x68, 0x65, 0x66,
	0x2e, 0x61, 0x75, 0x74, 0x6f, 0x6d, 0x61, 0x74, 0x65, 0x2e, 0x64, 0x6f, 0x6d, 0x61, 0x69, 0x6e,
	0x2e, 0x69, 0x6e, 0x66, 0x72, 0x61, 0x5f, 0x70, 0x72, 0x6f, 0x78, 0x79, 0x2e, 0x72, 0x65, 0x73,
	0x70, 0x6f, 0x6e, 0x73, 0x65, 0x2e, 0x43, 0x6c, 0x69, 0x65, 0x6e, 0x74, 0x4c, 0x69, 0x73, 0x74,
	0x49, 0x74, 0x65, 0x6d, 0x52, 0x07, 0x63, 0x6c, 0x69, 0x65, 0x6e, 0x74, 0x73, 0x12, 0x12, 0x0a,
	0x04, 0x70, 0x61, 0x67, 0x65, 0x18, 0x02, 0x20, 0x01, 0x28, 0x05, 0x52, 0x04, 0x70, 0x61, 0x67,
	0x65, 0x12, 0x14, 0x0a, 0x05, 0x74, 0x6f, 0x74, 0x61, 0x6c, 0x18, 0x03, 0x20, 0x01, 0x28, 0x05,
	0x52, 0x05, 0x74, 0x6f, 0x74, 0x61, 0x6c, 0x22, 0x42, 0x0a, 0x0e, 0x43, 0x6c, 0x69, 0x65, 0x6e,
	0x74, 0x4c, 0x69, 0x73, 0x74, 0x49, 0x74, 0x65, 0x6d, 0x12, 0x12, 0x0a, 0x04, 0x6e, 0x61, 0x6d,
	0x65, 0x18, 0x01, 0x20, 0x01, 0x28, 0x09, 0x52, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x12, 0x1c, 0x0a,
	0x09, 0x76, 0x61, 0x6c, 0x69, 0x64, 0x61, 0x74, 0x6f, 0x72, 0x18, 0x02, 0x20, 0x01, 0x28, 0x08,
	0x52, 0x09, 0x76, 0x61, 0x6c, 0x69, 0x64, 0x61, 0x74, 0x6f, 0x72, 0x22, 0x8d, 0x02, 0x0a, 0x06,
	0x43, 0x6c, 0x69, 0x65, 0x6e, 0x74, 0x12, 0x12, 0x0a, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x18, 0x01,
	0x20, 0x01, 0x28, 0x09, 0x52, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x12, 0x1f, 0x0a, 0x0b, 0x63, 0x6c,
	0x69, 0x65, 0x6e, 0x74, 0x5f, 0x6e, 0x61, 0x6d, 0x65, 0x18, 0x02, 0x20, 0x01, 0x28, 0x09, 0x52,
	0x0a, 0x63, 0x6c, 0x69, 0x65, 0x6e, 0x74, 0x4e, 0x61, 0x6d, 0x65, 0x12, 0x19, 0x0a, 0x08, 0x6f,
	0x72, 0x67, 0x5f, 0x6e, 0x61, 0x6d, 0x65, 0x18, 0x03, 0x20, 0x01, 0x28, 0x09, 0x52, 0x07, 0x6f,
	0x72, 0x67, 0x4e, 0x61, 0x6d, 0x65, 0x12, 0x1c, 0x0a, 0x09, 0x76, 0x61, 0x6c, 0x69, 0x64, 0x61,
	0x74, 0x6f, 0x72, 0x18, 0x04, 0x20, 0x01, 0x28, 0x08, 0x52, 0x09, 0x76, 0x61, 0x6c, 0x69, 0x64,
	0x61, 0x74, 0x6f, 0x72, 0x12, 0x1d, 0x0a, 0x0a, 0x6a, 0x73, 0x6f, 0x6e, 0x5f, 0x63, 0x6c, 0x61,
	0x73, 0x73, 0x18, 0x05, 0x20, 0x01, 0x28, 0x09, 0x52, 0x09, 0x6a, 0x73, 0x6f, 0x6e, 0x43, 0x6c,
	0x61, 0x73, 0x73, 0x12, 0x1b, 0x0a, 0x09, 0x63, 0x68, 0x65, 0x66, 0x5f, 0x74, 0x79, 0x70, 0x65,
	0x18, 0x06, 0x20, 0x01, 0x28, 0x09, 0x52, 0x08, 0x63, 0x68, 0x65, 0x66, 0x54, 0x79, 0x70, 0x65,
	0x12, 0x59, 0x0a, 0x0a, 0x63, 0x6c, 0x69, 0x65, 0x6e, 0x74, 0x5f, 0x6b, 0x65, 0x79, 0x18, 0x07,
	0x20, 0x01, 0x28, 0x0b, 0x32, 0x3a, 0x2e, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x61, 0x75, 0x74, 0x6f,
	0x6d, 0x61, 0x74, 0x65, 0x2e, 0x64, 0x6f, 0x6d, 0x61, 0x69, 0x6e, 0x2e, 0x69, 0x6e, 0x66, 0x72,
	0x61, 0x5f, 0x70, 0x72, 0x6f, 0x78, 0x79, 0x2e, 0x72, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65,
	0x2e, 0x43, 0x6c, 0x69, 0x65, 0x6e, 0x74, 0x41, 0x63, 0x63, 0x65, 0x73, 0x73, 0x4b, 0x65, 0x79,
	0x52, 0x09, 0x63, 0x6c, 0x69, 0x65, 0x6e, 0x74, 0x4b, 0x65, 0x79, 0x22, 0x77, 0x0a, 0x0c, 0x43,
	0x72, 0x65, 0x61, 0x74, 0x65, 0x43, 0x6c, 0x69, 0x65, 0x6e, 0x74, 0x12, 0x12, 0x0a, 0x04, 0x6e,
	0x61, 0x6d, 0x65, 0x18, 0x01, 0x20, 0x01, 0x28, 0x09, 0x52, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x12,
	0x53, 0x0a, 0x0a, 0x63, 0x6c, 0x69, 0x65, 0x6e, 0x74, 0x5f, 0x6b, 0x65, 0x79, 0x18, 0x02, 0x20,
	0x01, 0x28, 0x0b, 0x32, 0x34, 0x2e, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x61, 0x75, 0x74, 0x6f, 0x6d,
	0x61, 0x74, 0x65, 0x2e, 0x64, 0x6f, 0x6d, 0x61, 0x69, 0x6e, 0x2e, 0x69, 0x6e, 0x66, 0x72, 0x61,
	0x5f, 0x70, 0x72, 0x6f, 0x78, 0x79, 0x2e, 0x72, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x2e,
	0x43, 0x6c, 0x69, 0x65, 0x6e, 0x74, 0x4b, 0x65, 0x79, 0x52, 0x09, 0x63, 0x6c, 0x69, 0x65, 0x6e,
	0x74, 0x4b, 0x65, 0x79, 0x22, 0x76, 0x0a, 0x0b, 0x52, 0x65, 0x73, 0x65, 0x74, 0x43, 0x6c, 0x69,
	0x65, 0x6e, 0x74, 0x12, 0x12, 0x0a, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x18, 0x01, 0x20, 0x01, 0x28,
	0x09, 0x52, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x12, 0x53, 0x0a, 0x0a, 0x63, 0x6c, 0x69, 0x65, 0x6e,
	0x74, 0x5f, 0x6b, 0x65, 0x79, 0x18, 0x02, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x34, 0x2e, 0x63, 0x68,
	0x65, 0x66, 0x2e, 0x61, 0x75, 0x74, 0x6f, 0x6d, 0x61, 0x74, 0x65, 0x2e, 0x64, 0x6f, 0x6d, 0x61,
	0x69, 0x6e, 0x2e, 0x69, 0x6e, 0x66, 0x72, 0x61, 0x5f, 0x70, 0x72, 0x6f, 0x78, 0x79, 0x2e, 0x72,
	0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x2e, 0x43, 0x6c, 0x69, 0x65, 0x6e, 0x74, 0x4b, 0x65,
	0x79, 0x52, 0x09, 0x63, 0x6c, 0x69, 0x65, 0x6e, 0x74, 0x4b, 0x65, 0x79, 0x22, 0x88, 0x01, 0x0a,
	0x09, 0x43, 0x6c, 0x69, 0x65, 0x6e, 0x74, 0x4b, 0x65, 0x79, 0x12, 0x12, 0x0a, 0x04, 0x6e, 0x61,
	0x6d, 0x65, 0x18, 0x01, 0x20, 0x01, 0x28, 0x09, 0x52, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x12, 0x1d,
	0x0a, 0x0a, 0x70, 0x75, 0x62, 0x6c, 0x69, 0x63, 0x5f, 0x6b, 0x65, 0x79, 0x18, 0x02, 0x20, 0x01,
	0x28, 0x09, 0x52, 0x09, 0x70, 0x75, 0x62, 0x6c, 0x69, 0x63, 0x4b, 0x65, 0x79, 0x12, 0x27, 0x0a,
	0x0f, 0x65, 0x78, 0x70, 0x69, 0x72, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x5f, 0x64, 0x61, 0x74, 0x65,
	0x18, 0x03, 0x20, 0x01, 0x28, 0x09, 0x52, 0x0e, 0x65, 0x78, 0x70, 0x69, 0x72, 0x61, 0x74, 0x69,
	0x6f, 0x6e, 0x44, 0x61, 0x74, 0x65, 0x12, 0x1f, 0x0a, 0x0b, 0x70, 0x72, 0x69, 0x76, 0x61, 0x74,
	0x65, 0x5f, 0x6b, 0x65, 0x79, 0x18, 0x04, 0x20, 0x01, 0x28, 0x09, 0x52, 0x0a, 0x70, 0x72, 0x69,
	0x76, 0x61, 0x74, 0x65, 0x4b, 0x65, 0x79, 0x22, 0x6d, 0x0a, 0x0f, 0x43, 0x6c, 0x69, 0x65, 0x6e,
	0x74, 0x41, 0x63, 0x63, 0x65, 0x73, 0x73, 0x4b, 0x65, 0x79, 0x12, 0x12, 0x0a, 0x04, 0x6e, 0x61,
	0x6d, 0x65, 0x18, 0x01, 0x20, 0x01, 0x28, 0x09, 0x52, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x12, 0x1d,
	0x0a, 0x0a, 0x70, 0x75, 0x62, 0x6c, 0x69, 0x63, 0x5f, 0x6b, 0x65, 0x79, 0x18, 0x02, 0x20, 0x01,
	0x28, 0x09, 0x52, 0x09, 0x70, 0x75, 0x62, 0x6c, 0x69, 0x63, 0x4b, 0x65, 0x79, 0x12, 0x27, 0x0a,
	0x0f, 0x65, 0x78, 0x70, 0x69, 0x72, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x5f, 0x64, 0x61, 0x74, 0x65,
	0x18, 0x03, 0x20, 0x01, 0x28, 0x09, 0x52, 0x0e, 0x65, 0x78, 0x70, 0x69, 0x72, 0x61, 0x74, 0x69,
	0x6f, 0x6e, 0x44, 0x61, 0x74, 0x65, 0x42, 0x40, 0x5a, 0x3e, 0x67, 0x69, 0x74, 0x68, 0x75, 0x62,
	0x2e, 0x63, 0x6f, 0x6d, 0x2f, 0x63, 0x68, 0x65, 0x66, 0x2f, 0x61, 0x75, 0x74, 0x6f, 0x6d, 0x61,
	0x74, 0x65, 0x2f, 0x61, 0x70, 0x69, 0x2f, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x73, 0x65, 0x72, 0x76,
	0x69, 0x63, 0x65, 0x2f, 0x69, 0x6e, 0x66, 0x72, 0x61, 0x5f, 0x70, 0x72, 0x6f, 0x78, 0x79, 0x2f,
	0x72, 0x65, 0x73, 0x70, 0x6f, 0x6e, 0x73, 0x65, 0x62, 0x06, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x33,
}

var (
	file_interservice_infra_proxy_response_clients_proto_rawDescOnce sync.Once
	file_interservice_infra_proxy_response_clients_proto_rawDescData = file_interservice_infra_proxy_response_clients_proto_rawDesc
)

func file_interservice_infra_proxy_response_clients_proto_rawDescGZIP() []byte {
	file_interservice_infra_proxy_response_clients_proto_rawDescOnce.Do(func() {
		file_interservice_infra_proxy_response_clients_proto_rawDescData = protoimpl.X.CompressGZIP(file_interservice_infra_proxy_response_clients_proto_rawDescData)
	})
	return file_interservice_infra_proxy_response_clients_proto_rawDescData
}

var file_interservice_infra_proxy_response_clients_proto_msgTypes = make([]protoimpl.MessageInfo, 7)
var file_interservice_infra_proxy_response_clients_proto_goTypes = []any{
	(*Clients)(nil),         // 0: chef.automate.domain.infra_proxy.response.Clients
	(*ClientListItem)(nil),  // 1: chef.automate.domain.infra_proxy.response.ClientListItem
	(*Client)(nil),          // 2: chef.automate.domain.infra_proxy.response.Client
	(*CreateClient)(nil),    // 3: chef.automate.domain.infra_proxy.response.CreateClient
	(*ResetClient)(nil),     // 4: chef.automate.domain.infra_proxy.response.ResetClient
	(*ClientKey)(nil),       // 5: chef.automate.domain.infra_proxy.response.ClientKey
	(*ClientAccessKey)(nil), // 6: chef.automate.domain.infra_proxy.response.ClientAccessKey
}
var file_interservice_infra_proxy_response_clients_proto_depIdxs = []int32{
	1, // 0: chef.automate.domain.infra_proxy.response.Clients.clients:type_name -> chef.automate.domain.infra_proxy.response.ClientListItem
	6, // 1: chef.automate.domain.infra_proxy.response.Client.client_key:type_name -> chef.automate.domain.infra_proxy.response.ClientAccessKey
	5, // 2: chef.automate.domain.infra_proxy.response.CreateClient.client_key:type_name -> chef.automate.domain.infra_proxy.response.ClientKey
	5, // 3: chef.automate.domain.infra_proxy.response.ResetClient.client_key:type_name -> chef.automate.domain.infra_proxy.response.ClientKey
	4, // [4:4] is the sub-list for method output_type
	4, // [4:4] is the sub-list for method input_type
	4, // [4:4] is the sub-list for extension type_name
	4, // [4:4] is the sub-list for extension extendee
	0, // [0:4] is the sub-list for field type_name
}

func init() { file_interservice_infra_proxy_response_clients_proto_init() }
func file_interservice_infra_proxy_response_clients_proto_init() {
	if File_interservice_infra_proxy_response_clients_proto != nil {
		return
	}
	type x struct{}
	out := protoimpl.TypeBuilder{
		File: protoimpl.DescBuilder{
			GoPackagePath: reflect.TypeOf(x{}).PkgPath(),
			RawDescriptor: file_interservice_infra_proxy_response_clients_proto_rawDesc,
			NumEnums:      0,
			NumMessages:   7,
			NumExtensions: 0,
			NumServices:   0,
		},
		GoTypes:           file_interservice_infra_proxy_response_clients_proto_goTypes,
		DependencyIndexes: file_interservice_infra_proxy_response_clients_proto_depIdxs,
		MessageInfos:      file_interservice_infra_proxy_response_clients_proto_msgTypes,
	}.Build()
	File_interservice_infra_proxy_response_clients_proto = out.File
	file_interservice_infra_proxy_response_clients_proto_rawDesc = nil
	file_interservice_infra_proxy_response_clients_proto_goTypes = nil
	file_interservice_infra_proxy_response_clients_proto_depIdxs = nil
}
