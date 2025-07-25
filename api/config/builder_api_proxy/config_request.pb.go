// -*- mode: protobuf; indent-tabs-mode: t; c-basic-offset: 8; tab-width: 8 -*-

// Code generated by protoc-gen-go. DO NOT EDIT.
// versions:
// 	protoc-gen-go v1.36.6
// 	protoc        v3.19.0
// source: config/builder_api_proxy/config_request.proto

package bldrapiproxy

import (
	shared "github.com/chef/automate/api/config/shared"
	_ "github.com/chef/automate/components/automate-grpc/protoc-gen-a2-config/api/a2conf"
	protoreflect "google.golang.org/protobuf/reflect/protoreflect"
	protoimpl "google.golang.org/protobuf/runtime/protoimpl"
	wrapperspb "google.golang.org/protobuf/types/known/wrapperspb"
	reflect "reflect"
	sync "sync"
	unsafe "unsafe"
)

const (
	// Verify that this generated code is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(20 - protoimpl.MinVersion)
	// Verify that runtime/protoimpl is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(protoimpl.MaxVersion - 20)
)

type ConfigRequest struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	V1            *ConfigRequest_V1      `protobuf:"bytes,1,opt,name=v1,proto3" json:"v1,omitempty" toml:"v1,omitempty" mapstructure:"v1,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *ConfigRequest) Reset() {
	*x = ConfigRequest{}
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[0]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ConfigRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ConfigRequest) ProtoMessage() {}

func (x *ConfigRequest) ProtoReflect() protoreflect.Message {
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[0]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ConfigRequest.ProtoReflect.Descriptor instead.
func (*ConfigRequest) Descriptor() ([]byte, []int) {
	return file_config_builder_api_proxy_config_request_proto_rawDescGZIP(), []int{0}
}

func (x *ConfigRequest) GetV1() *ConfigRequest_V1 {
	if x != nil {
		return x.V1
	}
	return nil
}

type ConfigRequest_V1 struct {
	state         protoimpl.MessageState    `protogen:"open.v1"`
	Sys           *ConfigRequest_V1_System  `protobuf:"bytes,1,opt,name=sys,proto3" json:"sys,omitempty" toml:"sys,omitempty" mapstructure:"sys,omitempty"`
	Svc           *ConfigRequest_V1_Service `protobuf:"bytes,2,opt,name=svc,proto3" json:"svc,omitempty" toml:"svc,omitempty" mapstructure:"svc,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *ConfigRequest_V1) Reset() {
	*x = ConfigRequest_V1{}
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[1]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ConfigRequest_V1) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ConfigRequest_V1) ProtoMessage() {}

func (x *ConfigRequest_V1) ProtoReflect() protoreflect.Message {
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[1]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ConfigRequest_V1.ProtoReflect.Descriptor instead.
func (*ConfigRequest_V1) Descriptor() ([]byte, []int) {
	return file_config_builder_api_proxy_config_request_proto_rawDescGZIP(), []int{0, 0}
}

func (x *ConfigRequest_V1) GetSys() *ConfigRequest_V1_System {
	if x != nil {
		return x.Sys
	}
	return nil
}

func (x *ConfigRequest_V1) GetSvc() *ConfigRequest_V1_Service {
	if x != nil {
		return x.Svc
	}
	return nil
}

type ConfigRequest_V1_System struct {
	state         protoimpl.MessageState           `protogen:"open.v1"`
	Mlsa          *shared.Mlsa                     `protobuf:"bytes,1,opt,name=mlsa,proto3" json:"mlsa,omitempty" toml:"mlsa,omitempty" mapstructure:"mlsa,omitempty"`
	Tls           *shared.TLSCredentials           `protobuf:"bytes,2,opt,name=tls,proto3" json:"tls,omitempty" toml:"tls,omitempty" mapstructure:"tls,omitempty"`
	Service       *ConfigRequest_V1_System_Service `protobuf:"bytes,3,opt,name=service,proto3" json:"service,omitempty" toml:"service,omitempty" mapstructure:"service,omitempty"`
	Log           *ConfigRequest_V1_System_Logger  `protobuf:"bytes,4,opt,name=log,proto3" json:"log,omitempty" toml:"log,omitempty" mapstructure:"log,omitempty"`
	Nginx         *ConfigRequest_V1_System_Nginx   `protobuf:"bytes,5,opt,name=nginx,proto3" json:"nginx,omitempty" toml:"nginx,omitempty" mapstructure:"nginx,omitempty"`
	Http          *ConfigRequest_V1_System_HTTP    `protobuf:"bytes,6,opt,name=http,proto3" json:"http,omitempty" toml:"http,omitempty" mapstructure:"http,omitempty"`
	Web           *ConfigRequest_V1_System_Web     `protobuf:"bytes,7,opt,name=web,proto3" json:"web,omitempty" toml:"web,omitempty" mapstructure:"web,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *ConfigRequest_V1_System) Reset() {
	*x = ConfigRequest_V1_System{}
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[2]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ConfigRequest_V1_System) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ConfigRequest_V1_System) ProtoMessage() {}

func (x *ConfigRequest_V1_System) ProtoReflect() protoreflect.Message {
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[2]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ConfigRequest_V1_System.ProtoReflect.Descriptor instead.
func (*ConfigRequest_V1_System) Descriptor() ([]byte, []int) {
	return file_config_builder_api_proxy_config_request_proto_rawDescGZIP(), []int{0, 0, 0}
}

func (x *ConfigRequest_V1_System) GetMlsa() *shared.Mlsa {
	if x != nil {
		return x.Mlsa
	}
	return nil
}

func (x *ConfigRequest_V1_System) GetTls() *shared.TLSCredentials {
	if x != nil {
		return x.Tls
	}
	return nil
}

func (x *ConfigRequest_V1_System) GetService() *ConfigRequest_V1_System_Service {
	if x != nil {
		return x.Service
	}
	return nil
}

func (x *ConfigRequest_V1_System) GetLog() *ConfigRequest_V1_System_Logger {
	if x != nil {
		return x.Log
	}
	return nil
}

func (x *ConfigRequest_V1_System) GetNginx() *ConfigRequest_V1_System_Nginx {
	if x != nil {
		return x.Nginx
	}
	return nil
}

func (x *ConfigRequest_V1_System) GetHttp() *ConfigRequest_V1_System_HTTP {
	if x != nil {
		return x.Http
	}
	return nil
}

func (x *ConfigRequest_V1_System) GetWeb() *ConfigRequest_V1_System_Web {
	if x != nil {
		return x.Web
	}
	return nil
}

type ConfigRequest_V1_Service struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *ConfigRequest_V1_Service) Reset() {
	*x = ConfigRequest_V1_Service{}
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[3]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ConfigRequest_V1_Service) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ConfigRequest_V1_Service) ProtoMessage() {}

func (x *ConfigRequest_V1_Service) ProtoReflect() protoreflect.Message {
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[3]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ConfigRequest_V1_Service.ProtoReflect.Descriptor instead.
func (*ConfigRequest_V1_Service) Descriptor() ([]byte, []int) {
	return file_config_builder_api_proxy_config_request_proto_rawDescGZIP(), []int{0, 0, 1}
}

type ConfigRequest_V1_System_Service struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// Deprecated: Marked as deprecated in config/builder_api_proxy/config_request.proto.
	Host          *wrapperspb.StringValue `protobuf:"bytes,1,opt,name=host,proto3" json:"host,omitempty" toml:"host,omitempty" mapstructure:"host,omitempty"` // The listen host is no longer setable(localhost only)
	Port          *wrapperspb.Int32Value  `protobuf:"bytes,2,opt,name=port,proto3" json:"port,omitempty" toml:"port,omitempty" mapstructure:"port,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *ConfigRequest_V1_System_Service) Reset() {
	*x = ConfigRequest_V1_System_Service{}
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[4]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ConfigRequest_V1_System_Service) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ConfigRequest_V1_System_Service) ProtoMessage() {}

func (x *ConfigRequest_V1_System_Service) ProtoReflect() protoreflect.Message {
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[4]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ConfigRequest_V1_System_Service.ProtoReflect.Descriptor instead.
func (*ConfigRequest_V1_System_Service) Descriptor() ([]byte, []int) {
	return file_config_builder_api_proxy_config_request_proto_rawDescGZIP(), []int{0, 0, 0, 0}
}

// Deprecated: Marked as deprecated in config/builder_api_proxy/config_request.proto.
func (x *ConfigRequest_V1_System_Service) GetHost() *wrapperspb.StringValue {
	if x != nil {
		return x.Host
	}
	return nil
}

func (x *ConfigRequest_V1_System_Service) GetPort() *wrapperspb.Int32Value {
	if x != nil {
		return x.Port
	}
	return nil
}

type ConfigRequest_V1_System_Logger struct {
	state         protoimpl.MessageState  `protogen:"open.v1"`
	Level         *wrapperspb.StringValue `protobuf:"bytes,1,opt,name=level,proto3" json:"level,omitempty" toml:"level,omitempty" mapstructure:"level,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *ConfigRequest_V1_System_Logger) Reset() {
	*x = ConfigRequest_V1_System_Logger{}
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[5]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ConfigRequest_V1_System_Logger) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ConfigRequest_V1_System_Logger) ProtoMessage() {}

func (x *ConfigRequest_V1_System_Logger) ProtoReflect() protoreflect.Message {
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[5]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ConfigRequest_V1_System_Logger.ProtoReflect.Descriptor instead.
func (*ConfigRequest_V1_System_Logger) Descriptor() ([]byte, []int) {
	return file_config_builder_api_proxy_config_request_proto_rawDescGZIP(), []int{0, 0, 0, 1}
}

func (x *ConfigRequest_V1_System_Logger) GetLevel() *wrapperspb.StringValue {
	if x != nil {
		return x.Level
	}
	return nil
}

type ConfigRequest_V1_System_Nginx struct {
	state               protoimpl.MessageState `protogen:"open.v1"`
	WorkerConnections   *wrapperspb.Int32Value `protobuf:"bytes,1,opt,name=worker_connections,json=workerConnections,proto3" json:"worker_connections,omitempty" toml:"worker_connections,omitempty" mapstructure:"worker_connections,omitempty"`
	WorkerProcesses     *wrapperspb.Int32Value `protobuf:"bytes,2,opt,name=worker_processes,json=workerProcesses,proto3" json:"worker_processes,omitempty" toml:"worker_processes,omitempty" mapstructure:"worker_processes,omitempty"`
	WorkerRlimitNofile  *wrapperspb.Int32Value `protobuf:"bytes,3,opt,name=worker_rlimit_nofile,json=workerRlimitNofile,proto3" json:"worker_rlimit_nofile,omitempty" toml:"worker_rlimit_nofile,omitempty" mapstructure:"worker_rlimit_nofile,omitempty"`
	MaxBodySize         *wrapperspb.Int32Value `protobuf:"bytes,4,opt,name=max_body_size,json=maxBodySize,proto3" json:"max_body_size,omitempty" toml:"max_body_size,omitempty" mapstructure:"max_body_size,omitempty"`
	ProxySendTimeout    *wrapperspb.Int32Value `protobuf:"bytes,5,opt,name=proxy_send_timeout,json=proxySendTimeout,proto3" json:"proxy_send_timeout,omitempty" toml:"proxy_send_timeout,omitempty" mapstructure:"proxy_send_timeout,omitempty"`
	ProxyReadTimeout    *wrapperspb.Int32Value `protobuf:"bytes,6,opt,name=proxy_read_timeout,json=proxyReadTimeout,proto3" json:"proxy_read_timeout,omitempty" toml:"proxy_read_timeout,omitempty" mapstructure:"proxy_read_timeout,omitempty"`
	ProxyConnectTimeout *wrapperspb.Int32Value `protobuf:"bytes,7,opt,name=proxy_connect_timeout,json=proxyConnectTimeout,proto3" json:"proxy_connect_timeout,omitempty" toml:"proxy_connect_timeout,omitempty" mapstructure:"proxy_connect_timeout,omitempty"`
	EnableCaching       *wrapperspb.BoolValue  `protobuf:"bytes,8,opt,name=enable_caching,json=enableCaching,proto3" json:"enable_caching,omitempty" toml:"enable_caching,omitempty" mapstructure:"enable_caching,omitempty"`
	EnableGzip          *wrapperspb.BoolValue  `protobuf:"bytes,9,opt,name=enable_gzip,json=enableGzip,proto3" json:"enable_gzip,omitempty" toml:"enable_gzip,omitempty" mapstructure:"enable_gzip,omitempty"`
	unknownFields       protoimpl.UnknownFields
	sizeCache           protoimpl.SizeCache
}

func (x *ConfigRequest_V1_System_Nginx) Reset() {
	*x = ConfigRequest_V1_System_Nginx{}
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[6]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ConfigRequest_V1_System_Nginx) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ConfigRequest_V1_System_Nginx) ProtoMessage() {}

func (x *ConfigRequest_V1_System_Nginx) ProtoReflect() protoreflect.Message {
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[6]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ConfigRequest_V1_System_Nginx.ProtoReflect.Descriptor instead.
func (*ConfigRequest_V1_System_Nginx) Descriptor() ([]byte, []int) {
	return file_config_builder_api_proxy_config_request_proto_rawDescGZIP(), []int{0, 0, 0, 2}
}

func (x *ConfigRequest_V1_System_Nginx) GetWorkerConnections() *wrapperspb.Int32Value {
	if x != nil {
		return x.WorkerConnections
	}
	return nil
}

func (x *ConfigRequest_V1_System_Nginx) GetWorkerProcesses() *wrapperspb.Int32Value {
	if x != nil {
		return x.WorkerProcesses
	}
	return nil
}

func (x *ConfigRequest_V1_System_Nginx) GetWorkerRlimitNofile() *wrapperspb.Int32Value {
	if x != nil {
		return x.WorkerRlimitNofile
	}
	return nil
}

func (x *ConfigRequest_V1_System_Nginx) GetMaxBodySize() *wrapperspb.Int32Value {
	if x != nil {
		return x.MaxBodySize
	}
	return nil
}

func (x *ConfigRequest_V1_System_Nginx) GetProxySendTimeout() *wrapperspb.Int32Value {
	if x != nil {
		return x.ProxySendTimeout
	}
	return nil
}

func (x *ConfigRequest_V1_System_Nginx) GetProxyReadTimeout() *wrapperspb.Int32Value {
	if x != nil {
		return x.ProxyReadTimeout
	}
	return nil
}

func (x *ConfigRequest_V1_System_Nginx) GetProxyConnectTimeout() *wrapperspb.Int32Value {
	if x != nil {
		return x.ProxyConnectTimeout
	}
	return nil
}

func (x *ConfigRequest_V1_System_Nginx) GetEnableCaching() *wrapperspb.BoolValue {
	if x != nil {
		return x.EnableCaching
	}
	return nil
}

func (x *ConfigRequest_V1_System_Nginx) GetEnableGzip() *wrapperspb.BoolValue {
	if x != nil {
		return x.EnableGzip
	}
	return nil
}

type ConfigRequest_V1_System_HTTP struct {
	state                protoimpl.MessageState  `protogen:"open.v1"`
	KeepaliveConnections *wrapperspb.Int32Value  `protobuf:"bytes,1,opt,name=keepalive_connections,json=keepaliveConnections,proto3" json:"keepalive_connections,omitempty" toml:"keepalive_connections,omitempty" mapstructure:"keepalive_connections,omitempty"`
	KeepaliveRequests    *wrapperspb.Int32Value  `protobuf:"bytes,2,opt,name=keepalive_requests,json=keepaliveRequests,proto3" json:"keepalive_requests,omitempty" toml:"keepalive_requests,omitempty" mapstructure:"keepalive_requests,omitempty"`
	KeepaliveTimeout     *wrapperspb.Int32Value  `protobuf:"bytes,3,opt,name=keepalive_timeout,json=keepaliveTimeout,proto3" json:"keepalive_timeout,omitempty" toml:"keepalive_timeout,omitempty" mapstructure:"keepalive_timeout,omitempty"`
	SslCiphers           *wrapperspb.StringValue `protobuf:"bytes,4,opt,name=ssl_ciphers,json=sslCiphers,proto3" json:"ssl_ciphers,omitempty" toml:"ssl_ciphers,omitempty" mapstructure:"ssl_ciphers,omitempty"`
	SslProtocols         *wrapperspb.StringValue `protobuf:"bytes,5,opt,name=ssl_protocols,json=sslProtocols,proto3" json:"ssl_protocols,omitempty" toml:"ssl_protocols,omitempty" mapstructure:"ssl_protocols,omitempty"`
	SslVerifyDepth       *wrapperspb.Int32Value  `protobuf:"bytes,6,opt,name=ssl_verify_depth,json=sslVerifyDepth,proto3" json:"ssl_verify_depth,omitempty" toml:"ssl_verify_depth,omitempty" mapstructure:"ssl_verify_depth,omitempty"`
	XXssProtection       *wrapperspb.StringValue `protobuf:"bytes,7,opt,name=x_xss_protection,json=xXssProtection,proto3" json:"x_xss_protection,omitempty" toml:"x_xss_protection,omitempty" mapstructure:"x_xss_protection,omitempty"`
	unknownFields        protoimpl.UnknownFields
	sizeCache            protoimpl.SizeCache
}

func (x *ConfigRequest_V1_System_HTTP) Reset() {
	*x = ConfigRequest_V1_System_HTTP{}
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[7]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ConfigRequest_V1_System_HTTP) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ConfigRequest_V1_System_HTTP) ProtoMessage() {}

func (x *ConfigRequest_V1_System_HTTP) ProtoReflect() protoreflect.Message {
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[7]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ConfigRequest_V1_System_HTTP.ProtoReflect.Descriptor instead.
func (*ConfigRequest_V1_System_HTTP) Descriptor() ([]byte, []int) {
	return file_config_builder_api_proxy_config_request_proto_rawDescGZIP(), []int{0, 0, 0, 3}
}

func (x *ConfigRequest_V1_System_HTTP) GetKeepaliveConnections() *wrapperspb.Int32Value {
	if x != nil {
		return x.KeepaliveConnections
	}
	return nil
}

func (x *ConfigRequest_V1_System_HTTP) GetKeepaliveRequests() *wrapperspb.Int32Value {
	if x != nil {
		return x.KeepaliveRequests
	}
	return nil
}

func (x *ConfigRequest_V1_System_HTTP) GetKeepaliveTimeout() *wrapperspb.Int32Value {
	if x != nil {
		return x.KeepaliveTimeout
	}
	return nil
}

func (x *ConfigRequest_V1_System_HTTP) GetSslCiphers() *wrapperspb.StringValue {
	if x != nil {
		return x.SslCiphers
	}
	return nil
}

func (x *ConfigRequest_V1_System_HTTP) GetSslProtocols() *wrapperspb.StringValue {
	if x != nil {
		return x.SslProtocols
	}
	return nil
}

func (x *ConfigRequest_V1_System_HTTP) GetSslVerifyDepth() *wrapperspb.Int32Value {
	if x != nil {
		return x.SslVerifyDepth
	}
	return nil
}

func (x *ConfigRequest_V1_System_HTTP) GetXXssProtection() *wrapperspb.StringValue {
	if x != nil {
		return x.XXssProtection
	}
	return nil
}

type ConfigRequest_V1_System_Web struct {
	state         protoimpl.MessageState  `protogen:"open.v1"`
	CookieDomain  *wrapperspb.StringValue `protobuf:"bytes,1,opt,name=cookie_domain,json=cookieDomain,proto3" json:"cookie_domain,omitempty" toml:"cookie_domain,omitempty" mapstructure:"cookie_domain,omitempty"`
	Environment   *wrapperspb.StringValue `protobuf:"bytes,2,opt,name=environment,proto3" json:"environment,omitempty" toml:"environment,omitempty" mapstructure:"environment,omitempty"`
	UseGravatar   *wrapperspb.BoolValue   `protobuf:"bytes,3,opt,name=use_gravatar,json=useGravatar,proto3" json:"use_gravatar,omitempty" toml:"use_gravatar,omitempty" mapstructure:"use_gravatar,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *ConfigRequest_V1_System_Web) Reset() {
	*x = ConfigRequest_V1_System_Web{}
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[8]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ConfigRequest_V1_System_Web) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ConfigRequest_V1_System_Web) ProtoMessage() {}

func (x *ConfigRequest_V1_System_Web) ProtoReflect() protoreflect.Message {
	mi := &file_config_builder_api_proxy_config_request_proto_msgTypes[8]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ConfigRequest_V1_System_Web.ProtoReflect.Descriptor instead.
func (*ConfigRequest_V1_System_Web) Descriptor() ([]byte, []int) {
	return file_config_builder_api_proxy_config_request_proto_rawDescGZIP(), []int{0, 0, 0, 4}
}

func (x *ConfigRequest_V1_System_Web) GetCookieDomain() *wrapperspb.StringValue {
	if x != nil {
		return x.CookieDomain
	}
	return nil
}

func (x *ConfigRequest_V1_System_Web) GetEnvironment() *wrapperspb.StringValue {
	if x != nil {
		return x.Environment
	}
	return nil
}

func (x *ConfigRequest_V1_System_Web) GetUseGravatar() *wrapperspb.BoolValue {
	if x != nil {
		return x.UseGravatar
	}
	return nil
}

var File_config_builder_api_proxy_config_request_proto protoreflect.FileDescriptor

const file_config_builder_api_proxy_config_request_proto_rawDesc = "" +
	"\n" +
	"-config/builder_api_proxy/config_request.proto\x12&chef.automate.domain.builder_api_proxy\x1a\x1aconfig/shared/global.proto\x1a\x17config/shared/tls.proto\x1a?automate-grpc/protoc-gen-a2-config/api/a2conf/annotations.proto\x1a\x1egoogle/protobuf/wrappers.proto\"\xa7\x13\n" +
	"\rConfigRequest\x12H\n" +
	"\x02v1\x18\x01 \x01(\v28.chef.automate.domain.builder_api_proxy.ConfigRequest.V1R\x02v1\x1a\xa9\x12\n" +
	"\x02V1\x12Q\n" +
	"\x03sys\x18\x01 \x01(\v2?.chef.automate.domain.builder_api_proxy.ConfigRequest.V1.SystemR\x03sys\x12R\n" +
	"\x03svc\x18\x02 \x01(\v2@.chef.automate.domain.builder_api_proxy.ConfigRequest.V1.ServiceR\x03svc\x1a\xf0\x10\n" +
	"\x06System\x124\n" +
	"\x04mlsa\x18\x01 \x01(\v2 .chef.automate.infra.config.MlsaR\x04mlsa\x12<\n" +
	"\x03tls\x18\x02 \x01(\v2*.chef.automate.infra.config.TLSCredentialsR\x03tls\x12a\n" +
	"\aservice\x18\x03 \x01(\v2G.chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.ServiceR\aservice\x12X\n" +
	"\x03log\x18\x04 \x01(\v2F.chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.LoggerR\x03log\x12[\n" +
	"\x05nginx\x18\x05 \x01(\v2E.chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.NginxR\x05nginx\x12X\n" +
	"\x04http\x18\x06 \x01(\v2D.chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.HTTPR\x04http\x12U\n" +
	"\x03web\x18\a \x01(\v2C.chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.WebR\x03web\x1a\x87\x01\n" +
	"\aService\x124\n" +
	"\x04host\x18\x01 \x01(\v2\x1c.google.protobuf.StringValueB\x02\x18\x01R\x04host\x12F\n" +
	"\x04port\x18\x02 \x01(\v2\x1b.google.protobuf.Int32ValueB\x15\xc2\xf3\x18\x11\n" +
	"\x05https\x10\xf8N\x1a\x05httpsR\x04port\x1a<\n" +
	"\x06Logger\x122\n" +
	"\x05level\x18\x01 \x01(\v2\x1c.google.protobuf.StringValueR\x05level\x1a\x92\x05\n" +
	"\x05Nginx\x12J\n" +
	"\x12worker_connections\x18\x01 \x01(\v2\x1b.google.protobuf.Int32ValueR\x11workerConnections\x12F\n" +
	"\x10worker_processes\x18\x02 \x01(\v2\x1b.google.protobuf.Int32ValueR\x0fworkerProcesses\x12M\n" +
	"\x14worker_rlimit_nofile\x18\x03 \x01(\v2\x1b.google.protobuf.Int32ValueR\x12workerRlimitNofile\x12?\n" +
	"\rmax_body_size\x18\x04 \x01(\v2\x1b.google.protobuf.Int32ValueR\vmaxBodySize\x12I\n" +
	"\x12proxy_send_timeout\x18\x05 \x01(\v2\x1b.google.protobuf.Int32ValueR\x10proxySendTimeout\x12I\n" +
	"\x12proxy_read_timeout\x18\x06 \x01(\v2\x1b.google.protobuf.Int32ValueR\x10proxyReadTimeout\x12O\n" +
	"\x15proxy_connect_timeout\x18\a \x01(\v2\x1b.google.protobuf.Int32ValueR\x13proxyConnectTimeout\x12A\n" +
	"\x0eenable_caching\x18\b \x01(\v2\x1a.google.protobuf.BoolValueR\renableCaching\x12;\n" +
	"\venable_gzip\x18\t \x01(\v2\x1a.google.protobuf.BoolValueR\n" +
	"enableGzip\x1a\xff\x03\n" +
	"\x04HTTP\x12P\n" +
	"\x15keepalive_connections\x18\x01 \x01(\v2\x1b.google.protobuf.Int32ValueR\x14keepaliveConnections\x12J\n" +
	"\x12keepalive_requests\x18\x02 \x01(\v2\x1b.google.protobuf.Int32ValueR\x11keepaliveRequests\x12H\n" +
	"\x11keepalive_timeout\x18\x03 \x01(\v2\x1b.google.protobuf.Int32ValueR\x10keepaliveTimeout\x12=\n" +
	"\vssl_ciphers\x18\x04 \x01(\v2\x1c.google.protobuf.StringValueR\n" +
	"sslCiphers\x12A\n" +
	"\rssl_protocols\x18\x05 \x01(\v2\x1c.google.protobuf.StringValueR\fsslProtocols\x12E\n" +
	"\x10ssl_verify_depth\x18\x06 \x01(\v2\x1b.google.protobuf.Int32ValueR\x0esslVerifyDepth\x12F\n" +
	"\x10x_xss_protection\x18\a \x01(\v2\x1c.google.protobuf.StringValueR\x0exXssProtection\x1a\xc7\x01\n" +
	"\x03Web\x12A\n" +
	"\rcookie_domain\x18\x01 \x01(\v2\x1c.google.protobuf.StringValueR\fcookieDomain\x12>\n" +
	"\venvironment\x18\x02 \x01(\v2\x1c.google.protobuf.StringValueR\venvironment\x12=\n" +
	"\fuse_gravatar\x18\x03 \x01(\v2\x1a.google.protobuf.BoolValueR\vuseGravatar\x1a\t\n" +
	"\aService: \xc2\xf3\x18\x1c\n" +
	"\x1aautomate-builder-api-proxyBDZBgithub.com/chef/automate/api/config/builder_api_proxy;bldrapiproxyb\x06proto3"

var (
	file_config_builder_api_proxy_config_request_proto_rawDescOnce sync.Once
	file_config_builder_api_proxy_config_request_proto_rawDescData []byte
)

func file_config_builder_api_proxy_config_request_proto_rawDescGZIP() []byte {
	file_config_builder_api_proxy_config_request_proto_rawDescOnce.Do(func() {
		file_config_builder_api_proxy_config_request_proto_rawDescData = protoimpl.X.CompressGZIP(unsafe.Slice(unsafe.StringData(file_config_builder_api_proxy_config_request_proto_rawDesc), len(file_config_builder_api_proxy_config_request_proto_rawDesc)))
	})
	return file_config_builder_api_proxy_config_request_proto_rawDescData
}

var file_config_builder_api_proxy_config_request_proto_msgTypes = make([]protoimpl.MessageInfo, 9)
var file_config_builder_api_proxy_config_request_proto_goTypes = []any{
	(*ConfigRequest)(nil),                   // 0: chef.automate.domain.builder_api_proxy.ConfigRequest
	(*ConfigRequest_V1)(nil),                // 1: chef.automate.domain.builder_api_proxy.ConfigRequest.V1
	(*ConfigRequest_V1_System)(nil),         // 2: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System
	(*ConfigRequest_V1_Service)(nil),        // 3: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.Service
	(*ConfigRequest_V1_System_Service)(nil), // 4: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Service
	(*ConfigRequest_V1_System_Logger)(nil),  // 5: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Logger
	(*ConfigRequest_V1_System_Nginx)(nil),   // 6: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Nginx
	(*ConfigRequest_V1_System_HTTP)(nil),    // 7: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.HTTP
	(*ConfigRequest_V1_System_Web)(nil),     // 8: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Web
	(*shared.Mlsa)(nil),                     // 9: chef.automate.infra.config.Mlsa
	(*shared.TLSCredentials)(nil),           // 10: chef.automate.infra.config.TLSCredentials
	(*wrapperspb.StringValue)(nil),          // 11: google.protobuf.StringValue
	(*wrapperspb.Int32Value)(nil),           // 12: google.protobuf.Int32Value
	(*wrapperspb.BoolValue)(nil),            // 13: google.protobuf.BoolValue
}
var file_config_builder_api_proxy_config_request_proto_depIdxs = []int32{
	1,  // 0: chef.automate.domain.builder_api_proxy.ConfigRequest.v1:type_name -> chef.automate.domain.builder_api_proxy.ConfigRequest.V1
	2,  // 1: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.sys:type_name -> chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System
	3,  // 2: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.svc:type_name -> chef.automate.domain.builder_api_proxy.ConfigRequest.V1.Service
	9,  // 3: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.mlsa:type_name -> chef.automate.infra.config.Mlsa
	10, // 4: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.tls:type_name -> chef.automate.infra.config.TLSCredentials
	4,  // 5: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.service:type_name -> chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Service
	5,  // 6: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.log:type_name -> chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Logger
	6,  // 7: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.nginx:type_name -> chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Nginx
	7,  // 8: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.http:type_name -> chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.HTTP
	8,  // 9: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.web:type_name -> chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Web
	11, // 10: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Service.host:type_name -> google.protobuf.StringValue
	12, // 11: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Service.port:type_name -> google.protobuf.Int32Value
	11, // 12: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Logger.level:type_name -> google.protobuf.StringValue
	12, // 13: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Nginx.worker_connections:type_name -> google.protobuf.Int32Value
	12, // 14: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Nginx.worker_processes:type_name -> google.protobuf.Int32Value
	12, // 15: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Nginx.worker_rlimit_nofile:type_name -> google.protobuf.Int32Value
	12, // 16: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Nginx.max_body_size:type_name -> google.protobuf.Int32Value
	12, // 17: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Nginx.proxy_send_timeout:type_name -> google.protobuf.Int32Value
	12, // 18: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Nginx.proxy_read_timeout:type_name -> google.protobuf.Int32Value
	12, // 19: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Nginx.proxy_connect_timeout:type_name -> google.protobuf.Int32Value
	13, // 20: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Nginx.enable_caching:type_name -> google.protobuf.BoolValue
	13, // 21: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Nginx.enable_gzip:type_name -> google.protobuf.BoolValue
	12, // 22: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.HTTP.keepalive_connections:type_name -> google.protobuf.Int32Value
	12, // 23: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.HTTP.keepalive_requests:type_name -> google.protobuf.Int32Value
	12, // 24: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.HTTP.keepalive_timeout:type_name -> google.protobuf.Int32Value
	11, // 25: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.HTTP.ssl_ciphers:type_name -> google.protobuf.StringValue
	11, // 26: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.HTTP.ssl_protocols:type_name -> google.protobuf.StringValue
	12, // 27: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.HTTP.ssl_verify_depth:type_name -> google.protobuf.Int32Value
	11, // 28: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.HTTP.x_xss_protection:type_name -> google.protobuf.StringValue
	11, // 29: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Web.cookie_domain:type_name -> google.protobuf.StringValue
	11, // 30: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Web.environment:type_name -> google.protobuf.StringValue
	13, // 31: chef.automate.domain.builder_api_proxy.ConfigRequest.V1.System.Web.use_gravatar:type_name -> google.protobuf.BoolValue
	32, // [32:32] is the sub-list for method output_type
	32, // [32:32] is the sub-list for method input_type
	32, // [32:32] is the sub-list for extension type_name
	32, // [32:32] is the sub-list for extension extendee
	0,  // [0:32] is the sub-list for field type_name
}

func init() { file_config_builder_api_proxy_config_request_proto_init() }
func file_config_builder_api_proxy_config_request_proto_init() {
	if File_config_builder_api_proxy_config_request_proto != nil {
		return
	}
	type x struct{}
	out := protoimpl.TypeBuilder{
		File: protoimpl.DescBuilder{
			GoPackagePath: reflect.TypeOf(x{}).PkgPath(),
			RawDescriptor: unsafe.Slice(unsafe.StringData(file_config_builder_api_proxy_config_request_proto_rawDesc), len(file_config_builder_api_proxy_config_request_proto_rawDesc)),
			NumEnums:      0,
			NumMessages:   9,
			NumExtensions: 0,
			NumServices:   0,
		},
		GoTypes:           file_config_builder_api_proxy_config_request_proto_goTypes,
		DependencyIndexes: file_config_builder_api_proxy_config_request_proto_depIdxs,
		MessageInfos:      file_config_builder_api_proxy_config_request_proto_msgTypes,
	}.Build()
	File_config_builder_api_proxy_config_request_proto = out.File
	file_config_builder_api_proxy_config_request_proto_goTypes = nil
	file_config_builder_api_proxy_config_request_proto_depIdxs = nil
}
