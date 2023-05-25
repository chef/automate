package models

type PortReachableRequest struct {
	DestinationNodeIp              string `json:"destination_node_ip"`
	DestinationNodePort            int    `json:"destination_node_port"`
	DestinationNodeServiceProtocol string `json:"destination_node_service_protocol"`
	RootCA                         string `json:"root_ca"`
}
