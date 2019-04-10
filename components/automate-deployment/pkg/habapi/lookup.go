package habapi

// ServiceInfoByName searches the passed slice of ServiceInfo structs
// and returns a pointer to the ServiceInfo for the named service.
func ServiceInfoByName(svcs []ServiceInfo, svcName string) (*ServiceInfo, bool) {
	for i, svc := range svcs {
		if svc.Pkg.Name == svcName {
			return &svcs[i], true
		}
	}

	return nil, false
}

// PortForService retrieves the GRPC port for the named service from
// the []ServiceInfo.
func PortForService(svcs []ServiceInfo, svcName string) (int32, bool) {
	for _, svc := range svcs {
		if svc.Pkg.Name == svcName {
			return portFromConfig(svc.Cfg, svcName)
		}
	}
	return 0, false
}

func portFromConfig(cfg map[string]interface{}, name string) (int32, bool) {
	switch name {
	case "automate-dex":
		svcCfgMap, ok := getMap("grpc", cfg)
		if !ok {
			return 0, false
		}
		return getInt32("port", svcCfgMap)
	case "automate-gateway":
		svcCfgMap, ok := getMap("service", cfg)
		if !ok {
			return 0, false
		}
		return getInt32("grpc_port", svcCfgMap)
	default:
		svcCfgMap, ok := getMap("service", cfg)
		if !ok {
			return 0, false
		}
		return getInt32("port", svcCfgMap)
	}
}

func getMap(key string, data map[string]interface{}) (map[string]interface{}, bool) {
	config, ok := data[key]
	if !ok {
		return nil, false
	}

	aMap, ok := config.(map[string]interface{})
	if !ok {
		return nil, false
	}
	return aMap, true
}

func getInt32(key string, data map[string]interface{}) (int32, bool) {
	port, ok := data[key]
	if !ok {
		return 0, false
	}

	flt, ok := port.(float64)
	if !ok {
		return 0, false
	}
	return int32(flt), true
}
