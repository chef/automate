# config-mgmt-service

Welcome to the Config Management API service.

## Geting Started

Now we use the Global Habitat Studio; follow [this](https://github.com/chef/automate/#global-habitat-studio) instructions.

### Debug Go code (with VS Code)

If your preferred editor is VS Code, the config-mgmt-service is enabled so that you can easily
debug Go code by following these steps:

Enter the habitat studio and start the Go debug server

```
$ hab studio enter
[1][default:/src:130]# debug_cfgmgmt
2018/04/24 14:05:03 server.go:73: Using API v1
2018/04/24 14:05:03 debugger.go:70: attaching to pid 19548
API server listening at: [::]:2345
```

After that you can just open your VS Code editor and start debugging, you should already have
a configuration pointing to the studio called `Remote Habitat Studio`.

![](docs/images/vs_code_debug_habitat_studio.gif)

