# Demo Env

Demo docker env. Lots of containers, all running on localhost ports starting from `11022`. Access via (contained) `vagrant` ssh key.

```
├── c6   CentOS
├── c7
├── d7   Debian
├── d8
├── f22  Fedora
├── u12  Ubuntu
├── u14
```

## Usage

Build and run a docker container.

```bash
cd c6
./build
./run

docker ps
  CONTAINER ID        IMAGE               COMMAND               CREATED             STATUS              PORTS                   NAMES
  46bd2689eeae        c6:latest           "/usr/sbin/sshd -D"   2 hours ago         Up 2 hours          0.0.0.0:11024->22/tcp   nostalgic_almeida
```


Copyright: 2015 Dominik Richter
