### install goreleaser
``` shell
homebrew: 
    brew install goreleaser
apt: 
    echo 'deb [trusted=yes] https://repo.goreleaser.com/apt/ /' | sudo tee /etc/apt/sources.list.d/goreleaser.list
    sudo apt update
    sudo apt install goreleaser
yum:
    echo '[goreleaser]
    name=GoReleaser
    baseurl=https://repo.goreleaser.com/yum/
    enabled=1
    gpgcheck=0' | sudo tee /etc/yum.repos.d/goreleaser.repo
    sudo yum install goreleaser
bash script:
    curl -sfL https://goreleaser.com/static/run | bash
```
### next 
```shell
goreleaser -v
goreleaser init
```
### modify
```shell 
 builds:
  - env:
      - CGO_ENABLED=0
    id: "demoproj"
    binary: "demo1" 
    main: ./go   ## main.go dir
    goos:
      - linux
      - windows
      - darwin

```
### build
```shell
goreleaser --snapshot --skip-publish --rm-dist
```
### 1. port method
```shell
1> c(complex1).
4> complex1:start("/home/kwinin/Downloads/port").
<0.72.0>
5> complex1:foo(3).
<<4>>
8> complex1:bar(3).
<<6>>
```
### 2. tcp method