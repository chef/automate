package assets

//go:generate go run ../../tools/sort-services/sort-services.go data/services.json data/binds.txt
//go:generate go-bindata -pkg $GOPACKAGE -o assets.bindata.go data/...
//go:generate ../../../../scripts/fix_bindata_header assets.bindata.go
//go:generate go fmt ./
