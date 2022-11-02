package server

var logRotate = `
/var/log/automate.log {
    daily
    rotate 5
    size 10M
}
`
