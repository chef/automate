go test -race -v $(go list ./...| grep -v /components\/automat-cli/ | grep -v /components\/automate-deployment\/pkg\/a1upgrade/) -coverprofile=Coverage_report/cover.out
exit 0
  