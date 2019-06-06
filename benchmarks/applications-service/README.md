# Applications Service Benchmarks

This space has the benchmarks for the `applications-service`.

## How to run benchmarks

- Enter the studio.
```
$ hab studio enter
```
- Build and start the applications-service.
```
[1][default:/src:0]# start_applications_service
```
- Run the benchmarks and store them. _(NOTE: You should already have a PR open)_
```
[2][default:/src:0]# applications_benchmarks
  hab-studio: Running applications-service benchmark. (temporarily stored at /tmp/new.txt)
  hab-studio: This might take a while...
  hab-studio: Saving benchmark results at /src/benchmarks/applications-service/txt/new.txt
  hab-studio: Using benchstat tool to compute the results. ('benchstat new.txt')
  hab-studio: Generating HTML of the results at /src/benchmarks/applications-service/html/new.html
  hab-studio: Renaming the generated benchmark files:

              TXT:  /src/benchmarks/applications-service/txt/new.txt
              HTML: /src/benchmarks/applications-service/html/new.html

  hab-studio: Enter the new name based on the pull request opened.

              Example: For the following PR https://github.com/chef/automate/pull/510
                       The suggested name would be 'PR_510'

              New name: [ENTER_NAME_HERE]
  hab-studio: New benchmark files:

              TXT:  /src/benchmarks/applications-service/txt/[NAME_ENTERED].txt
              HTML: /src/benchmarks/applications-service/html/[NAME_ENTERED].html
```

