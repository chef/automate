# Automate Benchmarks

We use benchmarks to discover what is the best performance being achieved, whether in
a particular function, or an entire component/service. This information can then be
used to identify gaps or deltas between code changes and will be the grounds for us
to be able to make decisions in the near and long future.

## Components with Benchmarks

The list of components that has benchmarks:

- [Applications Service](https://github.com/chef/automate/tree/master/benchmarks/applications-service)


## Directory Scructure

Every component should have its own directory where we will store historical benchmarks
for every pull request that modifies the component:

```
   benchmarks/
    |-README.md
    |-applications-service/
    |  |-txt/
    |  |-html/
    |  |-README.md
    |-ingest-service/
    |  |-txt/
    |  |-html/
    |  |-README.md
```

At the moment we will run the benchmarks manually on our local computers, on every
component directory should have a `README.md` that explains how to run the benchmarks
and how to store them. In the future we would like our CI system to run and store them
automatically.

## Comparing Benchmarks (benchstat)

To compare two different benchmarks we use the go tool `benchstat`, use the last generated
benchmark and the new one from your pull request you are comparing to.

Example comparing benchmarks for the `applications-service`:

```
[1][default:/src:0]# cd /src/benchmarks/applications-service/txt
[2][default:/src/benchmarks/applications-service/txt:0]# install_benchstat
[3][default:/src/benchmarks/applications-service/txt:0]# benchstat PR_OLD.txt PR_NEW.txt
```

