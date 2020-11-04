#!/bin/bash

set -eou pipefail

SEMGREP_TOKEN=$(vault kv get -field token secret/semgrep)
SEMGREP_ID=$(vault kv get -field id secret/semgrep)

export SEMGREP_TOKEN
export SEMGREP_ID