# automate-backend-airgap_bundle-module

Terraform Module

## Module Dependencies

None

## Description

This module operates in two modes: pull or push.

Push Mode (rsync):
Input is an array of source, destination file pairs and transfers the files to the target IP.
Transfer of n file pairs are all backgrounded so as to run in parallel via rsync.
This provides:
 - parallel transfers
 - throughput / transfer stats
 - ability to resume partial file transfers

Pull Mode: To be implemented.
