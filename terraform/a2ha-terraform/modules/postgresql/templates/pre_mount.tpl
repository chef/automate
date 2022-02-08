#!/bin/bash -x

sudo mkdir -p /mnt/automate_backups/postgresql/pg_dump/

sudo mkdir -p /mnt/automate_backups/postgresql/archive/

sudo chown -R hab:hab /mnt/automate_backups/
