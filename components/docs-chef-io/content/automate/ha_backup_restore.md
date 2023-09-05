+++
title = "Overview"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Overview"
    identifier = "automate/deploy_high_availability/backup_and_restore/ha_backup_restore.md Overview Of Backup and Restore"
    parent = "automate/deploy_high_availability/backup_and_restore"
    weight = 200
+++

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

Computer security safeguards the computer system and data against theft, unlawful access, or any disaster. It's guarding against and detecting unlawful access to your computer system. Data security refers to securing data from unlawful access or any disaster. This process includes the following terms:

- Data Backup
- Data Restore

## Data Backup

Backup refers to making copies of data or data files to use in the event the original data or data files are lost or destroyed.

In information technology, a data backup is a copy of computer data taken and stored elsewhere to restore the original after a data loss event. A backup system contains at least one copy of all data considered worth saving. The data storage requirements can be extensive. An information repository model may be used to provide structure to this storage. Typically, backup data includes all the data such as documents, media files, configuration and registry files, and machine images.

## Data Restore

Data restore is the process of copying backup data from secondary storage and restoring it to its original location or a new location. A restoring process is carried out to return lost, stolen, or damaged data to its original condition or move it to a new location.

## Chef Automate High Availability (HA) Backups

You can manually back up the OpenSearch, Postgres, and Chef Automate Server data and configurations. The built-in Chef Automate CLI has no automated backup procedure that periodically backups the data.

## Backup Types

You can backup the Chef Automate HA data either using an External file-system (EFS) or Amazon's S3 bucket.

### What is EFS System?

External file system refers to any non-volatile storage device external to the computer. It can be any storage device that serves as an addition to the computer's primary storage, RAM, and cache memory. EFS aids in backing up the data used for future restores or disaster recovery, long-term archiving of data that is not frequently accessed, and storage of non-critical data in lower-performing, less expensive drives. These systems do not directly interact with the computer's CPU (Central Processing Unit).

External file systems include devices such as Solid-state drives (SSDs), Hard disk drives (HDDs), Cloud storage, CD-ROM drives, DVD drives, Blu-ray drives, USB flash drives, SD cards, Tape drives.

### What is Amazon's S3 Bucket?

An Amazon S3 bucket is a public cloud storage resource available in Amazon Web Services (AWS) Simple Storage Service (S3), an object storage offering. Amazon S3 buckets, similar to file folders, store objects consisting of data and its descriptive metadata. Amazon S3 is a program built to store, protect, and retrieve data from *buckets* at any time from anywhere on any device such as websites, mobile apps, archiving, data backups and restorations, IoT devices, enterprise application storage, and providing the underlying storage layer for your data lake.

With the AWS Free Usage Tier*, you can get started with Amazon S3 for free in all regions except the AWS GovCloud Regions. [See](https://aws.amazon.com/s3/) for more information.

## Taking Backup with Amazon S3 Bucket

This section explains how to take backup for External Elastic Search (ES) and Postgres-Sql to the Amazon S3 bucket.

{{< note >}}

Ensure you perform the backup configuration before deploying the Chef Automate High Availability (HA) cluster.

{{< /note >}}
