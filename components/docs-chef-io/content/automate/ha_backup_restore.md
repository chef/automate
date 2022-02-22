+++
title = "Backups and Restore"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Backups and Restore"
    identifier = "automate/install/ha_backup_restore.md Backups and Restore"
    parent = "automate/install"
    weight = 380
+++

Computer security is the safeguarding of computer systems and data against theft, illegal access, or against any disaster. It's the method of guarding against and detecting illegal access to your computer system. Data security refers to the process of securing data from illegal access or any disaster. This process includes the following terms:

- Data Backup
- Data Restore

## What is Data Backup?

Backup refers to the process of making copies of data or data files to use in the event the original data or data files are lost or destroyed.

In information technology, a backup or data backup is a copy of computer data taken and stored elsewhere to restore the original after a data loss event. A backup system contains at least one copy of all data considered worth saving. The data storage requirements can be large. An information repository model may be used to provide structure to this storage. Typically, backup data includes all the data--such as documents, media files, configuration and registry files, and machine images. required to perform the workload on your server.

## What is Data Restore?

Data restore is the process of copying backup data from secondary storage and restoring it to its original location or a new location. A restore is performed to return data that has been lost, stolen, or damaged to its original condition or to move data to a new location.

## What is the Difference Between Backup and Restore?

Backup and recovery are the process of duplicating data and storing it in a secure place in case of loss or damage, and then restoring that data to a location, the original one or a safe alternative that can be again used in operations to avoid downtime. Backup and recovery is also a category of onsite and cloud-based technology solutions that automate and support this process, enabling organizations to protect and retain their data for business and compliance reasons.

The key difference between backup and recovery is that the backup process is how you save and protect your production data and safely store it away so you have it for a later time when you might need to use it. Reliable backups and fast recovery together ensure business continuity and business resilience.

## What are Chef Automate High Availability (HA) Backups?

The Elastic Search, Postgres, and Chef Automate Server data and configurations can be backed up manually by running the backup command . There is no automated backup procedure built in Chef Automate CLI that runs on a periodic manner to backup the data.

## Backup Types

You can backup the Chef Automate HA data either using an External file-system (EFS) or Amazon's S3 bucket.

### What is EFS System?

External file system refers to any non-volatile storage device external to the computer. It can be any storage device that serves as an addition to the computer's primary storage, RAM, and cache memory. They are used for various purposes ranging from backup data used for future restores or disaster recovery, long-term archiving of data that is not frequently accessed, and storage of non-critical data in lower-performing, less expensive drives. These systems do not directly interact with the computer's CPU (Central Processing Unit).

External file systems include devices such as Solid-state drives (SSDs), Hard disk drives (HDDs), Cloud storage, CD-ROM drives, DVD drives, Blu-ray drives, USB flash drives, SD cards, Tape drives.

### What is Amazon's S3 Bucket?

An Amazon S3 bucket is a public cloud storage resource available in Amazon Web Services (AWS) Simple Storage Service (S3), an object storage offering. Amazon S3 buckets, similar to file folders, store objects consisting of data and its descriptive metadata. Amazon S3 is a program that is built to store, protect, and retrieve data from *buckets* at any time from anywhere on any device such as websites, mobile apps, archiving, data backups and restorations, IoT devices, enterprise application storage, and providing the underlying storage layer for your data lake.

With the AWS Free Usage Tier*, you can get started with Amazon S3 for free in all regions except the AWS GovCloud Regions. [See](https://aws.amazon.com/s3/) for more information.
