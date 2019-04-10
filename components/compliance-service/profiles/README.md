# Profile Store

The profile store consists of two parts:

- market
- store

The market includes the latest profiles that are included with Chef Automate. They are sourced from
[profiles](https://github.com/chef/compliance-profiles). Only the latest version of a profile is stored in the market.
Older versions are not available, once updated

The store a is namespaced per users/group and allows users to store multiple versions of the same profile. A user need
to install a profile from the market into the store. Only then, he can use them in InSpec.
