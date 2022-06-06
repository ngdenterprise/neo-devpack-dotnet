# Storage Schema Preview

## Versioning

Every release of the SSP version of the .NET devpack (NCCS compiler + Smart Contract Framework) has a version
number derived from the production devpack version number.

* SSP release patch number is the (existing patch number + 1) * 1000 + the count of git commits since
  the production release. For example, the last SSP release for the 3.1.0 devpack was 3.1.1037. 
  If there had been an SSP release for v3.2.1 of the devpack, it would have had a version number like
  3.2.2043
* SSP releases carry a pre-release tag of `storage-schema-preview`
