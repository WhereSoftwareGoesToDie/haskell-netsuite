# Changelog

## v0

### v0.4.0.0

* Breaking changes in how request data types and restlet configuration are defined, to allow requests to be written more succintly
* Supports custom column definitions in configuration

### v0.3.0.0

* searchNS and rawSearchNS now support ISO-8601 datestamps for certain search columns.  
  eg. lastmodifieddate => "2014-10-06 00:00:00" or "2014-10-06"

### v0.2.0.0

* Breaking change: NsData and NsSublistData now use Aeson Values as their base type instead of HashMaps.
* Breaking change: All external Netsuite operation functions now return objects of Value type from Data.Aeson.
* searchNS now searches using chunking.

### v0.1.0.0

* Initial release.
