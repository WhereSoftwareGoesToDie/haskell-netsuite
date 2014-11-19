Netsuite Connector for Haskell
==============================

Background
----------

Helps Haskell applications communicate with Netsuite's SuiteScript API.

The Netsuite Connector for Haskell uses restlet code developed as part of Ruby's ns_connector gem. See [the original ns_connector repository](https://github.com/christian-marie/ns_connector) for more information.

Exposed Restlet Actions
-----------------------

* retrieveNS
* fetchSublistNS
* rawSearchNS
* searchNS
* createNS
* attachNS
* detachNS
* updateNS
* updateSublistNS
* deleteNS
* invoicePdfNS
* transformNS

Usage
-----

To get started with these examples, open up GHCI and run something like the following, customising NsRestletConfig where appropriate, to get your environment ready.

All the functions below will return Value objects as defined by Data.Aeson.

```
import Data.Aeson
import Data.HashMap
import Data.Maybe
import Network.URI
import Netsuite.Connect
import Netsuite.Restlet.Configuration
import Netsuite.Types.Data

let testCfg = NsRestletConfig
              (fromJust $ parseURI "https://rest.netsuite.com/app/site/hosting/restlet.nl?script=123&deploy=1") -- URL for your script endpoint
              123456 -- NetSuite customer ID
              1000 -- NetSuite role ID
              "netsuite-user@yourcompany.example.com" -- identifier
              "mypassword" -- password
              Nothing -- custom user agent
              Nothing -- represents custom fields for each entity type
```

Here's a rough example of fetching a customer info. Run this in ghci:

```
retrieveNS testCfg "customer" 12345
```

You should get back an object containing all the fields for that customer.

A similar action is used for fetching credit cards, address books and so on:

```
fetchSublistNS testCfg ("customer","creditcards") 12345
```

A raw search:

```
rawSearchNS testCfg "customer" [toNsFilter ("lastmodifieddate",OnOrAfter,"daysAgo1")] [["externalid"], ["entityid"]]
```

And a simple search:

```
searchNS testCfg "customer" [toNsFilter ("lastmodifieddate",OnOrAfter,"daysAgo1")]
```

Creating a new contact:

```
let d = ["firstname" .= "Jane", "lastname" .= "Doe", "email" .= "jane.doe@example.com"]
let subd = [("addressbook", [["addr1" .= "Unit 1", "addr2" .= "123 Sesame Street", "city" .= "Sydney", "state" .= "NSW", "zip" .= "2000", "country" .= "AU"]])]
createNS testCfg "contact" d subd
```

Updating an existing contact (123456):

```
updateNS testCfg "contact" ["id" .= "123456", "firstname" .= "Wendy", "lastname" .= "Darling"]
```

Updating an existing contact's address book sublist:

```
updateSublistNS testCfg ("contact","addressbook") 123456 [["addr1" .= "Second Star to the Left", "addr2", "Straight on 'til Morning", "city" .= "Lost Boys' Hideout", "state" .= "Neverland", "zip" .= "12345", "country" .= "GB"]]
```

Attaching a contact (123456) to a customer (12345), with a default role:

```
attachNS testCfg "customer" [12345] "contact" 123456 []
```

Detaching a contact (123456) from a customer (12345):

```
detachNS testCfg "customer" [12345] "contact" 123456
```

Deleting a contact record:

```
deleteNS testCfg "contact" 123456
```

Downloading an invoice PDF:

```
invoicePdfNS testCfg 123456
```

Transforming a customer to a sales order:

```
transformNS testCfg "customer" "salesorder" 12345 []
```

Netsuite Types
--------------

* Customer
  * Address Book
  * Contact Roles
  * Credit Cards
  * Currency
  * Download
  * Group Pricing
  * Item Pricing
  * Partners
  * Sales Team
* Contact
  * Address Book
* Credit Memo
  * Invoices Applied To
  * Line Item
  * Partners
  * Sales Team
* Customer Deposit
* Customer Payment
  * Invoices Applied To
  * Credit
  * Deposit
* Discount Item
* Invoice
  * Expenses Cost
  * Line Item
  * Line Item Cost
  * Partners
  * Sales Team
  * Shipping Group
  * Time
* Non-Inventory Item
  * Price 1
  * Price 2
  * Price 3
  * Price 4
  * Price 5
  * Site Category
* Sales Order
  * Partners
  * Sales Team
  * Shipping Group
