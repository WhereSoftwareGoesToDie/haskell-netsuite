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

Installation
------------

Please apply the following pull request against your copy of http-streams:

https://github.com/afcowie/http-streams/pull/67

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

let testRestletConfig = NsRestletConfig (fromJust $ parseURI "https://rest.netsuite.com/app/site/hosting/restlet.nl?script=21&deploy=5") 123456 1000 "netsuite-user@yourcompany.example.com" "mypassword" Nothing
```

Here's a rough example of fetching a customer info. Run this in ghci:

```
retrieveNS testRestletConfig (NsType "customer") (NsDataId 12345) 
```

You should get back an object containing all the fields for that customer.

A similar action is used for fetching credit cards, address books and so on:

```
fetchSublistNS testRestletConfig (NsSubtype (NsType "customer") "creditcards") (NsId 12345)
```

A raw search:

```
rawSearchNS testRestletConfig (NsType "customer") [(NsFilter "lastmodifieddate" Nothing OnOrAfter (Just "daysAgo1") Nothing)] (toSearchCols [["externalid"], ["entityid"]])
```

And a simple search:

```
searchNS testRestletConfig (NsType "customer") [(NsFilter "lastmodifieddate" Nothing OnOrAfter (Just "daysAgo1") Nothing)]
```

Creating a new contact:

```
let d = NsData $ fromList [("firstname", "Jane"), ("lastname", "Doe"), ("email", "jane.doe@example.com")]
let subd = NsSublistData $ fromList [("addressbook", [NsData $ fromList [("addr1", "Unit 1"), ("addr2", "123 Sesame Street"), ("city", "Sydney"), ("state", "NSW"), ("zip", "2000"), ("country", "AU")]])]
createNS testRestletConfig (NsType "contact") d subd
```

Updating an existing contact (123456):

```
updateNS testRestletConfig (NsType "contact") (NsData $ fromList [("id", "123456"), ("firstname", "Wendy"), ("lastname", "Darling")])
```

Updating an existing contact's address book sublist:

```
updateSublistNS testRestletConfig (NsSubtype (NsType "contact") "addressbook") (NsId 123456) [NsData $ fromList [("addr1", "Second Star to the Left"), ("addr2", "Straight on 'til Morning"), ("city", "Lost Boys' Hideout"), ("state", "Neverland"), ("zip", "12345"), ("country", "GB")]]
```

Attaching a contact (123456) to a customer (12345), with a default role:

```
attachNS testRestletConfig (NsType "customer") [(NsId 12345)] (NsType "contact") (NsId 123456) (NsData $ fromList [])
```

Detaching a contact (123456) from a customer (12345):

```
detachNS testRestletConfig (NsType "customer") [(NsId 12345)] (NsType "contact") (NsId 123456)
```

Deleting a contact record:

```
deleteNS testRestletConfig (NsType "contact") (NsDataId 123456)
```

Downloading an invoice PDF:

```
invoicePdfNS testRestletConfig (NsId 123456)
```

Transforming a customer to a sales order:

```
transformNS testRestletConfig (NsType "customer") (NsType "salesorder") (NsId 12345) (NsData $ fromList [])
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
