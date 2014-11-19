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

### Setup

To get started with these examples, open up GHCI and run something like the following, customising your configuration where appropriate, to get your environment ready.

Unlike the previous version, your Netsuite Restlet Config is now defined by a tuple of values. See below for a few examples of the constructor.  
Note that in this case, I've set the Customer ID and Role to be explicitly Int; those values can be anything of class Integral.

```haskell
import Data.Aeson
import Data.HashMap
import Netsuite.Connect

let endpoint = "https://rest.netsuite.com/app/site/hosting/restlet.nl?script=123&deploy=1" -- URL for your script endpoint

-- The configuration below describes a full NsRestletConfig, with custom user agent and custom fields specified for customer entities.
let testCfg = (endpoint, -- NetSuite endpoint URL
               123456 :: Int, -- NetSuite customer ID
               1000 :: Int, -- NetSuite account role
               "netsuite-user@yourcompany.example.com", -- NetSuite account ident
               "mypassword", -- NetSuite account password
               "My Netsuite Client", -- user agent string
               fromList $ [["customer"], ["customfield1", "customfield2"]] -- custom fields as a HashMap [String] [String]
               )

-- The configuration below also describes a full NsRestletConfig, but it uses the default user agent and fields.
let testCfg2 = (endpoint, 123456 :: Int, 1000 :: Int, "netsuite-user@yourcompany.example.com", "mypassword")
```

### Methods

All the functions below will return Value objects as defined by Data.Aeson.

Here's a rough example of fetching a customer info. Run this in ghci:

```haskell
retrieveNS testCfg "customer" 12345
```

You should get back an object containing all the fields for that customer.

A similar action is used for fetching credit cards, address books and so on:

```haskell
fetchSublistNS testCfg ("customer","creditcards") 12345
```

A raw search:

```haskell
rawSearchNS testCfg "customer" [toNsFilter ("lastmodifieddate",OnOrAfter,"daysAgo1")] [["externalid"], ["entityid"]]
```

And a simple search:

```haskell
searchNS testCfg "customer" [toNsFilter ("lastmodifieddate",OnOrAfter,"daysAgo1")]
```

You can compose your search filters using tuples passed to `toNsFilter`. Here are examples of instances that translate to filters:

```haskell
("foo",IsNotEmpty) -- NsFilter "foo" Nothing IsNotEmpty Nothing Nothing

("bar",Is,"123") -- NsFilter "foo" Nothing IsNotEmpty (Just 123) Nothing

("mydate",Between,"1/1/2015","12/31/2015") -- NsFilter "mydate" Nothing Between (Just "1/1/2015") (Just "12/31/2015")

("foo","widget",IsNotEmpty) -- NsFilter "foo" (Just "widget") IsNotEmpty Nothing Nothing

("bar","widget",Is,"123") -- NsFilter "bar" (Just "widget") Is (Just "123") Nothing

("mydate","widget",Between,"1/1/2015","12/31/2015") -- NsFilter "mydate" (Just "widget") Between (Just "1/1/2015") (Just "12/31/2015")
```

Creating a new contact:

```haskell
let d = ["firstname" .= "Jane", "lastname" .= "Doe", "email" .= "jane.doe@example.com"]
let subd = [("addressbook", [["addr1" .= "Unit 1", "addr2" .= "123 Sesame Street", "city" .= "Sydney", "state" .= "NSW", "zip" .= "2000", "country" .= "AU"]])]
createNS testCfg "contact" d subd
```

Updating an existing contact (123456):

```haskell
updateNS testCfg "contact" ["id" .= "123456", "firstname" .= "Wendy", "lastname" .= "Darling"]
```

Updating an existing contact's address book sublist:

```haskell
updateSublistNS testCfg ("contact","addressbook") 123456 [["addr1" .= "Second Star to the Left", "addr2", "Straight on 'til Morning", "city" .= "Lost Boys' Hideout", "state" .= "Neverland", "zip" .= "12345", "country" .= "GB"]]
```

Attaching a contact (123456) to a customer (12345), with a default role:

```haskell
attachNS testCfg "customer" [12345] "contact" 123456 []
```

Detaching a contact (123456) from a customer (12345):

```haskell
detachNS testCfg "customer" [12345] "contact" 123456
```

Deleting a contact record:

```haskell
deleteNS testCfg "contact" 123456
```

Downloading an invoice PDF:

```haskell
invoicePdfNS testCfg 123456
```

Transforming a customer to a sales order:

```haskell
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
