{-# LANGUAGE OverloadedStrings #-}

module Netsuite.Types.Fields (
    nsTypeFields,
    nsSubtypeFields,
) where

-- | Lists of fields per Netsuite record type
nsTypeFields :: [String] -> [String]

-- | Customer
nsTypeFields ("customer":_) = [
    "id",
    "entityid",
    "accessrole",
    "accountnumber",
    "altemail",
    "altphone",
    "autoname",
    "balance",
    "billaddr1",
    "billaddr2",
    "billaddr3",
    "billcity",
    "billcountry",
    "billpay",
    "billstate",
    "billzip",
    "buyingreason",
    "buyingtimeframe",
    "campaigncategory",
    "category",
    "clickstream",
    "comments",
    "companyname",
    "consolbalance",
    "consoldaysoverdue",
    "consoldepositbalance",
    "consoloverduebalance",
    "consolunbilledorders",
    "contact",
    "creditholdoverride",
    "creditlimit",
    "currency",
    "currencyprecision",
    "customform",
    "datecreated",
    "daysoverdue",
    "defaultaddress",
    "defaultbankaccount",
    "depositbalance",
    "draccount",
    "email",
    "emailpreference",
    "emailtransactions",
    "enddate",
    "entitystatus",
    "estimatedbudget",
    "externalid",
    "fax",
    "faxtransactions",
    "firstname",
    "firstvisit",
    "fxaccount",
    "giveaccess",
    "globalsubscriptionstatus",
    "homephone",
    "image",
    "isbudgetapproved",
    "isinactive",
    "isjob",
    "isperson",
    "keywords",
    "language",
    "lastmodifieddate",
    "lastname",
    "lastpagevisited",
    "lastvisit",
    "leadsource",
    "middlename",
    "mobilephone",
    "monthlyclosing",
    "negativenumberformat",
    "numberformat",
    "openingbalance",
    "openingbalanceaccount",
    "openingbalancedate",
    "overduebalance",
    "parent",
    "partner",
    "phone",
    "phoneticname",
    "prefccprocessor",
    "pricelevel",
    "printoncheckas",
    "printtransactions",
    "receivablesaccount",
    "referrer",
    "reminderdays",
    "representingsubsidiary",
    "resalenumber",
    "salesgroup",
    "salesreadiness",
    "salesrep",
    "salutation",
    "sendemail",
    "shipcomplete",
    "shippingcarrier",
    "shippingitem",
    "stage",
    "startdate",
    "strength",
    "subsidiary",
    "syncpartnerteams",
    "syncsalesteams",
    "taxable",
    "taxexempt",
    "taxfractionunit",
    "taxitem",
    "taxrounding",
    "terms",
    "territory",
    "thirdpartyacct",
    "thirdpartycarrier",
    "thirdpartycountry",
    "thirdpartyzipcode",
    "title",
    "unbilledorders",
    "unsubscribe",
    "url",
    "vatregnumber",
    "visits",
    "weblead"]

-- | Contact
nsTypeFields ("contact":_) = [
    "id",
    "role",
    "accessrole",
    "accountnumber",
    "altemail",
    "altphone",
    "autoname",
    "balance",
    "billaddr1",
    "billaddr2",
    "billaddr3",
    "billcity",
    "billcountry",
    "billpay",
    "billstate",
    "billzip",
    "buyingreason",
    "buyingtimeframe",
    "campaigncategory",
    "category",
    "clickstream",
    "comments",
    "companyname",
    "consolbalance",
    "consoldaysoverdue",
    "consoldepositbalance",
    "consoloverduebalance",
    "consolunbilledorders",
    "contact",
    "creditholdoverride",
    "creditlimit",
    "currency",
    "currencyprecision",
    "customform",
    "datecreated",
    "daysoverdue",
    "defaultaddress",
    "defaultbankaccount",
    "depositbalance",
    "draccount",
    "email",
    "emailpreference",
    "emailtransactions",
    "enddate",
    "entityid",
    "entitystatus",
    "estimatedbudget",
    "externalid",
    "fax",
    "faxtransactions",
    "firstname",
    "firstvisit",
    "fxaccount",
    "giveaccess",
    "globalsubscriptionstatus",
    "homephone",
    "image",
    "isbudgetapproved",
    "isinactive",
    "isjob",
    "isperson",
    "keywords",
    "language",
    "lastmodifieddate",
    "lastname",
    "lastpagevisited",
    "lastvisit",
    "leadsource",
    "middlename",
    "mobilephone",
    "monthlyclosing",
    "negativenumberformat",
    "numberformat",
    "openingbalance",
    "openingbalanceaccount",
    "openingbalancedate",
    "overduebalance",
    "parent",
    "partner",
    "phone",
    "phoneticname",
    "prefccprocessor",
    "pricelevel",
    "printoncheckas",
    "printtransactions",
    "receivablesaccount",
    "referrer",
    "reminderdays",
    "representingsubsidiary",
    "resalenumber",
    "salesgroup",
    "salesreadiness",
    "salesrep",
    "salutation",
    "sendemail",
    "shipcomplete",
    "shippingcarrier",
    "shippingitem",
    "stage",
    "startdate",
    "strength",
    "subsidiary",
    "syncpartnerteams",
    "syncsalesteams",
    "taxable",
    "taxexempt",
    "taxfractionunit",
    "taxitem",
    "taxrounding",
    "terms",
    "territory",
    "thirdpartyacct",
    "thirdpartycarrier",
    "thirdpartycountry",
    "thirdpartyzipcode",
    "title",
    "unbilledorders",
    "unsubscribe",
    "url",
    "vatregnumber",
    "visits",
    "weblead"]

-- | Credit Memo
nsTypeFields ("creditmemo":_) = [
    "id",
    "account",
    "althandlingcost",
    "altshippingcost",
    "amountpaid",
    "amountremaining",
    "applied",
    "autoapply",
    "balance",
    "billaddr1",
    "billaddr2",
    "billaddr3",
    "billaddress",
    "billaddressee",
    "billaddresslist",
    "billattention",
    "billcity",
    "billcountry",
    "billisresidential",
    "billphone",
    "billstate",
    "billzip",
    "class",
    "consolidatebalance",
    "couponcode",
    "createddate",
    "createdfrom",
    "currency",
    "currencyname",
    "currencysymbol",
    "customform",
    "deferredrevenue",
    "department",
    "discountitem",
    "discountrate",
    "discounttotal",
    "email",
    "entity",
    "entitynexus",
    "estgrossprofit",
    "estgrossprofitpercent",
    "exchangerate",
    "excludecommission",
    "externalid",
    "handlingcost",
    "handlingtax1rate",
    "handlingtaxcode",
    "isbasecurrency",
    "istaxable",
    "lastmodifieddate",
    "leadsource",
    "location",
    "memo",
    "message",
    "messagesel",
    "muccpromocodeinstance",
    "nexus",
    "otherrefnum",
    "partner",
    "postingperiod",
    "promocode",
    "promocodepluginimpl",
    "recognizedrevenue",
    "revenuestatus",
    "revreconrevcommitment",
    "saleseffectivedate",
    "salesgroup",
    "salesrep",
    "shipaddr1",
    "shipaddr2",
    "shipaddr3",
    "shipaddress",
    "shipaddressee",
    "shipaddresslist",
    "shipattention",
    "shipcity",
    "shipcountry",
    "shipisresidential",
    "shipmethod",
    "shipoverride",
    "shipphone",
    "shippingcost",
    "shippingcostoverridden",
    "shippingtax1rate",
    "shippingtaxcode",
    "shipstate",
    "shipzip",
    "source",
    "status",
    "statusRef",
    "subsidiary",
    "subtotal",
    "syncpartnerteams",
    "syncsalesteams",
    "taxitem",
    "taxrate",
    "taxtotal",
    "tobeemailed",
    "tobefaxed",
    "tobeprinted",
    "total",
    "totalcostestimate",
    "trandate",
    "tranid",
    "tranisvsoebundle",
    "unapplied",
    "unbilledorders",
    "vsoeautocalc"]

-- | Invoice
nsTypeFields ("invoice":_) = [
    "id",
    "account",
    "althandlingcost",
    "altshippingcost",
    "amountpaid",
    "amountremaining",
    "amountremainingtotalbox",
    "balance",
    "billaddr1",
    "billaddr2",
    "billaddr3",
    "billaddress",
    "billaddressee",
    "billaddresslist",
    "billattention",
    "billcity",
    "billcountry",
    "billisresidential",
    "billphone",
    "billstate",
    "billzip",
    "consolidatebalance",
    "couponcode",
    "createddate",
    "createdfrom",
    "currency",
    "currencyname",
    "currencysymbol",
    "customform",
    "deferredrevenue",
    "department",
    "discountamount",
    "discountdate",
    "discountitem",
    "discountrate",
    "discounttotal",
    "duedate",
    "email",
    "enddate",
    "entity",
    "entitynexus",
    "estgrossprofit",
    "estgrossprofitpercent",
    "exchangerate",
    "excludecommission",
    "expcostdiscamount",
    "expcostdiscount",
    "expcostdiscprint",
    "expcostdiscrate",
    "expcostdisctaxable",
    "expcosttaxcode",
    "expcosttaxrate1",
    "externalid",
    "fob",
    "giftcertapplied",
    "handlingcost",
    "handlingtax1rate",
    "handlingtaxcode",
    "isbasecurrency",
    "ismultishipto",
    "istaxable",
    "itemcostdiscamount",
    "itemcostdiscount",
    "itemcostdiscprint",
    "itemcostdiscrate",
    "itemcostdisctaxable",
    "itemcosttaxcode",
    "itemcosttaxrate1",
    "lastmodifieddate",
    "leadsource",
    "attachedtrackingnumbers",
    "location",
    "memo",
    "message",
    "messagesel",
    "muccpromocodeinstance",
    "nexus",
    "opportunity",
    "otherrefnum",
    "partner",
    "postingperiod",
    "promocode",
    "promocodepluginimpl",
    "recognizedrevenue",
    "returntrackingnumbers",
    "revenuestatus",
    "revreconrevcommitment",
    "saleseffectivedate",
    "salesgroup",
    "salesrep",
    "shipaddr1",
    "shipaddr2",
    "shipaddr3",
    "shipaddress",
    "shipaddressee",
    "shipaddresslist",
    "shipattention",
    "shipcity",
    "shipcountry",
    "shipdate",
    "shipisresidential",
    "shipmethod",
    "shipoverride",
    "shipphone",
    "shippingcost",
    "shippingcostoverridden",
    "shippingtax1rate",
    "shippingtaxcode",
    "shipstate",
    "shipzip",
    "source",
    "startdate",
    "status",
    "statusRef",
    "subsidiary",
    "subtotal",
    "syncpartnerteams",
    "syncsalesteams",
    "taxitem",
    "taxrate",
    "taxtotal",
    "terms",
    "timediscamount",
    "timediscount",
    "timediscprint",
    "timediscrate",
    "timedisctaxable",
    "timetaxcode",
    "timetaxrate1",
    "tobeemailed",
    "tobefaxed",
    "tobeprinted",
    "total",
    "totalcostestimate",
    "trackingnumbers",
    "trandate",
    "tranid",
    "tranisvsoebundle",
    "unbilledorders",
    "vsoeautocalc"]

-- | Customer Payment
nsTypeFields ("customerpayment":_) = [
    "id",
    "account",
    "allowemptycards",
    "applied",
    "aracct",
    "authcode",
    "autoapply",
    "balance",
    "ccapproved",
    "ccavsstreetmatch",
    "ccavszipmatch",
    "ccexpiredate",
    "cchold",
    "ccholdetails",
    "cciavsmatch",
    "ccispurchasecardbin",
    "ccname",
    "ccnumber",
    "ccprocessoraccount",
    "ccsecuritycode",
    "ccsecuritycodematch",
    "ccstreet",
    "cczipcode",
    "chargeit",
    "checknum",
    "class",
    "consolidatebalance",
    "createddate",
    "creditcard",
    "creditcardprocessor",
    "currency",
    "currencyname",
    "currencysymbol",
    "customer",
    "customercode",
    "customform",
    "debitcardissueno",
    "department",
    "entitynexus",
    "exchangerate",
    "externalid",
    "ignoreavs",
    "ignorecsc",
    "isbasecurrency",
    "ispurchasecard",
    "lastmodifieddate",
    "location",
    "memo",
    "nexus",
    "overridehold",
    "overrideholdchecked",
    "payment",
    "paymenteventdate",
    "paymenteventholdreason",
    "paymenteventpurchasedatasent",
    "paymenteventresult",
    "paymenteventtype",
    "paymenteventupdatedby",
    "paymentmethod",
    "pending",
    "pnrefnum",
    "postingperiod",
    "softdescriptor",
    "status",
    "statusRef",
    "subsidiary",
    "threedstatuscode",
    "tobeemailed",
    "total",
    "trandate",
    "tranid",
    "unapplied",
    "undepfunds",
    "validfrom"]

-- | Customer Deposit
nsTypeFields ("customerdeposit":_) = [
    "id",
    "account",
    "allowemptycards",
    "authcode",
    "ccapproved",
    "ccavsstreetmatch",
    "ccavszipmatch",
    "ccexpiredate",
    "cchold",
    "ccholdetails",
    "cciavsmatch",
    "ccispurchasecardbin",
    "ccname",
    "ccnumber",
    "ccprocessoraccount",
    "ccsecuritycode",
    "ccsecuritycodematch",
    "ccstreet",
    "cczipcode",
    "chargeit",
    "checknum",
    "class",
    "createddate",
    "creditcard",
    "creditcardprocessor",
    "currency",
    "currencyname",
    "currencysymbol",
    "customer",
    "customercode",
    "customform",
    "debitcardissueno",
    "department",
    "entitynexus",
    "exchangerate",
    "externalid",
    "ignoreavs",
    "ignorecsc",
    "isbasecurrency",
    "ispurchasecard",
    "lastmodifieddate",
    "location",
    "memo",
    "nexus",
    "overridehold",
    "overrideholdchecked",
    "payment",
    "paymenteventdate",
    "paymenteventholdreason",
    "paymenteventpurchasedatasent",
    "paymenteventresult",
    "paymenteventtype",
    "paymenteventupdatedby",
    "paymentmethod",
    "pnrefnum",
    "postingperiod",
    "salesorder",
    "softdescriptor",
    "status",
    "statusRef",
    "subsidiary",
    "threedstatuscode",
    "tobeemailed",
    "trandate",
    "tranid",
    "undepfunds",
    "validfrom"]

-- | Non-Inventory Item
nsTypeFields ("noninventoryitem":_) = [
    "id",
    "amortizationperiod",
    "amortizationtemplate",
    "auctionquantity",
    "auctiontype",
    "availabletopartners",
    "billingschedule",
    "buyitnowprice",
    "class",
    "conditionenabled",
    "conditionhelpurl",
    "copydescription",
    "cost",
    "costcategory",
    "costestimate",
    "costestimatetype",
    "costunits",
    "countryofmanufacture",
    "createddate",
    "currency",
    "customform",
    "deferralaccount",
    "deferredrevenueaccount",
    "department",
    "displayinebaystore",
    "displayname",
    "dontshowprice",
    "dropshipexpenseaccount",
    "ebayhandlingtime",
    "ebayintlinsurancefee",
    "ebayintlpackagehandlingfee",
    "ebayintlshipinsurance",
    "ebayintlshippingitem1",
    "ebayintlshippingitem2",
    "ebayintlshippingitem3",
    "ebayisintlcalculatedrate",
    "ebayisirregularpackage",
    "ebayitemdescription",
    "ebayitemlocdisplay",
    "ebayitemloczipcode",
    "ebayitemlots",
    "ebayitemsubtitle",
    "ebayitemtitle",
    "ebayitemweightamt",
    "ebaylayout",
    "ebaypackagetype",
    "ebaypagecounter",
    "ebayrelistingoption",
    "ebaytheme",
    "ebaythemegroup",
    "endauctionswhenoutofstock",
    "enforceminqtyinternally",
    "excludefromsitemap",
    "expenseaccount",
    "externalid",
    "featureddescription",
    "froogleproductfeed",
    "gallery",
    "galleryfeatured",
    "gifttypeexpressship",
    "gifttypegiftwrap",
    "gifttypeshiptorecipient",
    "handlingcost",
    "handlingcostunits",
    "handlinggroup",
    "imagesgroup",
    "imageslocation",
    "includechildren",
    "incomeaccount",
    "internalid",
    "iscalculatedrate",
    "isdonationitem",
    "isdropshipitem",
    "isfulfillable",
    "isgcocompliant",
    "isinactive",
    "isonline",
    "isspecialorderitem",
    "issueproduct",
    "itemcondition",
    "itemhandlingfee",
    "itemid",
    "iteminsurancefee",
    "itemoptions",
    "itemshipinsurance",
    "itemtype",
    "lastmodifieddate",
    "listimmediate",
    "listingduration",
    "listingstartdate",
    "listingstarttime",
    "location",
    "manufacturer",
    "manufactureraddr1",
    "manufacturercity",
    "manufacturerstate",
    "manufacturertariff",
    "manufacturertaxid",
    "manufacturerzip",
    "matrixtype",
    "maxdonationamount",
    "metataghtml",
    "minimumquantity",
    "minimumquantityunits",
    "mpn",
    "multmanufactureaddr",
    "nextagcategory",
    "nextagproductfeed",
    "nopricemessage",
    "numactivelistings",
    "numcurrentlylisted",
    "offersupport",
    "outofstockbehavior",
    "outofstockmessage",
    "overallquantitypricingtype",
    "packageheight",
    "packagelength",
    "packagewidth",
    "pagetitle",
    "parent",
    "preferencecriterion",
    "pricinggroup",
    "primarycatdisplayname",
    "primarycategory",
    "producer",
    "productfeed",
    "purchasedescription",
    "purchaseunit",
    "quantitypricingschedule",
    "refundgivenas",
    "relateditemsdescription",
    "reserveprice",
    "residual",
    "returnpolicy",
    "returnpolicydetails",
    "returnshippingpaidby",
    "returnswithin",
    "revrecschedule",
    "salesdescription",
    "saleunit",
    "schedulebcode",
    "schedulebnumber",
    "schedulebquantity",
    "searchkeywords",
    "secondarycatdisplayname",
    "secondarycategory",
    "sellonebay",
    "shipasia",
    "shipaustralia",
    "shipcanada",
    "shipeurope",
    "shipgermany",
    "shipindividually",
    "shipjapan",
    "shipmexico",
    "shipnorthsouthamerica",
    "shippackage",
    "shippingcost",
    "shippingcostunits",
    "shippingdomesticmethodsgroup",
    "shippingdomgroup",
    "shippingintlgroup",
    "shippingintlgroup1",
    "shippingintlgroup2",
    "shippingintlgroup3",
    "shippingitem1",
    "shippingitem2",
    "shippingitem3",
    "shippinglocationsgroup",
    "shippingpackaginggroup",
    "shippingrate1",
    "shippingrate2",
    "shippingrate3",
    "shipuk",
    "shipworldwide",
    "shoppingdotcomcategory",
    "shoppingproductfeed",
    "shopzillacategoryid",
    "shopzillaproductfeed",
    "showasgift",
    "showdefaultdonationamount",
    "sitemappriority",
    "softdescriptor",
    "standardimages",
    "startingprice",
    "stockdescription",
    "storecatdisplayname",
    "storecatdisplayname2",
    "storecategory",
    "storecategory2",
    "storedescription",
    "storedetaileddescription",
    "storedisplayimage",
    "storedisplayname",
    "storedisplaythumbnail",
    "storeitemtemplate",
    "subsidiary",
    "subtype",
    "supersizeimages",
    "taxable",
    "taxschedule",
    "templatesgroup",
    "unitstype",
    "upccode",
    "urlcomponent",
    "usemarginalrates",
    "vendorname",
    "vsoedeferral",
    "vsoedelivered",
    "vsoepermitdiscount",
    "vsoeprice",
    "vsoesopgroup",
    "weight",
    "weightunit",
    "weightunits",
    "willship",
    "yahooproductfeed"]

-- | Discount Item
nsTypeFields ("discountitem":_) = [
    "id",
    "account",
    "availabletopartners",
    "class",
    "createddate",
    "customform",
    "department",
    "description",
    "displayname",
    "externalid",
    "includechildren",
    "isinactive",
    "ispretax",
    "issueproduct",
    "itemid",
    "itemtype",
    "lastmodifieddate",
    "location",
    "nonposting",
    "parent",
    "rate",
    "subsidiary",
    "taxschedule",
    "upccode",
    "vendorname"]

-- | Price level
nsTypeFields ("pricelevel":_) = [
    "id",
    "discountpct",
    "externalid",
    "isinactive",
    "isonline",
    "name",
    "updateexistingprices"]

-- | Sales Order
nsTypeFields ("salesorder":_) = [
    "id",
    "allowemptycards",
    "althandlingcost",
    "altsalestotal",
    "altshippingcost",
    "authcode",
    "balance",
    "billaddr3",
    "billaddress",
    "billaddressee",
    "billaddresslist",
    "billattention",
    "billcity",
    "billcountry",
    "billingschedule",
    "billisresidential",
    "billphone",
    "billstate",
    "billzip",
    "ccapproved",
    "ccavsstreetmatch",
    "ccavszipmatch",
    "ccexpiredate",
    "cchold",
    "ccholdetails",
    "cciavsmatch",
    "ccname",
    "ccnumber",
    "ccprocessoraccount",
    "ccsecuritycode",
    "ccsecuritycodematch",
    "ccstreet",
    "cczipcode",
    "class",
    "consolidatebalance",
    "couponcode",
    "createddate",
    "createdfrom",
    "creditcard",
    "creditcardprocessor",
    "currency",
    "currencyname",
    "currencysymbol",
    "customercode",
    "customform",
    "debitcardissueno",
    "deferredrevenue",
    "department",
    "discountitem",
    "discountrate",
    "discounttotal",
    "draccount",
    "email",
    "enddate",
    "entity",
    "entitynexus",
    "estgrossprofit",
    "estgrossprofitpercent",
    "exchangerate",
    "excludecommission",
    "externalid",
    "fob",
    "fxaccount",
    "getauth",
    "giftcertapplied",
    "handlingcost",
    "handlingtax1rate",
    "handlingtaxcode",
    "ignoreavs",
    "ignorecsc",
    "intercostatus",
    "intercotransaction",
    "isbasecurrency",
    "isdefaultshippingrequest",
    "ismultishipto",
    "ispurchasecard",
    "istaxable",
    "lastmodifieddate",
    "leadsource",
    "linkedtrackingnumbers",
    "location",
    "memo",
    "message",
    "messagesel",
    "muccpromocodeinstance",
    "nexus",
    "opportunity",
    "orderstatus",
    "otherrefnum",
    "overridehold",
    "overrideholdchecked",
    "overrideshippingcost",
    "partner",
    "paymenteventdate",
    "paymenteventholdreason",
    "paymenteventpurchasedatasent",
    "paymenteventresult",
    "paymenteventtype",
    "paymenteventupdatedby",
    "paymentmethod",
    "paypalauthid",
    "paypalprocess",
    "paypalstatus",
    "paypaltranid",
    "pnrefnum",
    "promocode",
    "promocodepluginimpl",
    "recognizedrevenue",
    "returntrackingnumbers",
    "revcommitstatus",
    "revenuestatus",
    "revreconrevcommitment",
    "saleseffectivedate",
    "salesgroup",
    "salesrep",
    "shipaddr3",
    "shipaddress",
    "shipaddressee",
    "shipaddresslist",
    "shipattention",
    "shipcity",
    "shipcomplete",
    "shipcountry",
    "shipdate",
    "shipisresidential",
    "shipmethod",
    "shipoverride",
    "shipphone",
    "shippingcost",
    "shippingcostoverridden",
    "shippingtax1rate",
    "shippingtaxcode",
    "shipstate",
    "shipzip",
    "softdescriptor",
    "source",
    "startdate",
    "status",
    "statusRef",
    "subsidiary",
    "subtotal",
    "syncpartnerteams",
    "syncsalesteams",
    "taxitem",
    "taxrate",
    "taxtotal",
    "terms",
    "threedstatuscode",
    "tobeemailed",
    "tobefaxed",
    "tobeprinted",
    "total",
    "totalcostestimate",
    "trandate",
    "tranid",
    "tranisvsoebundle",
    "unbilledorders",
    "validfrom",
    "vsoeautocalc"]

-- | serviceitem
nsTypeFields ("serviceitem":_) = [
    "amortizationperiod"
  , "amortizationtemplate"
  , "auctionquantity"
  , "auctiontype"
  , "availabletopartners"
  , "billingschedule"
  , "buyitnowprice"
  , "class"
  , "conditionenabled"
  , "conditionhelpurl"
  , "cost"
  , "costcategory"
  , "costestimate"
  , "costestimatetype"
  , "costunits"
  , "createddate"
  , "createjob"
  , "currency"
  , "customform"
  , "deferralaccount"
  , "deferredrevenueaccount"
  , "department"
  , "displayinebaystore"
  , "displayname"
  , "dontshowprice"
  , "ebayhandlingtime"
  , "ebayintlinsurancefee"
  , "ebayintlpackagehandlingfee"
  , "ebayintlshipinsurance"
  , "ebayintlshippingitem1"
  , "ebayintlshippingitem2"
  , "ebayintlshippingitem3"
  , "ebayisintlcalculatedrate"
  , "ebayisirregularpackage"
  , "ebayitemdescription"
  , "ebayitemlocdisplay"
  , "ebayitemloczipcode"
  , "ebayitemlots"
  , "ebayitemsubtitle"
  , "ebayitemtitle"
  , "ebayitemweightamt"
  , "ebaylayout"
  , "ebaypackagetype"
  , "ebaypagecounter"
  , "ebayrelistingoption"
  , "ebaytheme"
  , "ebaythemegroup"
  , "endauctionswhenoutofstock"
  , "enforceminqtyinternally"
  , "excludefromsitemap"
  , "expenseaccount"
  , "externalid"
  , "featureddescription"
  , "gallery"
  , "galleryfeatured"
  , "gifttypeexpressship"
  , "gifttypegiftwrap"
  , "gifttypeshiptorecipient"
  , "handlinggroup"
  , "imagesgroup"
  , "imageslocation"
  , "includechildren"
  , "incomeaccount"
  , "internalid"
  , "iscalculatedrate"
  , "isdonationitem"
  , "isfulfillable"
  , "isgcocompliant"
  , "isinactive"
  , "isonline"
  , "issueproduct"
  , "itemcondition"
  , "itemhandlingfee"
  , "itemid"
  , "iteminsurancefee"
  , "itemoptions"
  , "itemshipinsurance"
  , "itemtype"
  , "lastmodifieddate"
  , "listimmediate"
  , "listingduration"
  , "listingstartdate"
  , "listingstarttime"
  , "location"
  , "matrixtype"
  , "maxdonationamount"
  , "metataghtml"
  , "minimumquantity"
  , "minimumquantityunits"
  , "nopricemessage"
  , "numactivelistings"
  , "numcurrentlylisted"
  , "offersupport"
  , "outofstockbehavior"
  , "outofstockmessage"
  , "packageheight"
  , "packagelength"
  , "packagewidth"
  , "pagetitle"
  , "parent"
  , "pricinggroup"
  , "primarycatdisplayname"
  , "primarycategory"
  , "purchasedescription"
  , "purchaseunit"
  , "refundgivenas"
  , "relateditemsdescription"
  , "reserveprice"
  , "residual"
  , "returnpolicy"
  , "returnpolicydetails"
  , "returnshippingpaidby"
  , "returnswithin"
  , "revrecschedule"
  , "salesdescription"
  , "saleunit"
  , "searchkeywords"
  , "secondarycatdisplayname"
  , "secondarycategory"
  , "sellonebay"
  , "shipasia"
  , "shipaustralia"
  , "shipcanada"
  , "shipeurope"
  , "shipgermany"
  , "shipjapan"
  , "shipmexico"
  , "shipnorthsouthamerica"
  , "shippingdomesticmethodsgroup"
  , "shippingdomgroup"
  , "shippingintlgroup"
  , "shippingintlgroup1"
  , "shippingintlgroup2"
  , "shippingintlgroup3"
  , "shippingitem1"
  , "shippingitem2"
  , "shippingitem3"
  , "shippinglocationsgroup"
  , "shippingpackaginggroup"
  , "shippingrate1"
  , "shippingrate2"
  , "shippingrate3"
  , "shipuk"
  , "shipworldwide"
  , "showasgift"
  , "showdefaultdonationamount"
  , "sitemappriority"
  , "softdescriptor"
  , "standardimages"
  , "startingprice"
  , "storecatdisplayname"
  , "storecatdisplayname2"
  , "storecategory"
  , "storecategory2"
  , "storedescription"
  , "storedetaileddescription"
  , "storedisplayimage"
  , "storedisplayname"
  , "storedisplaythumbnail"
  , "storeitemtemplate"
  , "subsidiary"
  , "subtype"
  , "supersizeimages"
  , "taxable"
  , "taxschedule"
  , "templatesgroup"
  , "unitstype"
  , "upccode"
  , "urlcomponent"
  , "vendorname"
  , "vsoedeferral"
  , "vsoedelivered"
  , "vsoepermitdiscount"
  , "vsoeprice"
  , "vsoesopgroup"
  , "willship" ]

-- | inventorydetail
nsTypeFields ("inventorydetail":_) = [
    "customform"
  , "externalid"
  , "item"
  , "itemdescription"
  , "location"
  , "quantity"
  , "tolocation"
  , "unit" ]

-- | lead
nsTypeFields ("lead":_) = [
    "accessrole"
  , "accountnumber"
  , "altemail"
  , "altphone"
  , "autoname"
  , "balance"
  , "billaddr1"
  , "billaddr2"
  , "billaddr3"
  , "billcity"
  , "billcountry"
  , "billstate"
  , "billzip"
  , "buyingreason"
  , "buyingtimeframe"
  , "campaigncategory"
  , "category"
  , "clickstream"
  , "comments"
  , "companyname"
  , "consolbalance"
  , "consoldaysoverdue"
  , "consoldepositbalance"
  , "consoloverduebalance"
  , "consolunbilledorders"
  , "contact"
  , "creditholdoverride"
  , "creditlimit"
  , "currency"
  , "currencyprecision"
  , "customform"
  , "datecreated"
  , "daysoverdue"
  , "defaultaddress"
  , "defaultbankaccount"
  , "depositbalance"
  , "draccount"
  , "email"
  , "emailpreference"
  , "emailtransactions"
  , "entityid"
  , "entitystatus"
  , "estimatedbudget"
  , "externalid"
  , "fax"
  , "faxtransactions"
  , "firstname"
  , "firstvisit"
  , "fxaccount"
  , "giveaccess"
  , "globalsubscriptionstatus"
  , "homephone"
  , "image"
  , "isbudgetapproved"
  , "isinactive"
  , "isjob"
  , "isperson"
  , "keywords"
  , "language"
  , "lastmodifieddate"
  , "lastname"
  , "lastpagevisited"
  , "lastvisit"
  , "leadsource"
  , "middlename"
  , "mobilephone"
  , "monthlyclosing"
  , "negativenumberformat"
  , "numberformat"
  , "otherrelationships"
  , "overduebalance"
  , "parent"
  , "partner"
  , "phone"
  , "phoneticname"
  , "prefccprocessor"
  , "pricelevel"
  , "printoncheckas"
  , "printtransactions"
  , "receivablesaccount"
  , "referrer"
  , "resalenumber"
  , "salesgroup"
  , "salesreadiness"
  , "salesrep"
  , "salutation"
  , "sendemail"
  , "stage"
  , "strength"
  , "subsidiary"
  , "syncpartnerteams"
  , "syncsalesteams"
  , "taxable"
  , "taxexempt"
  , "taxfractionunit"
  , "taxitem"
  , "taxrounding"
  , "terms"
  , "territory"
  , "title"
  , "unbilledorders"
  , "unsubscribe"
  , "url"
  , "vatregnumber"
  , "visits"
  , "weblead" ]

-- | message
nsTypeFields ("message":_) = [
    "activity"
  , "author"
  , "authoremail"
  , "bcc"
  , "cc"
  , "contact"
  , "emailed"
  , "entity"
  , "entitytype"
  , "externalid"
  , "hasattachment"
  , "lastmodifieddate"
  , "message"
  , "recipient"
  , "recipientemail"
  , "record"
  , "recordtype"
  , "subject"
  , "template"
  , "time"
  , "transaction" ]

-- | opportunity
nsTypeFields ("opportunity":_) = [
    "actionitem"
  , "altsalesrangehigh"
  , "altsalesrangelow"
  , "altsalestotal"
  , "balance"
  , "billaddr1"
  , "billaddr2"
  , "billaddr3"
  , "billaddress"
  , "billaddressee"
  , "billaddresslist"
  , "billattention"
  , "billcity"
  , "billcountry"
  , "billisresidential"
  , "billphone"
  , "billstate"
  , "billzip"
  , "buyingreason"
  , "buyingtimeframe"
  , "class"
  , "consolidatebalance"
  , "createddate"
  , "currency"
  , "currencyname"
  , "currencysymbol"
  , "customform"
  , "department"
  , "documentstatus"
  , "entity"
  , "entitynexus"
  , "entitystatus"
  , "estgrossprofit"
  , "estgrossprofitpercent"
  , "estimatedbudget"
  , "exchangerate"
  , "expectedclosedate"
  , "externalid"
  , "forecasttype"
  , "isbasecurrency"
  , "isbudgetapproved"
  , "job"
  , "lastmodifieddate"
  , "leadsource"
  , "location"
  , "memo"
  , "nexus"
  , "partner"
  , "probability"
  , "projaltsalesamt"
  , "projectedtotal"
  , "rangehigh"
  , "rangelow"
  , "salesgroup"
  , "salesreadiness"
  , "salesrep"
  , "shipaddr1"
  , "shipaddr2"
  , "shipaddr3"
  , "shipaddress"
  , "shipaddressee"
  , "shipaddresslist"
  , "shipattention"
  , "shipcity"
  , "shipcountry"
  , "shipisresidential"
  , "shipoverride"
  , "shipphone"
  , "shipstate"
  , "shipzip"
  , "source"
  , "status"
  , "statusRef"
  , "subsidiary"
  , "syncpartnerteams"
  , "syncsalesteams"
  , "title"
  , "total"
  , "totalcostestimate"
  , "trandate"
  , "tranid"
  , "unbilledorders"
  , "weightedtotal"
  , "winlossreason" ]

-- * INSERT NEW TYPE FIELDS AFTER THIS POINT *

-- * INSERT NEW TYPE FIELDS BEFORE THIS POINT *

-- | Catch-all
nsTypeFields _ = []

-- | Lists of fields per Netsuite record type
nsSubtypeFields :: [String] -> [String]

-- | Customer > Address Book
nsSubtypeFields ("customer":"addressbook":_) = [
    "id",
    "internalid",
    "addr1",
    "addr2",
    "addr3",
    "addressee",
    "addressid",
    "addrtext",
    "attention",
    "city",
    "country",
    "defaultbilling",
    "defaultshipping",
    "displaystate",
    "isresidential",
    "label",
    "override",
    "phone",
    "state",
    "zip"]

-- | Customer > Contact Roles
nsSubtypeFields ("customer":"contactroles":_) = [
    "contact",
    "email",
    "giveaccess",
    "passwordconfirm",
    "role",
    "sendemail",
    "strength"]

-- | Customer > Credit Cards
nsSubtypeFields ("customer":"creditcards":_) = [
    "ccdefault",
    "ccexpiredate",
    "ccmemo",
    "ccname",
    "ccnumber",
    "customercode",
    "debitcardissueno",
    "internalid",
    "paymentmethod",
    "validfrom"]

-- | Customer > Currency
nsSubtypeFields ("customer":"currency":_) = [
    "balance",
    "consolbalance",
    "consoldepositbalance",
    "consoloverduebalance",
    "consolunbilledorders",
    "currency",
    "currencyformatsample",
    "depositbalance",
    "displaysymbol",
    "overduebalance",
    "overridecurrencyformat",
    "symbolplacement",
    "unbilledorders"]

-- | Customer > Download
nsSubtypeFields ("customer":"download":_) = [
    "expiration",
    "file",
    "licensecode",
    "remainingdownloads"]

-- | Customer > Group Pricing
nsSubtypeFields ("customer":"grouppricing":_) = [
    "group",
    "level"]

-- | Customer > Item Pricing
nsSubtypeFields ("customer":"itempricing":_) = [
    "currency",
    "item",
    "level",
    "price"]

-- | Customer > Partners
nsSubtypeFields ("customer":"partners":_) = [
    "contribution",
    "customer",
    "id",
    "isprimary",
    "partner",
    "partnerrole"]

-- | Customer > Sales Team
nsSubtypeFields ("customer":"salesteam":_) = [
    "contribution",
    "customer",
    "employee",
    "id",
    "isprimary",
    "issalesrep",
    "salesrole"]

-- | Contact > Address Book
nsSubtypeFields ("contact":"addressbook":_) = [
    "id",
    "internalid",
    "addr1",
    "addr2",
    "addr3",
    "addressee",
    "addressid",
    "addrtext",
    "attention",
    "city",
    "country",
    "defaultbilling",
    "defaultshipping",
    "displaystate",
    "label",
    "override",
    "phone",
    "state",
    "zip"]

-- | Invoice > Expense Cost
nsSubtypeFields ("invoice":"expcost":_) = [
    "amortizationperiod",
    "amortizationtype",
    "amount",
    "apply",
    "billeddate",
    "category",
    "doc",
    "employee",
    "job",
    "line",
    "location",
    "memo",
    "originalamount",
    "revrecenddate",
    "revrecschedule",
    "revrecstartdate",
    "taxable",
    "taxcode",
    "taxrate1",
    "url"]

-- | Invoice > Line Item
nsSubtypeFields ("invoice":"item":_) = [
    "account",
    "amortizationperiod",
    "amortizationtype",
    "amount",
    "billvariancestatus",
    "costestimate",
    "costestimaterate",
    "costestimatetype",
    "daysbeforeexpiration",
    "deferrevrec",
    "description",
    "giftcertfrom",
    "giftcertmessage",
    "giftcertrecipientemail",
    "giftcertrecipientname",
    "id",
    "inventorydetail",
    "istaxable",
    "isvsoebundle",
    "item",
    "itemsubtype",
    "itemtype",
    "job",
    "licensecode",
    "line",
    "linenumber",
    "matrixtype",
    "options",
    "price",
    "printitems",
    "quantity",
    "quantityavailable",
    "quantityremaining",
    "rate",
    "rateschedule",
    "revrecenddate",
    "revrecschedule",
    "revrecstartdate",
    "shipaddress",
    "shipcarrier",
    "shipmethod",
    "taxcode",
    "taxrate1",
    "units",
    "vsoeallocation",
    "vsoeamount",
    "vsoedeferral",
    "vsoedelivered",
    "vsoeisestimate",
    "vsoepermitdiscount",
    "vsoeprice",
    "vsoesopgroup"]

-- | Invoice > Line Item Cost
nsSubtypeFields ("invoice":"itemcost":_) = [
    "amortizationperiod",
    "amortizationtype",
    "amount",
    "apply",
    "billeddate",
    "binnumbers",
    "cost",
    "doc",
    "item",
    "itemcostcount",
    "job",
    "line",
    "location",
    "memo",
    "options",
    "rateschedule",
    "revrecenddate",
    "revrecschedule",
    "revrecstartdate",
    "serialnumbers",
    "taxable",
    "taxcode",
    "taxrate1",
    "unit",
    "url"]

-- | Invoice > Partners
nsSubtypeFields ("invoice":"partners":_) = [
    "contribution",
    "id",
    "isprimary",
    "partner",
    "partnerrole",
    "transaction"]

-- | Invoice > Sales Team
nsSubtypeFields ("invoice":"salesteam":_) = [
    "contribution",
    "employee",
    "id",
    "isprimary",
    "issalesrep",
    "salesrole",
    "transaction"]

-- | Invoice > Shipping Group
nsSubtypeFields ("invoice":"shipgroup":_) = [
    "destinationaddress",
    "handlingrate",
    "id",
    "shippingcarrier",
    "shippingmethod",
    "shippingrate",
    "sourceaddress",
    "weight"]

-- | Invoice > Time
nsSubtypeFields ("invoice":"time":_) = [
    "amortizationperiod",
    "amortizationtype",
    "amount",
    "apply",
    "billeddate",
    "doc",
    "item",
    "job",
    "memo",
    "rate",
    "rateschedule",
    "revrecenddate",
    "revrecschedule",
    "revrecstartdate",
    "taxable",
    "taxcode",
    "taxrate1",
    "unit",
    "url"]

-- | Customer Payment > Invoices Applied To
nsSubtypeFields ("customerpayment":"apply":_) = [
    "amount",
    "apply",
    "applydate",
    "createdfrom",
    "disc",
    "discamt",
    "discdate",
    "doc",
    "due",
    "duedate",
    "internalid",
    "job",
    "line",
    "refnum",
    "total",
    "url"]

-- | Customer Payment > Credit
nsSubtypeFields ("customerpayment":"credit":_) = [
    "amount",
    "apply",
    "createdfrom",
    "creditdate",
    "doc",
    "due",
    "duedate",
    "internalid",
    "line",
    "refnum",
    "total",
    "url"]

-- | Customer Payment > Deposit
nsSubtypeFields ("customerpayment":"deposit":_) = [
    "amount",
    "apply",
    "currency",
    "depositdate",
    "doc",
    "remaining",
    "total",
    "url"]

-- | Credit Memo > Invoices Applied To
nsSubtypeFields ("creditmemo":"apply":_) = [
    "amount",
    "apply",
    "applydate",
    "createdfrom",
    "doc",
    "due",
    "duedate",
    "internalid",
    "job",
    "line",
    "refnum",
    "total",
    "url"]

-- | Credit Memo > Line Item
nsSubtypeFields ("creditmemo":"item":_) = [
    "account",
    "amortizationperiod",
    "amortizationtype",
    "amount",
    "billvariancestatus",
    "costestimate",
    "costestimaterate",
    "costestimatetype",
    "daysbeforeexpiration",
    "deferrevrec",
    "description",
    "giftcertfrom",
    "giftcertmessage",
    "giftcertrecipientemail",
    "giftcertrecipientname",
    "id",
    "inventorydetail",
    "isdropshipment",
    "istaxable",
    "isvsoebundle",
    "item",
    "itemsubtype",
    "itemtype",
    "job",
    "line",
    "linenumber",
    "matrixtype",
    "options",
    "price",
    "printitems",
    "quantity",
    "rate",
    "rateschedule",
    "revrecenddate",
    "revrecschedule",
    "revrecstartdate",
    "taxcode",
    "taxrate1",
    "units",
    "vsoeallocation",
    "vsoeamount",
    "vsoedeferral",
    "vsoedelivered",
    "vsoeisestimate",
    "vsoepermitdiscount",
    "vsoeprice",
    "vsoesopgroup"]

-- | Credit Memo > Partners
nsSubtypeFields ("creditmemo":"partners":_) = [
    "contribution",
    "id",
    "isprimary",
    "partner",
    "partnerrole",
    "transaction"]

-- | Credit Memo > Sales Team
nsSubtypeFields ("creditmemo":"salesteam":_) = [
    "contribution",
    "employee",
    "id",
    "isprimary",
    "issalesrep",
    "salesrole",
    "transaction"]

-- | Non-Inventory Item > Price1
nsSubtypeFields ("noninventoryitem":"price1":_) = [
    "currency",
    "discount",
    "discountdisplay",
    "pricelevel"]

-- | Non-Inventory Item > Price2
nsSubtypeFields ("noninventoryitem":"price2":_) = [
    "currency",
    "discount",
    "discountdisplay",
    "pricelevel"]

-- | Non-Inventory Item > Price3
nsSubtypeFields ("noninventoryitem":"price3":_) = [
    "currency",
    "discount",
    "discountdisplay",
    "pricelevel"]

-- | Non-Inventory Item > Price4
nsSubtypeFields ("noninventoryitem":"price4":_) = [
    "currency",
    "discount",
    "discountdisplay",
    "pricelevel"]

-- | Non-Inventory Item > Price5
nsSubtypeFields ("noninventoryitem":"price5":_) = [
    "currency",
    "discount",
    "discountdisplay",
    "pricelevel"]

-- | Non-Inventory Item > Price6
nsSubtypeFields ("noninventoryitem":"price6":_) = [
    "currency",
    "discount",
    "discountdisplay",
    "pricelevel"]

-- | Non-Inventory Item > Site Category
nsSubtypeFields ("noninventoryitem":"sitecategory":_) = [
    "category",
    "categorydescription",
    "isdefault",
    "website"]

-- | Sales Order > Item
nsSubtypeFields ("salesorder":"item":_) = [
    "altsalesamt",
    "amortizationperiod",
    "amortizationtype",
    "amount",
    "billvariancestatus",
    "commitinventory",
    "costestimate",
    "costestimaterate",
    "costestimatetype",
    "createdpo",
    "createpo",
    "createwo",
    "daysbeforeexpiration",
    "deferrevrec",
    "description",
    "expectedshipdate",
    "fromjob",
    "giftcertfrom",
    "giftcertmessage",
    "giftcertrecipientemail",
    "giftcertrecipientname",
    "id",
    "inventorydetail",
    "isclosed",
    "isestimate",
    "istaxable",
    "isvsoebundle",
    "item",
    "itemsubtype",
    "itemtype",
    "job",
    "licensecode",
    "line",
    "linenumber",
    "matrixtype",
    "options",
    "porate",
    "povendor",
    "price",
    "printitems",
    "quantity",
    "quantityavailable",
    "quantitybackordered",
    "quantitybilled",
    "quantitycommitted",
    "quantityfulfilled",
    "quantityrevcommitted",
    "rate",
    "rateschedule",
    "revrecenddate",
    "revrecschedule",
    "revrecstartdate",
    "shipaddress",
    "shipcarrier",
    "shipmethod",
    "taxcode",
    "taxrate1",
    "units",
    "vsoeallocation",
    "vsoeamount",
    "vsoedeferral",
    "vsoedelivered",
    "vsoeisestimate",
    "vsoepermitdiscount",
    "vsoeprice",
    "vsoesopgroup"]

-- | Sales Order > Partners
nsSubtypeFields ("salesorder":"partners":_) = [
    "contribution",
    "id",
    "isprimary",
    "partner",
    "partnerrole",
    "transaction"]

-- | Sales Order > Sales Team
nsSubtypeFields ("salesorder":"salesteam":_) = [
    "contribution",
    "employee",
    "id",
    "isprimary",
    "issalesrep",
    "salesrole",
    "transaction"]

-- | Sales Order > Shipping Group
nsSubtypeFields ("salesorder":"shipgroup":_) = [
    "destinationaddress",
    "handlingrate",
    "id",
    "shippingcarrier",
    "shippingmethod",
    "shippingrate",
    "sourceaddress",
    "weight"]

-- | serviceitem > price1
nsSubtypeFields ("serviceitem":"price1":_) = [
    "currency"
  , "discount"
  , "discountdisplay"
  , "pricelevel" ]


-- | serviceitem > price2
nsSubtypeFields ("serviceitem":"price2":_) = [
    "currency"
  , "discount"
  , "discountdisplay"
  , "pricelevel" ]


-- | serviceitem > price3
nsSubtypeFields ("serviceitem":"price3":_) = [
    "currency"
  , "discount"
  , "discountdisplay"
  , "pricelevel" ]


-- | serviceitem > price4
nsSubtypeFields ("serviceitem":"price4":_) = [
    "currency"
  , "discount"
  , "discountdisplay"
  , "pricelevel" ]


-- | serviceitem > price5
nsSubtypeFields ("serviceitem":"price5":_) = [
    "currency"
  , "discount"
  , "discountdisplay"
  , "pricelevel" ]


-- | serviceitem > sitecategory
nsSubtypeFields ("serviceitem":"sitecategory":_) = [
    "category"
  , "categorydescription"
  , "isdefault"
  , "website" ]

-- | inventorydetail > inventoryassignment
nsSubtypeFields ("inventorydetail":"inventoryassignment":_) = [
    "binnumber"
  , "expirationdate"
  , "internalid"
  , "inventorydetail"
  , "issueinventorynumber"
  , "quantity"
  , "quantityavailable" ]


-- | lead > addressbook
nsSubtypeFields ("lead":"addressbook":_) = [
    "addr1"
  , "addr2"
  , "addr3"
  , "addressee"
  , "addressid"
  , "addrtext"
  , "attention"
  , "city"
  , "country"
  , "defaultbilling"
  , "defaultshipping"
  , "displaystate"
  , "id"
  , "internalid"
  , "isresidential"
  , "label"
  , "override"
  , "phone"
  , "state"
  , "zip" ]


-- | lead > contactroles
nsSubtypeFields ("lead":"contactroles":_) = [
    "contact"
  , "email"
  , "giveaccess"
  , "passwordconfirm"
  , "role"
  , "sendemail"
  , "strength" ]


-- | lead > currency
nsSubtypeFields ("lead":"currency":_) = [
    "balance"
  , "consolbalance"
  , "consoldepositbalance"
  , "consoloverduebalance"
  , "consolunbilledorders"
  , "currency"
  , "currencyformatsample"
  , "depositbalance"
  , "displaysymbol"
  , "overduebalance"
  , "overridecurrencyformat"
  , "symbolplacement"
  , "unbilledorders" ]


-- | lead > download
nsSubtypeFields ("lead":"download":_) = [
    "expiration"
  , "file"
  , "licensecode"
  , "remainingdownloads" ]


-- | lead > grouppricing
nsSubtypeFields ("lead":"grouppricing":_) = [
    "group"
  , "level" ]


-- | lead > itempricing
nsSubtypeFields ("lead":"itempricing":_) = [
    "currency"
  , "item"
  , "level"
  , "price" ]


-- | lead > partners
nsSubtypeFields ("lead":"partners":_) = [
    "contribution"
  , "customer"
  , "id"
  , "isprimary"
  , "partner"
  , "partnerrole" ]


-- | lead > salesteam
nsSubtypeFields ("lead":"salesteam":_) = [
    "contribution"
  , "customer"
  , "employee"
  , "id"
  , "isprimary"
  , "issalesrep"
  , "salesrole" ]


-- | message > mediaitem
nsSubtypeFields ("message":"mediaitem":_) = [
    "mediaitem" ]


-- | opportunity > competitors
nsSubtypeFields ("opportunity":"competitors":_) = [
    "competitor"
  , "id"
  , "notes"
  , "url"
  , "winner" ]


-- | opportunity > item
nsSubtypeFields ("opportunity":"item":_) = [
    "altsalesamt"
  , "amount"
  , "billingschedule"
  , "billvariancestatus"
  , "costestimate"
  , "costestimaterate"
  , "costestimatetype"
  , "daysbeforeexpiration"
  , "deferrevrec"
  , "description"
  , "expectedshipdate"
  , "fromjob"
  , "id"
  , "isestimate"
  , "istaxable"
  , "isvsoebundle"
  , "item"
  , "itemsubtype"
  , "itemtype"
  , "job"
  , "line"
  , "linenumber"
  , "matrixtype"
  , "options"
  , "price"
  , "printitems"
  , "quantity"
  , "rate"
  , "rateschedule"
  , "units" ]


-- | opportunity > partners
nsSubtypeFields ("opportunity":"partners":_) = [
    "contribution"
  , "id"
  , "isprimary"
  , "partner"
  , "partnerrole"
  , "transaction" ]


-- | opportunity > salesteam
nsSubtypeFields ("opportunity":"salesteam":_) = [
    "contribution"
  , "employee"
  , "id"
  , "isprimary"
  , "issalesrep"
  , "salesrole"
  , "transaction" ]

-- * INSERT NEW SUBTYPE FIELDS AFTER THIS POINT *

-- * INSERT NEW SUBTYPE FIELDS BEFORE THIS POINT *

-- | Catch-all
nsSubtypeFields _ = []
