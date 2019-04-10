Notes
====
These are general notes and thoughts about the technical direction of delivery-web

Browser support
---------------
We should in general support the most current version of all major web browsers and one version back. Major browsers include: Firefox, Internet Explorer, Chrome and Safari.

Authentication
--------------
Some sort of token based auth would be ideal as it relaxes the need for a proxy. Ideally we can authenticate to the delivery api and then make request directly to it.

Other Notes
-----------
There has been talk about streaming data directly to the user. This implies that we use something like websockets or server sent events. It shouldn't be a problem as long as we don't need to support older versions of Internet Explorer.

Technology
----------
The app will be built using Angular as the primary framework for javascript and probably something like compass and sass for our css framework.
