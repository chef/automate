# Background and Context

In the [Licensing](https://github.com/chef/automate/tree/master/docs#licensing) portion of the domains definition we do a good job of explaining what each piece is, but it feels useful to talk about how we get here from there.

## Understanding How We Go From Sale to License to Features

In Salesforce terms, Chef sells subscriptions. Subscriptions are described by a product, a quantity of that product, and the date range during which it is active. Subscriptions and products have a one-to-one relationship in the system but are not interchangeable concepts; they are distinct entities.

Subscriptions purchased by a customer are represented within a license as "entitlements". However, we currently we use only one entitlement, "base", representing the entire base platform, as a placeholder until entitlement use can be better defined and implemented.

A license includes purchaser information along with the list of non-expired entitlements (current at the time the license is generated). We encode all of that information as a JWT (JavaScript Web Token) for portability and signature validation. A license token is fed to the license-control-service which constructs a Policy of application behavior based on the type of license, entitlements and their active date range versus current date, and so on. The policy is returned to requesting services so that they may alter behavior as appropriate, such as showing banners when a subscription is expiring, or disabling features when there are no longer active entitlements for them.

## Understanding How We Arrived at the Shape

As we looked at what we were trying to accomplish, the goal was to build a license that did a few things:

1. It solved for the problems of the past...
  * Because we don't send the license name across the wire, it was not always easy to track it back to a given customer
  * There is no notion of an expiration date, term, etc.
  * There is no way to give the license more data arbitrarily.
  * Identifying Chef usage is near impossible if you lose track of the guids that are used by Chef
  * Partner licenses are no different than commercial licenses
  * The email we send for Proof of Electronic Delivery (POED) occasionally didn't make it through the spam filters
2. It decoupled what we sold from what the software was able to do with that data.
3. We were able to easily experiment with new products.
4. It better mapped to the way the Sales reps think about a license in terms of when a new one is required.
5. It allowed us to provide more data to the system about what it is supposed to do.
6. It made it possible to rotate signing keys over time.

For better or for worse, that is a lot of stuff. In the end, while the license feels over engineered at times, each decision was made to support the things above.

## Decisions

### Token vs File

By moving to a token, we were able to use something that felt like a license but was actually parsable. We are using a signed JWT to encode the license. By not using a file, it makes the license more portable and easier to move around is the concept. It is important to mention that the distinction is between what was a tarball and what is now a string. There is no reason we cant end up with a file in the long run, but this gives us the flexibility to choose to do something different as we build out deployment mechanisms.

### Expiration in Entitlements

The idea here is that it is supper common for our customers to have more than one subscription that is outstanding at any given time. By adding the expiration to the entitlement itself, we are able to add all the entitlements from all the subscriptions that are active.

### Why Entitlements and Not SKUs or Products

One of the larger concepts we wanted to decouple is that a product can contain multiple entitlements. Think about the ability to sell a standard version vs an enterprise version, where the difference is wether or not someone was entitled to SAML. This separation gives us that power.
