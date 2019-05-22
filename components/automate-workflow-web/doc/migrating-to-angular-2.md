# Migrating to Angular 2

The browser-based interface of Chef Automate today consists of two separate JavaScript applications: Workflow UI, which is written in [Angular 1](https://docs.angularjs.org), and Visibility UI, which is written in [Angular 2](https://angular.io/). For a number of reasons, this separateness is a bummer, so work was recently completed to allow you to write Workflow UI code in Angular 2 and [TypeScript](https://www.typescriptlang.org/), which you should definitely start doing today. **In general, if you're writing new code for Workflow UI, you should be writing it in Angular 2.** The eventual goal is to replace all Angular 1 code with Angular 2 and merge these two applications into a single, glorious bundle of excellence that makes us all happy and gives our customers a faster, shinier and more pleasing experience.

This guide aims to help you with the transition.

## How Angular Migration Works in Workflow UI

Some thoughtful people on the Angular team created a thing called the [UpgradeAdapter](https://angular.io/docs/ts/latest/api/upgrade/index/UpgradeAdapter-class.html) that allows the Angular 1 and 2 frameworks to run side-by-side in a single application and share ownership of the DOM. When you hit a URL that's handled by Workflow UI, `index.html` loads the [Webpack](https://webpack.github.io/)-built `main.bundle.js`, whose [entry point](https://github.com/chef/delivery/blob/master/web/config/webpack.common.js#L33), [app.js](https://github.com/chef/delivery/blob/master/web/src/app.js), imports (in this order):

  * the Angular 1 framework,
  * Workflow UI's Angular 1 code (including all routing definitions, services, directives, etc.),
  * the Angular 2 framework,
  * the Angular 2 UpgradeAdapter,
  * Workflow UI's Angular 2 code

... and then boostraps Workflow UI using the UpgradeAdapter, passing it the name of our Angular 1 application (`cd`) and a selector (`html`) representing its root in the DOM tree. At this point, we're running a "hybrid" Angular app, and we can write our code in either Angular 1 or 2. (The Workflow UI app is essentially running on Angular 1, with portions of its DOM owned by Angular 2.)

That's a very brief summary of how our hybrid app is compiled and started. Now we'll move on to some examples, beginning with a simple component that we'll develop progressively to handle various situations. We're also going to start referring to the Angular frameworks as Ng1 and Ng2.

## On to the Examples

The code for these examples is contained in a single branch ([`cdn/ng2-migration-guide`](https://github.com/chef/delivery/commits/cdn/ng2-migration-guide)), with each example in its own commit so you can follow the progression a bit at a time.

  1. [Add an Ng2 component to an existing view](#add-a-simple-ng2-component-to-an-existing-view)
    ([diff](https://github.com/chef/delivery/commit/68b47937448971425be8925fd55fd3366f0247ee))
  1. [Pass Ng1 scope data to an Ng2 component](#pass-some-ng1-scope-data-to-an-ng2-component)
    ([diff](https://github.com/chef/delivery/commit/012c4ae0adac153520dfc2ff307d46acf8394fbd))
  1. [Dispatch events from an Ng2 component](#dispatch-custom-events-from-an-ng2-component-to-an-ng1-component)
    ([diff](https://github.com/chef/delivery/commit/4fdfa6e92302009f4c297dc1acefc4b040e6a824))
  1. [Add a route for an Ng2 component](#giving-an-ng2-component-its-own-route)
    ([diff](https://github.com/chef/delivery/commit/fe81c0e25a973c6236aa8cf07ed0076775f7d9b3))
  1. [Capture events dispatched by a routed Ng2 component](#capturing-events-from-a-routed-component)
    ([diff](https://github.com/chef/delivery/commit/94bf86ea4aa98f50a93e3d84980240c9a78f1a2a))
  1. [Use an Ng1 service in an Ng2 component](#using-an-ng1-service-in-an-ng2-component)
    ([diff](https://github.com/chef/delivery/commit/30a4d90f4374b07e2df7f4f3125ad13a1eb58215))

You can also [view all examples as a single diff](https://github.com/chef/delivery/compare/master...cdn/ng2-migration-guide?diff=unified&name=cdn%2Fng2-migration-guide).

### Add a Simple Ng2 Component to an Existing View

Let's start by making a new Ng2 component that displays some static data. The steps we're going to take are:

  1. Define a new Ng2 component
  1. Add it to an Ng2 module
  1. Tell the UpgradeAdapter about it
  1. Use it somewhere

In `web/src/components`, create a new folder, `subversive`, with a new file, `subversive.component.ts`, containing following TypeScript code:

```
import { Component } from '@angular/core';

@Component({
  selector: 'subversive',
  template: '<p>im in ur ng1</p>'
})
export class SubversiveComponent {

}
```

Behold, our very simple component. It's been created, but it's not yet registered with Ng2, so let's do that. In `app.module.ts`, add a reference to it in the `declarations` block of the module definition:

```
import { NgModule } from '@angular/core';
...
import { SubversiveComponent } from './components/subversive/subversive.component';

@NgModule({
  imports: [
    ...
  ],
  declarations: [
    ...
    SubversiveComponent
  ]
})
export class AppModule {}
```

Ordinarily, this would be enough, but because we're running in hybrid mode, we need to register our new component with Ng1 as well. In `web/src/components/components.js`, import the UpgradeAdapter and the component, then add a call to `ng.module().directive`, passing its name (`'subversive'`) and a wrapped ("downgraded", in UpgradeAdapter parlance) reference to it:

```
...
import ng from 'angular';
...
import { upgradeAdapter } from '../upgrade-adapter';
import { SubversiveComponent } from './subversive/subversive.component';

export default ng
  .module('cd.components', [
    ...
  ])
  .directive('subversive', upgradeAdapter.downgradeNg2Component(SubversiveComponent))
  ...
```

At this point, we can drop a `<subversive></subversive>` tag into our Ng1 app pretty much anywhere we want, and we'll see its message. We'll use navbar (`web/src/components/navbar/navbar.html`) for simplicity:

```
<header class="navbar" role="banner">
  <subversive></subversive>
  ...
```

Reload, and you should see something like this:

![](https://cl.ly/0f3X431a1p46/Image%202016-12-15%20at%202.22.09%20PM.png)

We'll finish out this example by adding some styling.

### Give the Ng2 Component some CSS

Angular 2 handles CSS [a little differently](https://angular.io/docs/ts/latest/guide/component-styles.html). Do give the docs a read, but the gist is that Angular 2 thinks about CSS in two ways:

  * As application-wide/"global" styles (those applicable to the app as a whole -- things like `p` tags, `a` tags, etc., would be styled in this way), and
  * Component styles (styles intended for individual components)

Components styles are nice, and you should use them (the docs go into why) unless what you're doing really should be applied globally. We can add some component-specific styling by creating a CSS file in our component directory and then pulling it into our component as a string. Create `web/src/components/subversive/subversive.component.scss` and give it some style:

```
p {
  color: red;
}
```

... then add a `styles` property to the component definition:

```
import { Component } from '@angular/core';

@Component({
  ...
  styles: [ require('./subversive.component.scss').toString() ]
})
...
```

Reload, and our message should now be red.

### Pass Some Ng1 Scope Data to an Ng2 Component

You'll often want to pass some data into a component. For this example, we'll extend our `SubversiveComponent` by passing it something it can use to manipulate the UI. For this, we'll have to:

  1. Define an `@Input` property on the component
  1. Define some data on an Angular 1 scope
  1. Pass that data into the component as an HTML attribute with a binding expression

First, open the component and define the property you want to expose, making sure to import the `Input` module as well. (Also note that we've moved our template markup into a file of its own at `web/src/components/subversive/subversive.component.html`.) We'll have our component accept a `clickable` attribute (a boolean) to control whether to display a button alongside the text:

```
import { Component, Input } from '@angular/core';

@Component({
  ...
  template: require('./subversive.component.html'),
})
export class SubversiveComponent {
  @Input() clickable: boolean = false;
}
```

Then, in our template, we can use the value of `clickable` (which is now a public property declared on the component class) to toggle the button's appearance in the DOM:

```
<p>
  im in ur ng1
  <button *ngIf="clickable">click me</button><!-- Note Ng2's new conditional syntax -->
</p>
```

If you reload the page now, though, you'll see the button is still missing, because `clickable`'s default value is `false`. One way we can set that value from outside the component is by passing it a static value. Back in the navbar, we can set that to `true`:

```
<header class="navbar" role="banner">
  <subversive [clickable]="true"></subversive>
  ...
```

Save, reload, and you should see a fancy new button.

We can also pass `clickable` a variable. To do that, we just have to make sure the variable containing our boolean value is in scope, then we pass that variable name in the binding expression. For example, in `navbar.js`, set a scope variable with a boolean:

```
function navbarComponent($state, $http) {
  function link(scope) {
    ...
    scope.clickability = true;
  }
```

... and then in `navbar.html`, use that variable in the binding expression:

```
<header class="navbar" role="banner">
  <subversive [clickable]="clickability"></subversive>
  ...
```

Reload, and you should still see the button.

### Dispatch Custom Events from an Ng2 Component to an Ng1 Component

Our component will probably want to know when its button is clicked, and there's a good chance its containing component will want to know about that, too. To dispatch a custom event to a containing component, we use an Ng2 `Output`.

In `subversive.component.ts`, define an `Output` to expose the event &mdash; `Outputs` are typically [EventEmitter](https://angular.io/docs/ts/latest/api/core/index/EventEmitter-class.html)s &mdash; and `emit()` the event with whatever data you'd like to provide its listeners (here, we're just passing a string to keep it simple):

```
import { Component, Input, Output, EventEmitter } from '@angular/core';

@Component({
  ...
})
export class SubversiveComponent {
  ...
  @Output() onButtonClick = new EventEmitter<string>();

  onClick() {
    this.onButtonClick.emit('eek!');
  }
}
```

```
<p>
  im in ur ng1
  <button *ngIf="clickable" (click)="onClick()">click me</button>
</p>
```

Now we need to tell the Ng1 component (which is comprised of `navbar.html` and `navbar.js`) to listen for, and handle, this event:

```
<header class="navbar" role="banner">
  <subversive
    [clickable]="clickability"
    (on-button-click)="handleClick($event)">
    <!-- ^ Note the way Angular converts `onButtonClick` to `on-button-click` here. -->
  </subversive>
```

```
function navbarComponent($state, $http) {
  function link(scope) {
    ...
    scope.clickability = true;

    scope.handleClick = (message) => {
      alert(message);
    };
  }
```

That's it. Reload, click, and you should see an alert.

### Giving an Ng2 Component its Own Route

Angular 2 introduces a new concept for routing, but because we're a hybrid app still running primarily on Angular 1, we'll need to continue routing as we do today, with [AngularUI Router](https://github.com/angular-ui/ui-router). That means, in general, routes are named and defined by URL patterns and "views", and views are (for the most part) still defined by "templates" and "controllers".

Let's give our `SubversiveComponent` its own route. We'll hit it at `/#/subvert`.

Since we've already defined the component, we can leave it as-is, but in order to navigate to it, we'll need to:

  1. Name a new Ng1 route and give it an new URL pattern
  1. Associate a fragment of HTML, representing our component and the values we want to pass into it, with the new route
  1. Create an Ng1 controller to receive the parameters supplied by the route and expose them to our component as attributes

That's a mouthful, but we've already done most of the work here: we've got a new component, we've registered it with both Ng1 and Ng2, so now all we have to do is get it into the DOM in the right place. The Ng1 controller we create will conspire with UIRouter to make this happen.

Let's start by creating the controller. Make a new file at `web/src/routes/subvert/subvert.controller.js` and drop the following code into it:

```
import ng from 'angular';

subvertController.$inject = ['$scope', 'showTheButton'];

function subvertController($scope, showTheButton) {
  $scope.isClickable = showTheButton;
}

export default ng
  .module('cd.routes.subvert.controller', [])
  .controller('subvertController', subvertController)
  .name;
```

The controller defines an `isClickable` property that we'll use to toggle the display of our button.

Now let's create the new route definition. Its job is to respond to a URL pattern by inserting some markup (e.g.,, our component expressed as an HTML fragment) into the DOM at a specific location (here, as the `main` view) and assign a controller responsible for providing its "scope" (in the Ng1 sense) and handling its events. It'll also ensure that the data we want to pass into the component is available before the component is instantiated (via the `resolve` property), which, for this example, will just be a hard-coded value.

Okay! Create a new file at `web/src/routes/subvert/subvert.js` with the following code:

```
import ng from 'angular';
import uiRouter from 'angular-ui-router';
import subvertController from './subvert.controller';

export default ng
  .module('cd.routes.subvert', [
    uiRouter,
    subvertController
  ])
  .config(($stateProvider) => {
    $stateProvider.state('main.subvert', {
      url: '/subvert',
      resolve: {
        showTheButton: () => true
      },
      views: {
        'main': {
          template: '<subversive [clickable]="isClickable"></subversive>',
          controller: 'subvertController'
        }
      }
    });
  })
  .name;
```

Finally, add the new route to the list of those imported into the application. In `web/src/routes/routes.js`:

```
import ng from 'angular';
import uiRouter from 'angular-ui-router';
...
import subvert from './subvert/subvert';

export default ng
  .module('cd.routes', [
    ...
    subvert
  ])
  ...
  .name;
```

And that's it. You should now be able to navigate to [/#/subvert](http://localhost/e/cd/#/subvert) and see the component:

![](https://cl.ly/3x2M410L1f0l/Image%202016-12-15%20at%203.53.03%20PM.png)

I've tried to select the variable names somewhat carefully to clarify what's happening here:

  1. UI router resolves a variable called `showTheButton`,
  1. which it exposes as an injectable variable to subvertController.
  1. `subvertController` creates the Ng1 `$scope` variable `isClickable` with that value
  1. and then our component, by virtue of having been inserted into the DOM by UI router (with `subvertController` supplying its scope), can receive the value of `isClickable` through the Ng2 binding expression `[clickable]="isClickable"`.

### Capturing Events from a Routed Component

If you were to click the button of our routed component now, you'd notice nothing happens, because we aren't yet handling its emitted `onButtonClick` event. To do this, we simply add an event handler to our template (the fragment we specified in the route definition) and a scope function in our controller.

In the route definition (`subvert.js`), specify the handler:

```
...
export default ng
  ...
  .config(($stateProvider) => {
    $stateProvider.state('main.subvert', {
      url: '/subvert',
      ...
      views: {
        'main': {
          template: '<subversive [clickable]="isClickable" (on-button-click)="handleClick($event)"></subversive>',
          controller: 'subvertController'
        }
      }
    });
  })
  .name;
```

And then in `subvert.controller.js`, implement it:

```
...
function subvertController($scope, showTheButton) {
  ...

  $scope.handleClick = (message) => {
    alert(message);
  };
}
```

It's essentially the same thing [we did above](#dispatch-custom-events-from-an-ng2-component-to-an-ng1-component), when we handled the event with a parent component, only here, there's no surrounding component, so the controller ends up handling the event instead.

One thing to note is that for purposes of illustration, we've routed to the same component we began with (our humble `<p>` tag), where in practice, you'd probably create a component to act as an entire view, give it a more complex layout, some component styling or global-style references, and so on. But the approach would still be the same: define a route as above, give it a controller and a template that refers to your view component, and off you go.

### Using an Ng1 Service in an Ng2 Component

Let's say we wanted to display some data supplied by an Ng1 service in our `SubversiveComponent`. Workflow UI's `CurrentUser` service gives us some access to the signed-in user; we can use that service in our Ng2 component (by "upgrading" it) to display the current username, for example. To do that, we need to:

  1. Use the UpgradeAdapter to register the service with Angular 2
  1. Inject the service into our component instance
  1. Add a binding expression to our component's template to display the username

We start by adding (in `web/src/common/auth/current_user.js`) a couple of lines to handle registering the `CurrentUser` service as an Angular 2 provider:

```
export default ng
  ...
  .service('CurrentUser', CurrentUser)
  .name;

import { upgradeAdapter } from '../../upgrade-adapter.ts';
upgradeAdapter.upgradeNg1Provider('CurrentUser');  // This name must match the Ng1 service's name
```

... and then in our component, we `@Inject` the service instance via the component's constructor (which we're also adding here; Angular calls it implicitly) and hold onto it as an instance property:

```
import { Component, Input, Output, EventEmitter, Inject } from '@angular/core';

@Component({
  ...
})
export class SubversiveComponent {
  ...
  currentUser: any;

  constructor(@Inject('CurrentUser') currentUser) {
    this.currentUser = currentUser.user();
  }

  ...
}
```

With our `currentUser` property in place, we can now display the signed-in username in our template with a binding expression:

```
<p>
  {{ currentUser.name }} is in ur ng1
  ...
</p>
```

Et voil&agrave;. Save, reload and you should see your lovely username.

![](https://cl.ly/0Q223N3G1N2u/Screenshot%202016-12-15%2016.13.59.png)

That's all the examples we have for now! If there's something you'd like to see added to this document, stop by the **#angular-migration** channel and let us know.

Thank you for helping to make Automate more awesome!

---

## Suggested Reading

### Docs, Docs, Docs!

* [The Angular 2 docs](https://angular.io/docs/ts/latest/guide/), [API reference](https://angular.io/docs/ts/latest/api/) and [Upgrade Guide](https://angular.io/docs/ts/latest/guide/upgrade.html). These docs are great &mdash; very thorough, and since Angular 2 changed so much in the run up to release (and is still changing), they're usually much more accurate than what you find out there in the wild. The time you spend here will be well spent.

* [TypeScript](https://www.typescriptlang.org/). We use a [Webpack](https://webpack.github.io/) plugin to compile your TypeScript into JavaScript automatically, so you should be able to focus on the language itself and how it deals with [modules](https://www.typescriptlang.org/docs/handbook/modules.html).

* [The RxJS docs](http://reactivex.io/rxjs/), specifically [Observables](http://reactivex.io/rxjs/class/es6/Observable.js~Observable.html), [Subjects](http://reactivex.io/rxjs/class/es6/Subject.js~Subject.html) and [BehaviorSubjects](http://reactivex.io/rxjs/class/es6/BehaviorSubject.js~BehaviorSubject.html). While not technically required for Angular 2 development, they're becoming the standard for working with asynchronous events, so you'll need to become familiar with them. [This is a great introduction](https://gist.github.com/staltz/868e7e9bc2a7b8c1f754) to the concepts and how they differ from what you might be used to.

### Books

Personally I prefer books to blogs, but Angular 2 is still so new that there isn't a whole lot of coverage in print yet. I can personally vouch for these two, though.

* [ng2-book](https://www.ng-book.com/2/)

* [Angular 2 Development with TypeScript](https://www.manning.com/books/angular-2-development-with-typescript)

## Need Help?

You can always visit the **#angular-migration** channel, but do glance through the Troubleshooting section below in case what you're seeing has already been documented.

### Troubleshooting

```
Error: Expecting ComponentFactory for: SomeComponent
```

Two known causes for this, both having to do with an Angular [Module](https://angular.io/docs/ts/latest/guide/appmodule.html) (probably `app.module.ts`) failing to locate your component somehow. Ensure the component is both listed in the `declarations` property of the module definition and that its `import` references point to the component file itself, rather than a [barrel](https://angular.io/docs/ts/latest/guide/glossary.html#!#B) (e.g,. an `index.ts`), which can sometimes cause problems:

```
import { NgModule } from '@angular/core';
...
import { MyComp } from './components/my-comp/my-comp.component';  // Do this

@NgModule({
  imports: [
    ...
  ],
  declarations: [
    ...
    MyComp  // And this
  ]
})
export class AppModule {}
```

```
import ng from 'angular';
import { upgradeAdapter } from '../../upgrade-adapter';
import { MyComp } from './my-comp/my-comp.component';  // And this

export default ng
  .module('cd.components.my-name', [])
  .directive('myName', upgradeAdapter.downgradeNg2Component(MyNameComponent))
  .name;
```

