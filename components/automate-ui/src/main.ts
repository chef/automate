import { enableProdMode } from '@angular/core';
import { platformBrowserDynamic } from '@angular/platform-browser-dynamic';
import { create, CyclePlugin } from 'rxjs-spy';

import { applyPolyfills, defineCustomElements } from './assets/chef-ui-library/loader';

import { AppModule } from './app/app.module';
import { environment } from './environments/environment';

if (environment.production) {
  enableProdMode();
} else {
  // This provides rxjs-spy access in the browser console,
  // which supports analyzing, debugging, and logging any tagged Observables.
  // Just execute `enableRxjsSpy()` in the console, then apply commands from
  // https://github.com/cartant/rxjs-spy#module-log
  (window as any).enableRxjsSpy = () => {

    const spy = create();

    // suppress "cycle-plugin.js:49 Cyclic next detected" warning in browser console
    // reference: https://github.com/cartant/rxjs-spy/issues/41
    spy.unplug(spy.find(CyclePlugin));

    // not strictly necessary, but let's you see what is in the object at runtime
    return spy;
  };
}

platformBrowserDynamic().bootstrapModule(AppModule, { preserveWhitespaces: true });

applyPolyfills().then(() => {
  defineCustomElements(window);
});
