// This file is required by karma.conf.js and loads recursively all the .spec and framework files

import 'zone.js/testing';
import { getTestBed } from '@angular/core/testing';
import {
  BrowserDynamicTestingModule,
  platformBrowserDynamicTesting
} from '@angular/platform-browser-dynamic/testing';

// Unfortunately there's no typing for the `__karma__` variable. Just declare it as any.
declare const __karma__: any;
declare const require: any;

// Configure proper TestBed cleanup to prevent EmptyError in afterAll hooks
import { TestBed } from '@angular/core/testing';

// Store original afterEach
const originalAfterEach = afterEach;
if (typeof afterEach !== 'undefined') {
  (window as any).afterEach = function(fn: any, timeout?: number) {
    return originalAfterEach.call(this, function() {
      // Always reset TestBed after each test to prevent service cleanup issues
      try {
        TestBed.resetTestingModule();
      } catch (e) {
        // Ignore any errors during TestBed reset
      }

      // Execute the original afterEach function if provided
      if (fn) {
        return fn();
      }
    }, timeout);
  };
}

// Guard needed for wallaby.js
// (reference: https://github.com/wallabyjs/public/issues/2433#issuecomment-637972491)
if ((<any>window).__karma__) {
  // Prevent Karma from running prematurely.
  __karma__.loaded = function () { };
}



// First, initialize the Angular testing environment.
getTestBed().initTestEnvironment(
  BrowserDynamicTestingModule,
  platformBrowserDynamicTesting()
);
// Then we find all the tests.
const context = require.context('./', true, /\.spec\.ts$/);
// And load the modules.
context.keys().map(context);

// Guard needed for wallaby.js
if ((<any>window).__karma__) {
  // Finally, start Karma to run the tests.
  __karma__.start();
}
