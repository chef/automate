import { NgZone } from '@angular/core';
import { Router } from '@angular/router';
import { isFunction } from 'lodash/fp';

/**
 * Wrapper of Angular router (only for testing purpose)
 * Meant to run all router operations within Angular zone
 *  * Keep change detection enabled
 *  * Avoids flooded console with warnings
 *    https://github.com/angular/angular/issues/25837
 * This function comes specifically from
 *    https://github.com/angular/angular/issues/25837#issuecomment-445796236
 *
 * @see Router
 */
export function wrapRouterInNgZone(router: Router, ngZone: NgZone): Router {
  return new Proxy(router, {
    get(target: Router, p: PropertyKey): unknown {
      const invokedProperty = target[p];
      if (!isFunction(invokedProperty)) {
        return invokedProperty;
      }

      return function (...args: Array<unknown>): unknown {
        return ngZone.run(() => invokedProperty.apply(target, args));
      };
    }
  });
}
