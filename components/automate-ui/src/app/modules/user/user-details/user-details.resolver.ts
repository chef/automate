import { Injectable } from '@angular/core';
import { Resolve, RouterStateSnapshot,
         ActivatedRouteSnapshot } from '@angular/router';

@Injectable()
export class UserDetailsNonAdminResolve implements Resolve<boolean> {
  resolve(_route: ActivatedRouteSnapshot, _state: RouterStateSnapshot) {
    return true;
  }
}
