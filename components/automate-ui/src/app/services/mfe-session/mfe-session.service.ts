import { ActivatedRouteSnapshot, CanActivate, RouterStateSnapshot } from '@angular/router';

export class MfeSessionService implements CanActivate {
  canActivate(_route: ActivatedRouteSnapshot, state: RouterStateSnapshot): boolean {
    let map_url = localStorage.getItem('mfe_url_mapping');
    let flag = true;

    map_url = JSON.parse(map_url);

    if (map_url && map_url[state.url]) {
      window.location.href = map_url[state.url];
      flag = false;
    }

    return flag;
  }
}
