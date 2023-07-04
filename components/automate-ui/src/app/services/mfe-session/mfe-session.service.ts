import { ActivatedRouteSnapshot, CanActivate, RouterStateSnapshot } from '@angular/router';

export class MfeSessionService implements CanActivate {
  canActivate(_route: ActivatedRouteSnapshot, state: RouterStateSnapshot): boolean {
    let urls : any = localStorage.getItem('mfe_urls');
    if(urls){
      urls = JSON.parse(urls);
      for (let index = 0; index < urls.length; index++) {
        const element = urls[index];
        if(element === state.url) {
          return false;
        }
      }
      return true;
    }
    return false;
  }
}
