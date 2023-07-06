import { ActivatedRouteSnapshot, CanActivate, RouterStateSnapshot } from '@angular/router';

export class MfeSessionService implements CanActivate {
  canActivate(_route: ActivatedRouteSnapshot, state: RouterStateSnapshot): boolean {
    let urls : any = localStorage.getItem('mfe_urls');
    let map_url = localStorage.getItem('url_mapping');

    if(urls && map_url){
      urls = JSON.parse(urls);
      urls.forEach(url => {
        const element = url;
        if(element === state.url) {
          map_url = JSON.parse(map_url);
          window.location.href = map_url[element];
          return false;
        }
      });
      return true;
    }
    return false;
  }
}
