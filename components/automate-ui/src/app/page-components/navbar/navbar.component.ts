import { Component, isDevMode } from '@angular/core';
import { isProductDeployed } from 'app/staticConfig';

@Component({
  selector: 'app-navbar',
  templateUrl: './navbar.component.html',
  styleUrls: ['./navbar.component.scss']
})

export class NavbarComponent {

  constructor() {}

  isDevMode() {
    return isDevMode();
  }

  isDesktopView(): boolean {
    return isProductDeployed('desktop');
  }
}
