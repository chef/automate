import { Component, isDevMode } from '@angular/core';
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
}
