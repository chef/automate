import { Component } from '@angular/core';
import { ProductDeployedService } from 'app/services/product-deployed/product-deployed.service';

@Component({
  selector: 'app-navbar',
  templateUrl: './navbar.component.html',
  styleUrls: ['./navbar.component.scss']
})

export class NavbarComponent {

  public isDesktopView = false;

  constructor(private productDeployedService: ProductDeployedService) {
    this.isDesktopView = this.productDeployedService.isProductDeployed('desktop');
  }
}
