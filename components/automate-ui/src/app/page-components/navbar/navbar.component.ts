import { Component } from '@angular/core';
import { ProductDeployedService } from '../../services/product-deployed/product-deployed.service';

@Component({
  standalone: false,
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
