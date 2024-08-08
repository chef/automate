import { Injectable } from '@angular/core';
import { isProductDeployed as staticIsProductDeployed } from '../../staticConfig';

@Injectable()
export class ProductDeployedService {

  constructor() {}

  isProductDeployed(product: string): boolean {
    return staticIsProductDeployed( product );
  }
}
