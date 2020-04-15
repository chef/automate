import { Injectable } from '@angular/core';
import { isEmpty, xor } from 'lodash';

@Injectable({
  providedIn: 'root'
})
export class ValidatorService {
  validateCustomOrder<T, K>(node: K, customColumnOrder: Array<keyof T> & string[]):
  { valid: boolean, xor: string[] } {
    const xorN = xor(Object.keys(node), customColumnOrder);
    return {
      valid: isEmpty(xorN),
      xor: xorN
    };
  }
}
