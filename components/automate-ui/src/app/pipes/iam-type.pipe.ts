import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'iamType'
})
export class IamTypePipe implements PipeTransform {

  transform(value: string): 'Chef-managed' | 'Custom' {
    switch (value) {
      case 'CHEF_MANAGED':
        return 'Chef-managed';
      case 'CUSTOM':
        return 'Custom';
    }
  }
}
