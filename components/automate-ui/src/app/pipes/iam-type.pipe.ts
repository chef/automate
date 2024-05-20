import { Pipe, PipeTransform } from '@angular/core';

@Pipe({
  name: 'iamType'
})
export class IamTypePipe implements PipeTransform {

  transform(value: string | undefined): 'Chef-managed' | 'Custom' | null {
    switch (value) {
      case 'CHEF_MANAGED':
        return 'Chef-managed';
      case 'CUSTOM':
        return 'Custom';
    }
    return null
  }
}
