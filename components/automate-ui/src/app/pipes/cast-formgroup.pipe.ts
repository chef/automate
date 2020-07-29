import { Pipe, PipeTransform } from '@angular/core';
import { FormGroup } from '@angular/forms';

@Pipe({
  name: 'castFormGroup',
  pure: true
})
export class CastFormGroupPipe implements PipeTransform {

  transform(value: any, _args?: any): FormGroup {
    return value;
  }
}
