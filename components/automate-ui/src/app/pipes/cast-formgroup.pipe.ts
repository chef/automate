import { Pipe, PipeTransform } from '@angular/core';
import { FormGroup } from '@angular/forms';

@Pipe({
  name: 'castFormGroup',
  pure: true
})
export class CastFormGroupPipe implements PipeTransform {

  constructor() {
  }

  transform(value: any, _args?: any): FormGroup {
    return value;
  }
}
