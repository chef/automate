import { Input, Component } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { get } from 'lodash/fp';

@Component({
  template: ''
})
export class BaseIntegrationFormComponent {
  @Input() formGroup: FormGroup;

  credentialsFormGroup(): FormGroup {
    return this.formGroup.controls.credentials as FormGroup;
  }

  showInstanceCreds() {
    const formData = this.formGroup.value;
    const no_creds = get('no_creds', formData);
    return !no_creds;
  }
}
