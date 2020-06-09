import { Component, Input, ChangeDetectionStrategy } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { get } from 'lodash/fp';

@Component({
  selector: 'app-azure-integration-form',
  templateUrl: './azure-form.component.html',
  styleUrls: ['./azure-form.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class IntegrationsAzureFormComponent {
  @Input() formGroup: FormGroup;

  showInstanceCreds() {
    const formData = this.formGroup.value;
    const no_creds = get('no_creds', formData);
    return !no_creds;
  }
}
