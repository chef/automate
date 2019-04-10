import { Component, Input, ChangeDetectionStrategy } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { get } from 'lodash/fp';

@Component({
  selector: 'app-gcp-integration-form',
  templateUrl: './gcp-form.component.html',
  styleUrls: ['./gcp-form.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class IntegrationsGCPFormComponent {
  @Input() formGroup: FormGroup;

  showInstanceCreds() {
    const formData = this.formGroup.value;
    const no_creds = get('no_creds', formData);
    return !no_creds;
  }
}
