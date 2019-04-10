import { Component, Input, ChangeDetectionStrategy } from '@angular/core';
import { FormGroup } from '@angular/forms';
import { get } from 'lodash/fp';

@Component({
  selector: 'app-aws-integration-form',
  templateUrl: './aws-form.component.html',
  styleUrls: ['./aws-form.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class IntegrationsAWSFormComponent {
  @Input() formGroup: FormGroup;

  showInstanceCreds() {
    const formData = this.formGroup.value;
    const no_creds = get('no_creds', formData);
    return !no_creds;
  }
}
