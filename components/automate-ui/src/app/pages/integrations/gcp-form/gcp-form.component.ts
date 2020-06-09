import { Component, Input, ChangeDetectionStrategy } from '@angular/core';
import { FormGroup } from '@angular/forms';

@Component({
  selector: 'app-gcp-integration-form',
  templateUrl: './gcp-form.component.html',
  styleUrls: ['./gcp-form.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class IntegrationsGCPFormComponent {
  @Input() formGroup: FormGroup;
}
