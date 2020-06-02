import { Component, ChangeDetectionStrategy } from '@angular/core';
import { BaseIntegrationFormComponent } from '../base-integration-form.component';

@Component({
  selector: 'app-gcp-integration-form',
  templateUrl: './gcp-form.component.html',
  styleUrls: ['./gcp-form.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class IntegrationsGCPFormComponent extends BaseIntegrationFormComponent {
}
