import { Component, ChangeDetectionStrategy } from '@angular/core';
import { BaseIntegrationFormComponent } from '../base-integration-form.component';

@Component({
  selector: 'app-azure-integration-form',
  templateUrl: './azure-form.component.html',
  styleUrls: ['./azure-form.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class IntegrationsAzureFormComponent extends BaseIntegrationFormComponent {
}
