import { Component, ChangeDetectionStrategy } from '@angular/core';
import { BaseIntegrationFormComponent } from 'app/pages/integrations/base-integration-form.component';

@Component({
  selector: 'app-aws-integration-form',
  templateUrl: './aws-form.component.html',
  styleUrls: ['./aws-form.component.scss'],
  changeDetection: ChangeDetectionStrategy.OnPush
})
export class IntegrationsAWSFormComponent extends BaseIntegrationFormComponent {
}
