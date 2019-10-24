import { Component, EventEmitter, Output } from '@angular/core';
import { Observable } from 'rxjs';

import { LicenseFacadeService, LicenseApplyReason } from 'app/entities/license/license.facade';
import { Notification } from 'app/entities/notifications/notification.model';

@Component({
  selector: 'app-license-notifications',
  templateUrl: './license-notifications.component.html',
  styleUrls: ['./license-notifications.component.scss']
})
export class LicenseNotificationsComponent {
  @Output() triggerApplyLicense = new EventEmitter<LicenseApplyReason>();

  notifications$: Observable<Notification[]>;

  constructor(
    private licenseFacade: LicenseFacadeService
  ) {
    this.notifications$ = this.licenseFacade.notifications$;
  }

  handleNotificationDismissal(id: string): void {
    this.licenseFacade.handleNotificationDismissal(id);
  }

  onTriggerBannerLicenseApply(): void {
    this.licenseFacade.updateLicenseApplyReason(LicenseApplyReason.LICENSE_ABOUT_TO_EXPIRE);
    this.triggerApplyLicense.emit();
  }
}
