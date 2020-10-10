import { Component, EventEmitter, Output, OnDestroy } from '@angular/core';
import { distinctUntilChanged, takeUntil } from 'rxjs/operators';
import { Subject } from 'rxjs';

import { LayoutFacadeService } from 'app/entities/layout/layout.facade';
import { LicenseFacadeService, LicenseApplyReason } from 'app/entities/license/license.facade';
import { Notification } from 'app/entities/notifications/notification.model';

@Component({
  selector: 'app-license-notifications',
  templateUrl: './license-notifications.component.html',
  styleUrls: ['./license-notifications.component.scss']
})
export class LicenseNotificationsComponent implements OnDestroy {
  @Output() triggerApplyLicense = new EventEmitter<LicenseApplyReason>();
  private isDestroyed: Subject<boolean> = new Subject<boolean>();

  notifications: Notification[];

  constructor(
    private licenseFacade: LicenseFacadeService,
    private layoutFacade: LayoutFacadeService
  ) {
    this.licenseFacade.notifications$
      .pipe(
        takeUntil(this.isDestroyed),
        distinctUntilChanged()
      ).subscribe(notifications => {
      this.notifications = notifications.filter((n) => n.type === 'license');
      this.layoutFacade.layout.license.display = this.notifications.length > 0;
    });
  }

  ngOnDestroy() {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  handleNotificationDismissal(id: string): void {
    this.licenseFacade.handleNotificationDismissal(id);
    this.layoutFacade.layout.license.display = false;
  }

  onTriggerBannerLicenseApply(): void {
    this.licenseFacade.updateLicenseApplyReason(LicenseApplyReason.LICENSE_ABOUT_TO_EXPIRE);
    this.triggerApplyLicense.emit();
  }
}
