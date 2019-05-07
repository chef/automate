import { Component, OnInit } from '@angular/core';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Feature } from 'app/services/feature-flags/types';

import { GetIamVersion } from 'app/entities/policies/policy.actions';
import { notificationState } from 'app/entities/notifications/notification.selectors';
import { routeURL } from './route.selectors';
import { Notification } from 'app/entities/notifications/notification.model';
import { DeleteNotification } from 'app/entities/notifications/notification.actions';
import { LicenseApplyReason } from 'app/page-components/license-apply/license-apply.component';

@Component({
  selector: 'app-ui',
  templateUrl: './ui.component.html',
  styleUrls: ['./ui.component.scss']
})
export class UIComponent implements OnInit {
  // Feature Flags
  // TODO:eng-ex This static data seems out of place. Should it go in InitialState?
  experimentalFeatures: Array<Feature> = [];

  notifications$: Observable<Notification[]>;

  betaFeatures: Array<Feature> = [
    {
      key: 'applications',
      name: 'EAS Applications'
    },
    {
      key: 'servicenow_cmdb',
      name: 'ServiceNow CMDB Integration'
    }
  ];

  legacyFeatures: Array<Feature> = [];

  licenseApplyReason: LicenseApplyReason;

  // TODO(sr) 2018/12/03: This is specific to the policies page and should be
  // handled differently. Unfortunately, I don't know how; but this isn't the
  // correct way ;)
  renderNavbar = window.location.pathname.split('/').pop() !== 'add-members';

  constructor(
    private store: Store<NgrxStateAtom>
  ) {
    this.notifications$ = store.select(notificationState);
  }

  ngOnInit(): void {
    this.store.select(routeURL).subscribe((url: string) => {
      this.renderNavbar = url.split('/').pop() !== 'add-members';
    });
    this.store.dispatch(new GetIamVersion());
  }

  onTriggerApplyLicense(reason: LicenseApplyReason): void {
    this.licenseApplyReason = reason;
  }

  onTriggerBannerLicenseApply(): void {
    this.licenseApplyReason = LicenseApplyReason.LICENSE_ABOUT_TO_EXPIRE;
  }

  handleNotificationDismissal(id: string): void {
    this.store.dispatch(new DeleteNotification({ id }));
  }
}
