import { Component, OnInit } from '@angular/core';
import { ActivationStart, ActivationEnd, Router, NavigationEnd } from '@angular/router';
import { Store } from '@ngrx/store';
import { Observable } from 'rxjs';
import { filter } from 'rxjs/operators';

import { NgrxStateAtom } from 'app/ngrx.reducers';
import { Feature } from 'app/services/feature-flags/types';

import { GetIamVersion } from 'app/entities/policies/policy.actions';
import { notificationState } from 'app/entities/notifications/notification.selectors';
import { Notification } from 'app/entities/notifications/notification.model';
import { DeleteNotification } from 'app/entities/notifications/notification.actions';
import { LicenseApplyReason } from 'app/page-components/license-apply/license-apply.component';
import { GetAllUserPerms } from './entities/userperms/userperms.actions';

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

  renderNavbar = true;

  constructor(
    private store: Store<NgrxStateAtom>,
    private router: Router
  ) {
    this.notifications$ = store.select(notificationState);

    // ActivationEnd specifically needs to be here in the constructor to catch early events.
    this.router.events.pipe(
      filter(event => event instanceof ActivationEnd)
    ).subscribe((event: ActivationEnd) => {
      this.renderNavbar = typeof event.snapshot.data.hideNavBar !== 'undefined'
        ? !event.snapshot.data.hideNavBar
        : this.renderNavbar;
    });
  }

  ngOnInit(): void {
    this.router.events.pipe(
        filter(event => event instanceof ActivationStart)
    ).subscribe((event: ActivationStart) => this.renderNavbar = !event.snapshot.data.hideNavBar);

    this.router.events.pipe(
        filter(event => event instanceof NavigationEnd)
    ).subscribe(() => this.store.dispatch(new GetAllUserPerms()));

    // Initial call
    this.store.dispatch(new GetAllUserPerms());

    // Initial calls for polled events
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
