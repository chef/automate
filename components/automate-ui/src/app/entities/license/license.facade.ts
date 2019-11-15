import { Injectable } from '@angular/core';
import { Action, Store } from '@ngrx/store';
import { Observable, BehaviorSubject } from 'rxjs';

import * as fromServiceGroups from './license.reducer';

import {
    FetchStatus,
    ApplyStatus,
    RequestStatus,
    TriggerWelcomeStatus
  } from './license.model';

import {
  fetchLicense,
  applyLicense,
  requestLicense,
  triggerWelcome
} from './license.selectors';

import {
    ApplyLicense,
    GetLicenseStatus,
    RequestLicense,
    TriggerWelcome
  } from './license.actions';

import { notificationState } from 'app/entities/notifications/notification.selectors';
import { Notification } from 'app/entities/notifications/notification.model';
import { DeleteNotification } from 'app/entities/notifications/notification.actions';

export enum LicenseApplyReason {
    INITIAL_INSTALL = 1,
    LICENSE_EXPIRED = 2,
    LICENSE_ABOUT_TO_EXPIRE = 3
}

@Injectable({
  providedIn: 'root'
})
export class LicenseFacadeService {
    notifications$: Observable<Notification[]>;
    fetchLicense$: Observable<FetchStatus>;
    applyLicense$: Observable<ApplyStatus>;
    requestLicense$: Observable<RequestStatus>;
    triggerWelcome$: Observable<TriggerWelcomeStatus>;
    licenseApplyReason$: BehaviorSubject<LicenseApplyReason>;

    constructor(
        private store: Store<fromServiceGroups.LicenseStatusEntityState>
    ) {
        this.notifications$ = store.select(notificationState);
        this.fetchLicense$ = store.select(fetchLicense);
        this.applyLicense$ = store.select(applyLicense);
        this.requestLicense$ = store.select(requestLicense);
        this.triggerWelcome$ = store.select(triggerWelcome);
        this.licenseApplyReason$ = new BehaviorSubject(LicenseApplyReason.INITIAL_INSTALL);
    }

    dispatch(action: Action) {
        this.store.dispatch(action);
    }

    getLicenseStatus() {
        this.store.dispatch(new GetLicenseStatus());
    }

    applyLicense(license) {
        this.store.dispatch(new ApplyLicense(license));
    }

    requestLicense(licenseRequest) {
        this.store.dispatch(new RequestLicense(licenseRequest));
    }

    triggerWelcome() {
        this.store.dispatch(new TriggerWelcome());
    }

    handleNotificationDismissal(id: string): void {
        this.store.dispatch(new DeleteNotification({ id }));
    }

    updateLicenseApplyReason(reason?: LicenseApplyReason): void {
        this.licenseApplyReason$.next(reason || LicenseApplyReason.INITIAL_INSTALL);
    }
}
