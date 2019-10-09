import { Injectable } from '@angular/core';
import { Action, Store } from '@ngrx/store';
import { Observable } from 'rxjs';

import * as fromServiceGroups from './license.reducer';

import {
  fetchLicense,
  applyLicense,
  requestLicense,
  triggerWelcome
} from './license.selectors';

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
    fetchLicense$: Observable<any>;
    applyLicense$: Observable<any>;
    requestLicense$: Observable<any>;
    triggerWelcome$: Observable<any>;
    licenseApplyReason: LicenseApplyReason;

    constructor(
        private store: Store<fromServiceGroups.LicenseStatusEntityState>
    ) {
        this.notifications$ = store.select(notificationState);
        this.fetchLicense$ = store.select(fetchLicense);
        this.applyLicense$ = store.select(applyLicense);
        this.requestLicense$ = store.select(requestLicense);
        this.triggerWelcome$ = store.select(triggerWelcome);
    }

    dispatch(action: Action) {
        this.store.dispatch(action);
    }

    handleNotificationDismissal(id: string): void {
        this.store.dispatch(new DeleteNotification({ id }));
    }

    updateLicenseApplyReason(reason?: LicenseApplyReason): void {
        this.licenseApplyReason = reason || LicenseApplyReason.INITIAL_INSTALL;
    }
}
