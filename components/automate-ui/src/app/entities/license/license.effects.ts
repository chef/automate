import { interval as observableInterval,  of ,  Observable } from 'rxjs';

import { withLatestFrom, mergeMap, map, catchError } from 'rxjs/operators';
import { Injectable } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { Actions, createEffect, ofType } from '@ngrx/effects';
import { Store } from '@ngrx/store';
import moment from 'moment';
import { find } from 'lodash';

import { NgrxStateAtom } from '../../ngrx.reducers';
import { CreateNotification, DeleteNotification } from '../notifications/notification.actions';
import { notificationState } from '../notifications/notification.selectors';
import { Notification, Type } from '../notifications/notification.model';
import {
  LicenseStatusActionTypes,
  ApplyLicense,
  ApplyLicenseSuccess,
  ApplyLicenseFailure,
  GetLicenseStatusSuccess,
  GetLicenseStatusSuccessExpiringSoon,
  GetLicenseStatusFailure,
  RequestLicense,
  RequestLicenseSuccess,
  RequestLicenseFailure,
  LicenseStatusAction
} from './license.actions';
import { LicenseStatusRequests } from './license.requests';
import { LicenseStatus, RequestLicenseResponse, ApplyLicenseResponse } from './license.model';

@Injectable()
export class LicenseStatusEffects {

  // Yes, not ideal, but it is valuable to keep a couple commented test lines here(!).
  // Uncomment lines marked 'TEST ONLY' to exercise the banner updating
  // (Might want to reduce the polling interval, too!)
  // testIndex = -1; // TEST ONLY
  // testData = [59, 59, 58, 57, 120, 120, 120, 60 ]; // TEST ONLY

  constructor(
    private actions$: Actions,
    private requests: LicenseStatusRequests,
    private store: Store<NgrxStateAtom>
  ) { }

  // Number of days that remain on license after which
  // we start displaying the license warning header.
  private LICENSE_WARNING_PERIOD = 90;

  private POLLING_INTERVAL_IN_SECONDS = 300; // 5 minutes

  newLicenseStatus$ = createEffect(() =>
    observableInterval(1000 * this.POLLING_INTERVAL_IN_SECONDS).pipe(
    mergeMap(() => this.getResponseAction$())));

  fetchLicenseStatus$ = createEffect(() =>
    this.actions$.pipe(
      ofType(LicenseStatusActionTypes.GET),
      mergeMap(() => this.getResponseAction$())));

  fetchLicenseSuccess$ = createEffect(() =>
    this.actions$.pipe(
    ofType(LicenseStatusActionTypes.GET_SUCCESS_EXPIRING_SOON),
    map((action: GetLicenseStatusSuccessExpiringSoon) => {
      return new CreateNotification({
        type: Type.license,
        message: action.payload.expiry_message,
        timeout: 0
      });
    })));

  applyLicense$ = createEffect(() =>
    this.actions$.pipe(
    ofType(LicenseStatusActionTypes.APPLY),
    mergeMap((action: ApplyLicense) => {
      return this.requests.applyLicense(action.payload).pipe(
        map((resp: ApplyLicenseResponse) => new ApplyLicenseSuccess(resp)),
        catchError((error) => of(new ApplyLicenseFailure(error))));
    })));

  requestTrialLicense$ = createEffect(() =>
    this.actions$.pipe(
    ofType(LicenseStatusActionTypes.REQUEST),
    mergeMap((action: RequestLicense) =>
      this.requests.requestLicense(action.payload).pipe(
        map((resp: RequestLicenseResponse) => new RequestLicenseSuccess(resp)),
        catchError((error) => of(new RequestLicenseFailure(error))))
    )));

    private getResponseAction$(): Observable<LicenseStatusAction> {
      return this.requests.fetchLicenseStatus().pipe(
        withLatestFrom(this.store.select(notificationState)),
        map(([licenseStatus, notifications]) =>
          this.getActionAndNotify(licenseStatus, notifications)
        ),
        catchError((error: HttpErrorResponse) => of(new GetLicenseStatusFailure(error))));
    }

  private getActionAndNotify(
    licenseStatus: LicenseStatus,
    notifications: Notification[]
  ): LicenseStatusAction {
    const oldNote = find(notifications, ['type', Type.license]);
    const oldMessage = oldNote ? oldNote.message : '';
    const newMessage = this.expiringSoonMessage(licenseStatus);
    if (oldNote && oldMessage !== newMessage) {
      this.store.dispatch(new DeleteNotification({ id: oldNote.id }));
    }
    if (newMessage && (oldMessage !== newMessage)) {
      return new GetLicenseStatusSuccessExpiringSoon(
        { license: licenseStatus, expiry_message: newMessage });
    }
    return new GetLicenseStatusSuccess(licenseStatus);
  }

  // expiringSoonMessage returns the proper message string
  // or '' if the license is not yet in expiry warning period.
  private expiringSoonMessage(licenseStatus): string {

    const daysRemaining = moment(licenseStatus && licenseStatus.licensed_period.end).diff(moment(), 'days');
    // this.testIndex = (this.testIndex + 1) % this.testData.length; // TEST ONLY
    // const daysRemaining = this.testData[this.testIndex]; // TEST ONLY

    // Do nothing if license is already expired. Modal will pop up in that case.
    if (daysRemaining > this.LICENSE_WARNING_PERIOD || (daysRemaining < 0 && licenseStatus?.grace_period !== true)) {
      return '';
    }
    if (licenseStatus?.grace_period === true){
      return `Your Progress® Chef® Automate™ license expired on ${moment(licenseStatus && licenseStatus.licensed_period.end).format('ddd, DD MMM YYYY')}! 
      and you are currently on a limited extension period! Contact the Account Team or email chef-account-team@progress.com for help `
    }
    if (licenseStatus?.license_type === "commercial") {
      return `Your Progress® Chef® Automate™ license is set to expire on ${moment(licenseStatus && licenseStatus.licensed_period.end).format('ddd, DD MMM YYYY')}! Contact the Account Team or email  chef-account-team@progress.com for help `
    }
    return `Your Progress® Chef® Automate™ trial license is set to expire on ${moment(licenseStatus && licenseStatus.licensed_period.end).format('ddd, DD MMM YYYY')}! Contact the Account Team for help `;
  }
}
