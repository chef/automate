import { TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';
import * as moment from 'moment';
import { of as observableOf } from 'rxjs';

import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { SessionStorageService } from 'app/services/storage/sessionstorage.service';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { MockChefSessionService } from 'app/testing/mock-chef-session.service';
import { EntityStatus } from 'app/entities/entities';
import { HttpStatus } from 'app/types/types';
import { FetchStatus, RequestStatus } from 'app/entities/license/license.reducer';
import { LicenseLockoutComponent } from './license-lockout.component';
import { using } from 'app/testing/spec-helpers';
import { LicenseStatus } from 'app/entities/license/license.model';

class MockTelemetryService {
  enabled = observableOf(false);
}

class MockSessionStorageService {
  public putBoolean(): void {}
}

function genErrorResp(status: number, msg: string): any /* HttpErrorResponse */ {
  // not a full-fledged HttpErrorResponse but enough for our needs
  return {
    status,
    statusText: 'Bad Request', // Note: always "OK" when using HTTP2
    url: 'https://a2-dev.test/api/v0/license/apply',
    ok: false,
    name: 'HttpErrorResponse',
    message: 'Lorem ipsum...',
    error: { error: msg, code: 3, details: [] }
  };
}

function genLicenseResp(licenseEndDate: moment.Moment): LicenseStatus {
  return {
    license_id: '3e068eee-64be-48ed-b609-b1d46b873fcc',
    configured_at: '2018-05-19T20:49:32.000Z',
    licensed_period: {
      start: '2018-03-19T20:49:32.590686519Z',
      end: licenseEndDate.format()
    },
    customer_name: 'user <test@chef.io> - TRIAL'
  };
}

describe('LicenseLockoutComponent', () => {
  let component: LicenseLockoutComponent;

  // Tests on this action make use of the inherent state updating
  // of the underlying component.
  describe('GetLicenseStatus Action', () => {

    it('creates component', () => {
      setup(genLicenseFetchReducer(null));

      expect(component).toBeTruthy();
    });

    it('reflects current license', () => {
      const futureDate = moment().add(2, 'months');
      setup(genLicenseFetchReducer(futureDate));

      expect(component.licenseExpired).toBeFalsy();
      expect(component.fetchStatusInternalError).toBeFalsy();
      // Note using moment formatting so this unit test will still pass outside the US!
      expect(component.expirationDate).toEqual(futureDate.format('l'));
    });

    // this test is failing in wallaby with "Expression has changed after it was checked"
    xit('reflects expired license', () => {
      const pastDate = moment().subtract(2, 'months');
      setup(genLicenseFetchReducer(pastDate));

      expect(component.licenseExpired).toBeTruthy();
      expect(component.fetchStatusInternalError).toBeFalsy();
      expect(component.expirationDate).toBeUndefined();
    });

    it('reflects missing license as the sole error', () => {
      setup(genErrorFetchReducer(HttpStatus.NOT_FOUND));

      expect(component.trialLicenseApplied).toBeFalsy();
      expect(component.fetchStatusInternalError).toBeFalsy();
      expect(component.licenseExpired).toBeFalsy();
      expect(component.expirationDate).toBeUndefined();
    });

    // this test is failing in wallaby with "Expression has changed after it was checked"
    xit('reflects other error as no license plus a generic fetch error', () => {
      setup(genErrorFetchReducer(HttpStatus.FORBIDDEN));

      expect(component.trialLicenseApplied).toBeFalsy();
      expect(component.fetchStatusInternalError).toBeTruthy();
      expect(component.licenseExpired).toBeFalsy();
      expect(component.expirationDate).toBeUndefined();
    });
  });

  // Tests on this action cannot use the inherent state updating
  // of the underlying component because we have to manipulate
  // a property of the component before triggering a state change.
  // So instead, just call the handler manually.
  describe('RequestLicense Action', () => {

    it('reflects successfully requested and applied license', () => {
      const { state } = setup(genLicenseRequestReducer());
      component.requestingLicense = true;
      component.handleLicenseRequest(state.request);

      expect(component.trialLicenseApplied).toBeTruthy();
      expect(component.permissionDenied).toBeFalsy();
      expect(component.expirationDate).toBeUndefined();
      expect(component.trialRequestConnectivityError).toBeFalsy();
      expect(component.trialRequestInternalError).toBeFalsy();
    });

    it('reflects permission denied', () => {
      const { state } = setup(genErrorRequestReducer(HttpStatus.FORBIDDEN));
      component.requestingLicense = true;
      component.handleLicenseRequest(state.request);

      expect(component.trialLicenseApplied).toBeFalsy();
      expect(component.permissionDenied).toBeTruthy();
      expect(component.expirationDate).toBeUndefined();
      expect(component.trialRequestConnectivityError).toBeFalsy();
      expect(component.trialRequestInternalError).toBeFalsy();
    });

    it('reflects "precondition failed" error as network connectivity error', () => {
      const { state } = setup(genErrorRequestReducer(HttpStatus.PRECONDITION_FAILED));
      component.requestingLicense = true;
      component.handleLicenseRequest(state.request);

      expect(component.trialLicenseApplied).toBeFalsy();
      expect(component.permissionDenied).toBeFalsy();
      expect(component.expirationDate).toBeUndefined();
      expect(component.trialRequestConnectivityError).toBeTruthy();
      expect(component.trialRequestInternalError).toBeFalsy();
    });

    it('reflects other error as a generic request error', () => {
      // any other HTTP code not mentioned above will do
      const { state } = setup(genErrorRequestReducer(HttpStatus.NOT_FOUND));
      component.requestingLicense = true;
      component.handleLicenseRequest(state.request);

      expect(component.trialLicenseApplied).toBeFalsy();
      expect(component.permissionDenied).toBeFalsy();
      expect(component.expirationDate).toBeUndefined();
      expect(component.trialRequestInternalError).toBeTruthy();
    });

    using([
      HttpStatus.FORBIDDEN,
      HttpStatus.PRECONDITION_FAILED,
      HttpStatus.NOT_FOUND
    ], (err: number) => {
      it(`does not dispatch anything on ${err} error`, () => {
        const { state, store } = setup(genErrorRequestReducer(err));
        component.requestingLicense = true;
        component.handleLicenseRequest(state.request);
        expect(store.dispatch).not.toHaveBeenCalled();
      });
    });
  });

  function setup(reducer: () => any): { state: any, store: Store<any> } {
    TestBed.configureTestingModule({
      imports: [
        ReactiveFormsModule,
        StoreModule.forRoot({
          licenseStatus: reducer
        })
      ],
      declarations: [
        LicenseLockoutComponent,
        MockComponent({ selector: 'app-telemetry-checkbox' }),
        MockComponent({ selector: 'chef-alert' }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-checkbox', inputs: ['checked', 'disabled'] }),
        MockComponent({ selector: 'chef-error' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-modal', inputs: ['visible', 'locked'] }),
        MockComponent({ selector: 'chef-trap-focus' })
      ],
      providers: [
        { provide: ChefSessionService, useClass: MockChefSessionService },
        { provide: SessionStorageService, useClass: MockSessionStorageService },
        { provide: TelemetryService, useClass: MockTelemetryService }
      ]
    }).compileComponents();
    const fixture = TestBed.createComponent(LicenseLockoutComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    const store = TestBed.get(Store);
    spyOn(store, 'dispatch').and.callThrough();
    return { state: reducer(), store };
  }

  function genLicenseFetchReducer(licenseEndDate: moment.Moment): () => { fetch: FetchStatus } {
   return () => ({
      fetch: {
        license: genLicenseResp(licenseEndDate || moment().add(2, 'months')),
        status: EntityStatus.loadingSuccess,
        expiryMessage: '',
        errorResp: null
      }
    });
  }

  function genErrorFetchReducer(statusCode: number): () => { fetch: FetchStatus } {
    return () => ({
      fetch: {
        license: null,
        status: EntityStatus.loadingFailure,
        expiryMessage: '',
        errorResp: genErrorResp(statusCode, 'any')
      }
    });
  }

  function genLicenseRequestReducer(): () => { request: RequestStatus } {
    return () => ({
      request: {
        status: EntityStatus.loadingSuccess,
        errorResp: null
      }
    });
  }

  function genErrorRequestReducer(statusCode: number): () => { request: RequestStatus } {
    return () => ({
      request: {
        license: null,
        status: EntityStatus.loadingFailure,
        errorResp: genErrorResp(statusCode, 'any')
      }
    });
  }
});
