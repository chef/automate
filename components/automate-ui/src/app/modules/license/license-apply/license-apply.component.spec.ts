import { TestBed } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { Store, StoreModule } from '@ngrx/store';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import moment from 'moment';
import { MockComponent } from 'ng2-mock-component';
import { MockChefButton, MockChefCheckbox, MockChefError, MockChefFormField, MockChefIcon, MockChefLoadingSpinner, MockChefModal } from 'app/testing/mock-components';
import { using } from 'app/testing/spec-helpers';
import { DateTime } from 'app/helpers/datetime/datetime';
import { HttpStatus } from 'app/types/types';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { MockChefSessionService } from 'app/testing/mock-chef-session.service';
import { EntityStatus } from 'app/entities/entities';
import { runtimeChecks } from 'app/ngrx.reducers';
import { ApplyStatus, LicenseStatus } from 'app/entities/license/license.model';
import { LicenseApplyComponent } from './license-apply.component';

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
    customer_name: 'user <test@chef.io> - TRIAL',
    grace_period: false,
    license_type: 'trial'
  };
}

describe('LicenseApplyComponent', () => {
  let component: LicenseApplyComponent;

  it('should be created', () => {
    setup(genLicenseApplyReducer());

    expect(component).toBeTruthy();
  });

  // Tests on this action cannot use the inherent state updating
  // of the underlying component because we have to manipulate
  // a property of the component before triggering a state change.
  // So instead, just call the handler manually.
  describe('ApplyLicense Action', () => {

    it('reflects successfully applied license', () => {
      const futureDate = moment().utc().add(2, 'months');
      const { state } = setup(genLicenseApplyReducer(futureDate));
      component.applyingLicense = true;
      component.handleLicenseApply(state.apply);

      expect(component.licenseApplied).toBeTruthy();
      expect(component.permissionDenied).toBeFalsy();
      expect(component.applyLicenseInternalError).toBeFalsy();
      expect(component.badRequestReason).toBe('');
      expect(component.expirationDate).toEqual(futureDate.format(DateTime.RFC2822));
    });

    it('reflects permission denied', () => {
      const { state } = setup(genErrorApplyReducer(HttpStatus.FORBIDDEN, 'any'));
      component.applyingLicense = true;
      component.handleLicenseApply(state.apply);

      expect(component.licenseApplied).toBeFalsy();
      expect(component.permissionDenied).toBeTruthy();
      expect(component.applyLicenseInternalError).toBeFalsy();
      expect(component.badRequestReason).toBe('');
      expect(component.expirationDate).toBeUndefined();
    });

    it('does not dispatch anything on FORBIDDEN error', () => {
      const { state, store } = setup(genErrorApplyReducer(HttpStatus.FORBIDDEN, 'any'));
      component.applyingLicense = true;
      component.handleLicenseApply(state.apply);
      expect(store.dispatch).not.toHaveBeenCalled();
    });

    it('reflects expired', () => {
      const msg = 'Rejecting license ID 123 expired at 456';
      const { state } = setup(genErrorApplyReducer(HttpStatus.BAD_REQUEST, msg));
      component.applyingLicense = true;
      component.handleLicenseApply(state.apply);

      expect(component.licenseApplied).toBeFalsy();
      expect(component.badRequestReason).toBe('has expired');
      expect(component.applyLicenseInternalError).toBeFalsy();
      expect(component.permissionDenied).toBeFalsy();
      expect(component.expirationDate).toBeUndefined();
    });

    it('reflects invalid license as invalid', () => {
      const msg = 'Invalid license data';
      const { state } = setup(genErrorApplyReducer(HttpStatus.BAD_REQUEST, msg));
      component.applyingLicense = true;
      component.handleLicenseApply(state.apply);

      expect(component.licenseApplied).toBeFalsy();
      expect(component.badRequestReason).toBe('is invalid');
      expect(component.applyLicenseInternalError).toBeFalsy();
      expect(component.permissionDenied).toBeFalsy();
      expect(component.expirationDate).toBeUndefined();
    });

    it('reflects other error as generic apply error', () => {
      // any other HTTP code not mentioned above will do
      const { state } = setup(genErrorApplyReducer(HttpStatus.NOT_FOUND, 'any'));
      component.applyingLicense = true;
      component.handleLicenseApply(state.apply);

      expect(component.licenseApplied).toBeFalsy();
      expect(component.applyLicenseInternalError).toBeTruthy();
      expect(component.badRequestReason).toBe('');
      expect(component.permissionDenied).toBeFalsy();
      expect(component.expirationDate).toBeUndefined();
    });

    using([
      [ HttpStatus.FORBIDDEN, 'blah'],
      [ HttpStatus.BAD_REQUEST, 'Rejecting license ID 123 expired at 456'],
      [ HttpStatus.BAD_REQUEST, 'Invalid license data'],
      [ HttpStatus.NOT_FOUND, 'any']
    ], (err: number, msg: string) => {
      it(`does not dispatch anything on ${err} error with message ${msg}`, () => {
        const { state, store } = setup(genErrorApplyReducer(err, msg));
        component.applyingLicense = true;
        component.handleLicenseApply(state.apply);
        expect(store.dispatch).not.toHaveBeenCalled();
      });
    });

    describe('GetLicenseStatus() Service', () => {
      it('check "license status" after the successfully applied license.', () => {
        const futureDate = moment().utc().add(2, 'months');
        const { state } = setup(genLicenseApplyReducer(futureDate));
        component.applyingLicense = true;
        spyOn(component.licenseFacade, 'getLicenseStatus');
        component.handleLicenseApply(state.apply);
        expect(component.licenseFacade.getLicenseStatus).toHaveBeenCalled();
      });
    });
  });

  function setup(reducer: () => any): { state: any, store: Store<any> } {
    TestBed.configureTestingModule({
      imports: [
        ReactiveFormsModule,
        StoreModule.forRoot({
          licenseStatus: reducer
        }, { runtimeChecks }),
        MockComponent({ selector: 'app-telemetry-checkbox' }),
        MockComponent({ selector: 'chef-alert' }),
        MockChefButton,
        MockChefCheckbox,
        MockChefError,
        MockChefFormField,
        MockChefIcon,
        MockChefLoadingSpinner,
        MockChefModal,
        MockComponent({ selector: 'chef-trap-focus' })
      ],
      declarations: [
        LicenseApplyComponent
      ],
      providers: [
        { provide: ChefSessionService, useClass: MockChefSessionService },
        { provide: TelemetryService, useValue: {} }
      ],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    }).compileComponents();
    const fixture = TestBed.createComponent(LicenseApplyComponent);
    component = fixture.componentInstance;
    const store = TestBed.inject(Store);
    spyOn(store, 'dispatch').and.callThrough();
    fixture.detectChanges();
    return { state: reducer(), store };
  }

  function genLicenseApplyReducer(expiry?: moment.Moment): () => { apply: ApplyStatus } {
    return () => ({
      apply: {
        status: EntityStatus.loadingSuccess,
        errorResp: null
      },
      fetch: {
        license: genLicenseResp(expiry || moment().utc().add(2, 'months')),
        status: EntityStatus.loadingSuccess,
        expiryMessage: '',
        errorResp: null
      }
    });
  }

  function genErrorApplyReducer(statusCode: number, msg: string): () => { apply: ApplyStatus } {
    return () => ({
      apply: {
        status: EntityStatus.loadingFailure,
        errorResp: genErrorResp(statusCode, msg)
      }
    });
  }
});
