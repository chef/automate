import {
  licenseStatusEntityReducer,
  LicenseStatusEntityInitialState,
  LicenseStatusEntityState
} from './license.reducer';
import {
  GetLicenseStatus,
  GetLicenseStatusSuccessExpiringSoon,
  GetLicenseStatusSuccess,
  GetLicenseStatusFailure,
  ApplyLicense,
  ApplyLicenseSuccess,
  ApplyLicenseFailure,
  RequestLicense,
  RequestLicenseSuccess,
  RequestLicenseFailure,
  TriggerWelcome
} from './license.actions';
import { EntityStatus } from '../entities';
import {
  LicenseStatus,
  ApplyLicensePayload,
  RequestLicensePayload,
  ApplyLicenseResponse,
  RequestLicenseResponse
} from './license.model';
import { HttpErrorResponse } from '@angular/common/http';

describe('licenseStatusEntityReducer', () => {
  const initialState: LicenseStatusEntityState = LicenseStatusEntityInitialState;
  // note: the error code doesn't matter, but it has to be passed to the
  // constructor
  const httpErrorResponse = new HttpErrorResponse({status: 400});
  const licenseStatus: LicenseStatus = {
    license_id: 'a953c5bb-82a5-41be-b7dd-a5de1ea53ada',
    configured_at: '2018-04-26T21:14:34.000Z',
    licensed_period: {
      start: '2018-02-14T00:00:00.000Z',
      end: '2020-02-14T23:59:59.000Z'
    },
    customer_name: 'Chef Dev'
  };

  describe('LicenseStatus action types', () => {
    describe('GET', () => {
      const action = new GetLicenseStatus();

      it('sets status to loading', () => {
        const { fetch: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.status).toEqual(EntityStatus.loading);
      });

      it('leaves expiry_message intact', () => {
        const previousState = {
          ...initialState,
          fetch: { ...initialState.fetch, expiryMessage: 'something something expiry' }
        };
        const { fetch: newState } = licenseStatusEntityReducer(previousState, action);
        expect(newState.expiryMessage).toEqual('something something expiry');
      });

      it('leaves errorResp intact', () => {
        const previousState = {
          ...initialState,
          fetch: { ...initialState.fetch, errorResp: httpErrorResponse }
        };
        const { fetch: newState } = licenseStatusEntityReducer(previousState, action);
        expect(newState.errorResp).toEqual(httpErrorResponse);
      });
    });

    describe('GET_SUCCESS_EXPIRING_SOON', () => {
      const payload = { license: licenseStatus, expiry_message: '4 days remaining' };
      const action = new GetLicenseStatusSuccessExpiringSoon(payload);

      it('sets status to loadingSuccess', () => {
        const { fetch: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.status).toEqual(EntityStatus.loadingSuccess);
      });

      it('resets errorResp', () => {
        const previousState = {
          ...initialState,
          fetch: { ...initialState.fetch, errorResp: httpErrorResponse }
        };
        const { fetch: newState } = licenseStatusEntityReducer(previousState, action);
        expect(newState.errorResp).toBeNull();
      });

      it('carries over expiry_message', () => {
        const { fetch: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.expiryMessage).toEqual('4 days remaining');
      });

      it('carries over the license status', () => {
        const { fetch: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.license).toEqual(licenseStatus);
      });
    });

    describe('GET_SUCCESS', () => {
      const payload = licenseStatus;
      const action = new GetLicenseStatusSuccess(payload);

      it('sets status to loadingSuccess', () => {
        const { fetch: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.status).toEqual(EntityStatus.loadingSuccess);
      });

      it('resets errorResp', () => {
        const previousState = {
          ...initialState,
          fetch: { ...initialState.fetch, errorResp: httpErrorResponse }
        };
        const { fetch: newState } = licenseStatusEntityReducer(previousState, action);
        expect(newState.errorResp).toBeNull();
      });

      it('resets expiry_message', () => {
        const previousState = {
          ...initialState,
          fetch: { ...initialState.fetch, expiryMessage: 'something something expiry' }
        };
        const { fetch: newState } = licenseStatusEntityReducer(previousState, action);
        expect(newState.expiryMessage).toEqual('');
      });

      it('carries over the license status', () => {
        const { fetch: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.license).toEqual(licenseStatus);
      });
    });

    describe('GET_FAILURE', () => {
      const payload = httpErrorResponse;
      const action = new GetLicenseStatusFailure(payload);

      it('sets status to loadingFailure', () => {
        const { fetch: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.status).toEqual(EntityStatus.loadingFailure);
      });

      it('carries over errorResp', () => {
        const { fetch: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.errorResp).toEqual(payload);
      });

      it('leaves expiry_message intact', () => {
        const previousState = {
          ...initialState,
          fetch: { ...initialState.fetch, expiryMessage: 'something something expiry' }
        };
        const { fetch: newState } = licenseStatusEntityReducer(previousState, action);
        expect(newState.expiryMessage).toEqual('something something expiry');
      });
    });

    describe('APPLY', () => {
      const payload: ApplyLicensePayload = { license: 'any' };
      const action = new ApplyLicense(payload);

      it('sets status to loading', () => {
        const { apply: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.status).toEqual(EntityStatus.loading);
      });

      it('leaves errorResp intact', () => {
        const previousState = {
          ...initialState,
          apply: { ...initialState.fetch, errorResp: httpErrorResponse }
        };
        const { apply: newState } = licenseStatusEntityReducer(previousState, action);
        expect(newState.errorResp).toEqual(httpErrorResponse);
      });
    });

    describe('APPLY_SUCCESS', () => {
      const payload: ApplyLicenseResponse = { status: licenseStatus };
      const action = new ApplyLicenseSuccess(payload);

      it('sets status to loadingSuccess', () => {
        const { apply: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.status).toEqual(EntityStatus.loadingSuccess);
      });

      // FIXME? Does it not matter, since this is a one-shot action?
      it('leaves errorResp intact', () => {
        const previousState = {
          ...initialState,
          apply: { ...initialState.fetch, errorResp: httpErrorResponse }
        };
        const { apply: newState } = licenseStatusEntityReducer(previousState, action);
        expect(newState.errorResp).toEqual(httpErrorResponse);
      });
    });

    describe('APPLY_FAILURE', () => {
      const payload = httpErrorResponse;
      const action = new ApplyLicenseFailure(payload);

      it('sets status to loadingFailure', () => {
        const { apply: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.status).toEqual(EntityStatus.loadingFailure);
      });

      it('sets errorResp', () => {
        const { apply: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.errorResp).toEqual(payload);
      });
    });

    describe('REQUEST', () => {
      const payload: RequestLicensePayload = {
        first_name: 'alice',
        last_name: 'schmidt',
        email: 'alice@schmidt.com',
        gdpr_agree: true
     };
      const action = new RequestLicense(payload);

      it('sets status to loading', () => {
        const { request: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.status).toEqual(EntityStatus.loading);
      });

      it('leaves errorResp intact', () => {
        const previousState = {
          ...initialState,
          request: { ...initialState.fetch, errorResp: httpErrorResponse }
        };
        const { request: newState } = licenseStatusEntityReducer(previousState, action);
        expect(newState.errorResp).toEqual(httpErrorResponse);
      });
    });

    describe('REQUEST_SUCCESS', () => {
      const payload: RequestLicenseResponse = { status: licenseStatus, license: 'any' };
      const action = new RequestLicenseSuccess(payload);

      it('sets status to loadingSuccess', () => {
        const { request: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.status).toEqual(EntityStatus.loadingSuccess);
      });

      it('leaves errorResp intact', () => {
        const previousState = {
          ...initialState,
          request: { ...initialState.fetch, errorResp: httpErrorResponse }
        };
        const { request: newState } = licenseStatusEntityReducer(previousState, action);
        expect(newState.errorResp).toEqual(httpErrorResponse);
      });
    });

    describe('REQUEST_FAILURE', () => {
      const payload = httpErrorResponse;
      const action = new RequestLicenseFailure(payload);

      it('sets status to loadingFailure', () => {
        const { request: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.status).toEqual(EntityStatus.loadingFailure);
      });

      it('sets errorResp', () => {
        const { request: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.errorResp).toEqual(payload);
      });
    });

    describe('TRIGGER_WELCOME', () => {
      const action = new TriggerWelcome();

      it('sets status to loadingSuccess', () => {
        const { triggerWelcome: newState } = licenseStatusEntityReducer(initialState, action);
        expect(newState.status).toEqual(EntityStatus.loadingSuccess);
      });
    });
  });
});
