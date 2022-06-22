import { TestBed } from '@angular/core/testing';
import { ChefSessionService } from './chef-session.service';
import { StoreModule } from '@ngrx/store';
import {
  ngrxReducers,
  runtimeChecks
} from 'app/ngrx.reducers';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('ChefSessionService', () => {
  let service: ChefSessionService;
  const initialState = {};

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        StoreModule.forRoot(ngrxReducers, { initialState, runtimeChecks }),
        HttpClientTestingModule
      ],
      providers: [
        ChefSessionService
      ]
    });

    service = TestBed.inject(ChefSessionService);
  });

  afterEach(() => {
    localStorage.clear();
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  describe('#tryInitializeSession', () => {
    beforeEach(() => {
      service['user'] = undefined;
      localStorage.clear();
    });

    describe('when session exists', () => {
      beforeEach(() => {
        service.setDefaultSession();
      });

      describe('and telemetry is enabled', () => {
        beforeEach(() => {
          localStorage.setItem('test_subject-telemetry-enabled', 'true');
        });

        it('pulls values from localStorage', () => {
          service.tryInitializeSession();

          expect(service.uuid).toEqual('test_subject');
          expect(service.fullname).toEqual('Test User');
          expect(service.username).toEqual('testchefuser');
          expect(service.groups).toEqual(['group1', 'group2', 'group3']);
          expect(service.id_token).toEqual('test_id_token');
          expect(service.telemetry_enabled).toEqual(true);
        });
      });

      describe('and telemetry is disabled', () => {
        beforeEach(() => {
          localStorage.setItem('test_subject-telemetry-enabled', 'false');
        });

        it('pulls values from localStorage', () => {
          service.tryInitializeSession();

          expect(service.uuid).toEqual('test_subject');
          expect(service.fullname).toEqual('Test User');
          expect(service.username).toEqual('testchefuser');
          expect(service.groups).toEqual(['group1', 'group2', 'group3']);
          expect(service.id_token).toEqual('test_id_token');
          expect(service.telemetry_enabled).toEqual(false);
        });
      });

      describe('and telemetry is not set', () => {
        it('pulls values from localStorage', () => {
          service.tryInitializeSession();

          expect(service.uuid).toEqual('test_subject');
          expect(service.fullname).toEqual('Test User');
          expect(service.username).toEqual('testchefuser');
          expect(service.groups).toEqual(['group1', 'group2', 'group3']);
          expect(service.id_token).toEqual('test_id_token');
          expect(service.telemetry_enabled).toEqual(true);
        });
      });
    });

    describe('when session is empty', () => {
      beforeEach(() => {
        localStorage.clear();
      });

      it('leaves user unset', () => {
        expect(service.tryInitializeSession()).toBeUndefined();
        expect(service['user']).toBeUndefined();
      });
    });
  });

  describe('#storeTelemetryPreference', () => {
    beforeEach(() => {
      service.setDefaultSession();
      localStorage.clear();
    });

    describe('when passed true', () => {
      it('sets the user telemetry preference to true', () => {
        service.storeTelemetryPreference(true);
        expect(localStorage.getItem((service as any).userTelemetryStorageKey())).toEqual('true');
      });
    });

    describe('when passed false', () => {
      it('sets the user telemetry preference to false', () => {
        service.storeTelemetryPreference(false);
        expect(localStorage.getItem((service as any).userTelemetryStorageKey())).toEqual('false');
      });
    });
  });
});
