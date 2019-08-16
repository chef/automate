import { TestBed } from '@angular/core/testing';
import { ChefSessionService } from './chef-session.service';

describe('ChefSessionService', () => {
  let service: ChefSessionService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [
        ChefSessionService
      ]
    });

    service = TestBed.get(ChefSessionService);
  });

  afterEach(() => {
    localStorage.clear();
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
          expect(service.telemetry_enabled).toEqual(null);
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

  describe('#refreshSessionCallback', () => {
    let event, xhr;
    beforeEach(() => {
      service.setDefaultSession();
      service.tryInitializeSession();
      event = {};
      xhr = {};
      event.target = xhr;
      xhr.readyState = 4;
      spyOn(service, 'ingestIDToken');
    });

    describe('when session refresh succeeds', () => {
      beforeEach(() => {
        xhr.status = 200;
        xhr.response = { id_token: 'refreshed_id_token'};
      });

      it('ingests the returned ID token', () => {
        service.refreshSessionCallback(event);
        expect(service.ingestIDToken).toHaveBeenCalledWith('refreshed_id_token');
      });
    });

    describe('when session refresh fails', () => {
      beforeEach(() => {
        xhr.status = 401;
        spyOn(service, 'currentPath').and.returnValue('/some/path');
        spyOn(service, 'logout');
      });

      it('does not ingest any token, but calls logout', () => {
        service.refreshSessionCallback(event);
        expect(service.logout).toHaveBeenCalledWith('/some/path', true);
        expect(service.ingestIDToken).not.toHaveBeenCalled();
      });
    });
  });
});
