import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ProfileDetailsComponent } from './profile-details.component';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { Observable, throwError, of as observableOf } from 'rxjs';
import { ProfilesService } from '../../../../services/profiles/profiles.service';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';

class MockProfilesService {
  getProfile(_owner: string, _name: string): Observable<Object> {
    return observableOf({'name': 'theprofile', 'owner': 'person', 'controls': []});
  }
  deleteProfile(_owner: string, _name: string): Observable<Object> {
    return observableOf({status: 200});
  }
}

describe('ProfileDetailsComponent', () => {
  let fixture, component;

  const mockSession: any = {
    username: 'TestUser'
  };

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientTestingModule
      ],
      declarations: [
        ProfileDetailsComponent
      ],
      providers: [
        { provide: ChefSessionService, useValue: mockSession },
        { provide: ProfilesService, useClass: MockProfilesService }
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(ProfileDetailsComponent);
    component = fixture.componentInstance;
    spyOn(component.router, 'navigate');
  });

  describe('calling deleteProfile', () => {
    beforeEach(() => {
      fixture.detectChanges();
      spyOn(component.profilesService, 'deleteProfile')
        .and.returnValue(observableOf({ status: 200 }));
      component.deleteProfile({owner: 'human', name: 'profile'});
    });

    it('makes a call to the service to delete the profile', () => {
      expect(component.profilesService.deleteProfile).toHaveBeenCalled();
    });

    it('routes the user back to profiles-overview', () => {
      expect(component.router.navigate).toHaveBeenCalledWith(['/profiles']);
    });
  });

  describe('installProfile()', () => {

    it('makes a call to the service to upload the profile', () => {
      spyOn(component.availableProfilesService, 'installMarketProfile')
        .and.returnValue(observableOf({}));
      component.installProfile({ version: '2.1.0', name: 'profile' });
      expect(component.availableProfilesService.installMarketProfile)
        .toHaveBeenCalledWith('profile', '2.1.0');
    });

    describe('onNext()', () => {
      const profile = {'name': 'profile1', 'version': '2.1.0' };

      beforeEach(() => {
        spyOn(component, 'fetchProfile');
        spyOn(component, 'routeToProfile');
        spyOn(component.availableProfilesService, 'installMarketProfile')
          .and.returnValue(observableOf({ status: 200 }));
      });

      it('calls fetchProfile', () => {
        component.installProfile(profile);
        expect(component.fetchProfile)
          .toHaveBeenCalledWith(mockSession.username, profile.name, profile.version);
      });

      it('calls routeToProfile', () => {
        component.installProfile(profile);
        expect(component.routeToProfile)
          .toHaveBeenCalledWith(mockSession.username, profile.name, profile.version);
      });

      it('sets isAvailable to false', () => {
        expect(component.isAvailable).toBe(false);
      });
    });

    describe('onError()', () => {
      const profile = {'name': 'profile1', 'version': '2.1.0' };

      beforeEach(() => {
        spyOn(component, 'showInstallError');
        spyOn(component.availableProfilesService, 'installMarketProfile')
          .and.returnValue(throwError({ status: 400 }));
      });

      it('displays the error', () => {
        component.installProfile(profile);
        expect(component.showInstallError).toHaveBeenCalled();
      });
    });
  });

  describe('calling fetchProfile', () => {
    describe('when owner is undefined', () => {
      it('calls the available profiles service', () => {
        fixture.detectChanges();
        spyOn(component.availableProfilesService, 'getProfile').and.returnValue(observableOf({}));
        component.fetchProfile(undefined, '2.1.0', 'profile');
        expect(component.availableProfilesService.getProfile).toHaveBeenCalled();
      });
    });
    describe('when owner is defined', () => {
      it('calls the profiles service', () => {
        fixture.detectChanges();
        spyOn(component.profilesService, 'getProfile').and.returnValue(observableOf({}));
        component.fetchProfile('tester', '2.1.0', 'profile');
        expect(component.profilesService.getProfile).toHaveBeenCalled();
      });
    });
  });

  describe('showDownloadError()', () => {
    it('shows download error notification', () => {
      component.showDownloadError();
      expect(component.downloadErrorVisible).toBe(true);
    });
  });

  describe('hideDownloadError()', () => {
    it('hides download error notification', () => {
      component.hideDownloadError();
      expect(component.downloadErrorVisible).toBe(false);
    });
  });

  describe('showDeleteError()', () => {
    it('shows delete error notification', () => {
      component.showDeleteError();
      expect(component.deleteErrorVisible).toBe(true);
    });
  });

  describe('hideDeleteError()', () => {
    it('hides delete error notification', () => {
      component.hideDeleteError();
      expect(component.deleteErrorVisible).toBe(false);
    });
  });

  describe('downloadProfile()', () => {
    describe('when owner is undefined', () => {
      it('fetches the tarball from the available profiles service', () => {
        spyOn(component.availableProfilesService, 'getTarFile').and.returnValue(observableOf());

        component.downloadProfile({ name: 'ssh', version: '2.1.0'});

        expect(component.availableProfilesService.getTarFile).toHaveBeenCalled();
      });

      describe('onError()', () => {
        it('shows download error', () => {
          spyOn(component.availableProfilesService, 'getTarFile')
            .and.returnValue(throwError('error'));
          spyOn(component, 'showDownloadError');

          component.downloadProfile({ name: 'ssh', version: '2.1.0'});

          expect(component.showDownloadError).toHaveBeenCalled();
        });
      });
    });

    describe('when owner is defined', () => {
      it('fetches the tarball from the profiles service', () => {
        spyOn(component.profilesService, 'getTarFile').and.returnValue(observableOf());

        component.downloadProfile({owner: 'bob', name: 'ssh', version: '2.1.0'});

        expect(component.profilesService.getTarFile).toHaveBeenCalled();
      });

      describe('onError()', () => {
        it('shows download error', () => {
          spyOn(component, 'showDownloadError');
          spyOn(component.profilesService, 'getTarFile')
            .and.returnValue(throwError('error'));

          component.downloadProfile({ owner: 'bob', name: 'ssh', version: '2.1.0'});

          expect(component.showDownloadError).toHaveBeenCalled();
        });
      });
    });
  });
});
