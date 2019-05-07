import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { By } from '@angular/platform-browser';
import { RouterTestingModule } from '@angular/router/testing';
import { ProfileOverviewComponent } from './profile-overview.component';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { Observable, throwError, of as observableOf } from 'rxjs';
import { ProfilesService } from '../../../../services/profiles/profiles.service';
import { UploadService } from '../../../../services/profiles/upload.service';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';

class MockProfilesService {
  getAllProfiles(): Observable<Array<Object>> {
    return observableOf([{'name': 'profile'}, {'name': 'other_profile'}]);
  }
}

class MockUploadService {
  progress: Observable<any> = observableOf('hello');
  sendFile(_file: File): Observable<Object> {
    return observableOf('{}');
  }
}

describe('ProfilesOverviewComponent', () => {
  let fixture, component, element;

  const mockSession: any = {
    enterprise: 'mock-enterprise',
    username: 'TestUser',
    token: 'TestToken'
  };

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        HttpClientTestingModule
      ],
      declarations: [
        ProfileOverviewComponent
      ],
      providers: [
        {provide: ProfilesService, useClass: MockProfilesService},
        {provide: ChefSessionService, useValue: mockSession},
        {provide: UploadService, useClass: MockUploadService}
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(ProfileOverviewComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
  });

  it('sets profilesListLoading to true', () => {
    expect(component.profilesListLoading).toEqual(true);
  });

  it('sets availableListLoading to true', () => {
    expect(component.availableListLoading).toEqual(true);
  });

  describe('ngOnInit()', () => {
    it('loads all installed profiles', () => {
      spyOn(component, 'loadProfiles').and.callThrough();
      component.ngOnInit();
      expect(component.loadProfiles).toHaveBeenCalled();
    });

    it('loads all available profiles', () => {
      spyOn(component, 'loadAvailableProfiles').and.callThrough();
      component.ngOnInit();
      expect(component.loadAvailableProfiles).toHaveBeenCalled();
    });

    it('sets profilesEmpty to false', () => {
      component.ngOnInit();
      expect(component.profilesEmpty).toBe(false);
    });

    it('calls checkForUpdatesAvailable', () => {
      spyOn(component, 'checkForUpdatesAvailable');
      spyOn(component, 'loadProfiles').and.returnValue(observableOf([]));
      spyOn(component, 'loadAvailableProfiles').and.returnValue(observableOf([]));
      component.ngOnInit();
      expect(component.checkForUpdatesAvailable).toHaveBeenCalled();
    });
  });

  describe('loadProfiles()', () => {
    it('fetches all profiles', () => {
      spyOn(component.profilesService, 'getAllProfiles').and.returnValue(observableOf([]));
      component.loadProfiles();
      expect(component.profilesService.getAllProfiles).toHaveBeenCalled();
    });

    it('sets profilesListLoading to false', done => {
      spyOn(component.profilesService, 'getAllProfiles').and.returnValue(observableOf([]));
      component.loadProfiles().subscribe(_profiles => {
        expect(component.profilesListLoading).toEqual(false);
        done();
      });
    });

    describe('when profiles asset store is enabled', () => {
      beforeEach(() => {
        spyOn(component.profilesService, 'getAllProfiles').and.returnValue(observableOf([]));
      });

      it('sets profilesEnabled to true', () => {
        component.loadProfiles();
        expect(component.profilesEnabled).toEqual(true);
      });

      describe('when profiles length is 0', () => {
        it('sets profilesEmpty to true', () => {
          component.loadProfiles();
          expect(component.profilesEnabled).toEqual(true);
        });
      });
    });

    describe('when profiles asset store is not enabled', () => {
      beforeEach(() => {
        const resp = { status: 404 };
        spyOn(component.profilesService, 'getAllProfiles').and.returnValue(throwError(resp));
        spyOn(component.profilesService, 'getVersion').and.returnValue(throwError(resp));
      });

      it('sets profilesEnabled to false', done => {
        component.loadProfiles().subscribe(null, _profiles => {
          expect(component.profilesEnabled).toEqual(false);
          done();
        });
      });
    });
  });

  describe('loadAvailableProfiles()', () => {
    beforeEach(() => {
      spyOn(component.availableProfilesService, 'getAllProfiles')
        .and.returnValue(observableOf([]));
    });

    it('fetches all profiles', () => {
      component.loadAvailableProfiles();
      expect(component.availableProfilesService.getAllProfiles).toHaveBeenCalled();
    });

    it('sets availableListLoading to false', done => {
      component.loadAvailableProfiles().subscribe(_profiles => {
        expect(component.availableListLoading).toEqual(false);
        done();
      });
    });
  });

  describe('getProfiles()', () => {
    const profiles = [
      { 'name': 'profile1', 'version': '2.1.0' },
      { 'name': 'profile2', 'version': '1.9.0' }
    ];
    it('uploads all profiles', () => {
      spyOn(component.availableProfilesService, 'installMarketProfile')
        .and.returnValue(observableOf({}));
      component.getProfiles(profiles);
      expect(component.availableProfilesService.installMarketProfile)
        .toHaveBeenCalledWith('profile1', '2.1.0');
      expect(component.availableProfilesService.installMarketProfile)
        .toHaveBeenCalledWith('profile2', '1.9.0');
    });
    describe('when response status is 200', () => {
      beforeEach(() => {
        spyOn(component, 'refreshProfiles');
        spyOn(component.availableProfilesService, 'installMarketProfile')
          .and.returnValue(observableOf({ status: 200 }));
        component.getProfiles(
          [{ 'name': 'profile1', 'version': '2.1.0' }, { 'name': 'profile2', 'version': '1.9.0' }]);
      });

      it('calls load profiles', () => {
        expect(component.refreshProfiles).toHaveBeenCalled();
      });
    });
    describe('when response status is an 400', () => {
      beforeEach(() => {
        spyOn(component, 'refreshProfiles');
        spyOn(component, 'showDownloadError');
        spyOn(component.availableProfilesService, 'installMarketProfile')
          .and.returnValue(throwError({ status: 400 }));
        component.getProfiles(
          [{ 'name': 'profile1', 'version': '2.1.0' }, { 'name': 'profile2', 'version': '1.9.0' }]);
      });

      it('does not call refreshProfiles', () => {
        expect(component.refreshProfiles).not.toHaveBeenCalled();
      });

      it('displays the error', () => {
        expect(component.showDownloadError).toHaveBeenCalled();
      });
    });
  });

  describe('when profilesEnabled is true', () => {
    beforeEach(() => {
      fixture.detectChanges();
      component.profilesEnabled = true;
      fixture.detectChanges();
    });

    it('does not display setup help', () => {
      const setupHelp = element.query(By.css('.setup-help'));
      expect(setupHelp).toBeNull();
    });
  });

  describe('when profilesEnabled is false', () => {
    beforeEach(() => {
      fixture.detectChanges();
      component.profilesEnabled = false;
      fixture.detectChanges();
    });

    it('displays setup help', () => {
      const setupHelp = element.query(By.css('.setup-help'));
      expect(setupHelp).not.toBeNull();
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

  describe('checkForUpdatesAvailable', () => {

    it('sets profileUpdatesAvailable to an empty array', () => {
      const profiles = [];
      component.checkForUpdatesAvailable(profiles);
      expect(component.profileUpdatesAvailable).toEqual([]);
    });
    describe('when a profile is marked with latest version', () => {

      beforeEach(() => {
        const profiles = [
          {name: 'apache', version: '2.1.0', latest_version: '2.2.0'},
          {name: 'linux', version: '1.1.0', latest_version: '1.8.0'},
          {name: 'ssh', version: '1.1.0'}
        ];
        component.availableProfiles = [
          {name: 'apache', version: '2.2.0'},
          {name: 'linux', version: '1.8.0'},
          {name: 'ssh', version: '1.1.0'}
        ];
        component.checkForUpdatesAvailable(profiles);
      });

      it('adds the corresponding market profile to the profileUpdatesAvailable array', () => {
        expect(component.profileUpdatesAvailable).toEqual([
          {name: 'apache', version: '2.2.0', installed_version: '2.1.0'},
          {name: 'linux', version: '1.8.0', installed_version: '1.1.0'}
        ]);
      });
    });

    describe('when a profile has no latest_version field ', () => {

      beforeEach(() => {
        const profiles = [
          {name: 'apache', version: '2.1.0'},
          {name: 'linux', version: '1.1.0'},
          {name: 'ssh', version: '1.1.0'}
        ];
        component.availableProfiles = [
          {name: 'apache', version: '2.1.0'},
          {name: 'linux', version: '1.1.0'},
          {name: 'ssh', version: '1.1.0'}
        ];
        component.checkForUpdatesAvailable(profiles);
      });

      it('adds nothing to the profileUpdatesAvailable array', () => {
        expect(component.profileUpdatesAvailable).toEqual([]);
      });
    });
  });

  describe('showAvailableUpdates()', () => {
    it('sets viewAvailableUpdatesList to true', () => {
      component.viewAvailableUpdatesList = false;
      component.showAvailableUpdates();
      expect(component.viewAvailableUpdatesList).toBe(true);
    });
  });

  describe('hideAvailableUpdates()', () => {
    it('sets viewAvailableUpdatesList to false', () => {
      component.viewAvailableUpdatesList = true;
      component.hideAvailableUpdates();
      expect(component.viewAvailableUpdatesList).toBe(false);
    });
  });
});
