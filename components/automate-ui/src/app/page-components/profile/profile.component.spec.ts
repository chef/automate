import { CUSTOM_ELEMENTS_SCHEMA, DebugElement } from '@angular/core';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { By } from '@angular/platform-browser';
import { RouterTestingModule } from '@angular/router/testing';
import { Observable, Subject, of as observableOf } from 'rxjs';
import { MockComponent } from 'ng2-mock-component';

import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { MetadataService } from 'app/services/metadata/metadata.service';

import { MockChefSessionService } from 'app/testing/mock-chef-session.service';
import { ProfileComponent } from './profile.component';

class MockMetadataService {
  getBuildVersion(): Observable<string> {
    return observableOf('');
  }
}

class MockTelemetryService {
  track(_event?: string, _properties?: any): void { }
}

describe('ProfileComponent', () => {
  let fixture: ComponentFixture<ProfileComponent>;
  let element: DebugElement;
  let component: ProfileComponent;
  let chefSessionService: ChefSessionService;
  let telemetryService: TelemetryService;
  let metadataService: MetadataService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule
      ],
      declarations: [
        MockComponent({ selector: 'app-welcome-modal' }),
        MockComponent({ selector: 'chef-modal', inputs: ['visible'] }),
        ProfileComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        { provide: MetadataService, useClass: MockMetadataService },
        { provide: ChefSessionService, useClass: MockChefSessionService }
      ],
      schemas: [CUSTOM_ELEMENTS_SCHEMA]
    });

    localStorage.setItem('welcome-modal-seen', 'true');

    fixture = TestBed.createComponent(ProfileComponent);
    element = fixture.debugElement;
    component = fixture.componentInstance;
    component.ngOnInit();

    metadataService = TestBed.get(MetadataService);
    telemetryService = TestBed.get(TelemetryService);
    chefSessionService = TestBed.get(ChefSessionService);
  });

  it('hides dropdown by default', () => {
    expect(component.dropdownVisible).toEqual(false);
  });

  /* TODO: Auth Team: need a new version API endpoint */
  xdescribe('initialization', () => {
    let subject: Subject<any>;

    beforeEach(() => subject = new Subject());

    it('gets current build version', () => {
      const expectedVersion = '1.2.3';

      spyOn(metadataService, 'getBuildVersion')
        .and.returnValue(subject);
      spyOn(telemetryService, 'track');

      component.ngOnInit();

      subject.next(expectedVersion);
      fixture.detectChanges();

      expect(metadataService.getBuildVersion).toHaveBeenCalled();
      expect(telemetryService.track).toHaveBeenCalledWith('automateVersion',
        { automateVersion: `${expectedVersion}` });
      expect(component.buildVersion).toEqual(expectedVersion);
    });
  });

  describe('cleanup', () => {

    it('unsubscribes from all open subscriptions', () => {
      component.ngOnInit();
      spyOn(component.versionSub, 'unsubscribe');

      component.ngOnDestroy();

      expect(component.versionSub.unsubscribe).toHaveBeenCalled();
    });
  });

  describe('displayName', () => {
    it('returns first and last', () => {
      expect(component.displayName).toEqual(chefSessionService.fullname);
      expect(component.displayName).toEqual('Test Mock');
    });
  });

  describe('email', () => {
    it('returns the username for the session user', () => {
      expect(component.email).toEqual(chefSessionService.username);
      expect(component.email).toEqual('testmock');
    });
  });

  describe('username', () => {
    it('returns the username for the session user', () => {
      expect(component.userName).toEqual(chefSessionService.username);
      expect(component.userName).toEqual('testmock');
    });
  });

  describe('clicking on the user', () => {

    it('toggles dropdown visibility', () => {
      fixture.detectChanges();
      const event = { stopPropagation: () => '' };
      const userInfo = element.query(By.css('.dropdown-toggle'));

      expect(component.dropdownVisible).toEqual(false);

      userInfo.triggerEventHandler('click', event);
      expect(component.dropdownVisible).toEqual(true);

      userInfo.triggerEventHandler('click', event);
      expect(component.dropdownVisible).toEqual(false);
    });
  });

  describe('sign out link', () => {

    it('logs out', () => {
      spyOn(chefSessionService, 'logout');

      component.dropdownVisible = true;
      fixture.detectChanges();

      const link = element.query(By.css('button.logout'));
      link.triggerEventHandler('click', {});
      expect(chefSessionService.logout).toHaveBeenCalledWith('/', true);
    });
  });

  describe('build version link', () => {

    it('displays the build version', () => {

      const version = '20180416135645';
      spyOn(metadataService, 'getBuildVersion')
        .and.returnValue(observableOf(version));

      component.dropdownVisible = true;
      fixture.detectChanges();

      const expected = `Build: ${version}`;
      const actual = element.nativeElement.querySelector('.build').innerText;
      expect(actual).toEqual(expected);
    });
  });

  describe('release notes link', () => {
    it('links to the full version text in a new tab', () => {

      const version = '20180416135645';
      spyOn(metadataService, 'getBuildVersion')
        .and.returnValue(observableOf(version));

      component.dropdownVisible = true;
      fixture.detectChanges();

      const link = element.nativeElement.querySelector('.release-notes');
      const href = link.getAttribute('href');
      const target = link.getAttribute('target');
      expect(href).toEqual(`https://automate.chef.io/release-notes/?v=${version}`);
      expect(target).toEqual('_blank');
    });
  });

  describe('about automate link', () => {

    it('displays the welcome modal', () => {
      component.dropdownVisible = true;
      fixture.detectChanges();

      spyOn(component, 'showWelcomeModal');

      const link = element.query(By.css('.welcome-modal-button'));
      link.triggerEventHandler('click', {});

      expect(component.showWelcomeModal).toHaveBeenCalled();
    });
  });

  describe('profile link', () => {

    it('links to user profile', () => {
      component.dropdownVisible = true;
      fixture.detectChanges();

      const link = element.nativeElement.querySelector('a.profile');
      const href = link.getAttribute('href');
      expect(href).toEqual(`/user-details/${component.userName}`);
    });
  });
});
