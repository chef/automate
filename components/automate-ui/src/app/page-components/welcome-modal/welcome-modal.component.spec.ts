import { TestBed } from '@angular/core/testing';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { NO_ERRORS_SCHEMA, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { By } from '@angular/platform-browser';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { WelcomeModalComponent } from './welcome-modal.component';
import { LocalStorageService } from '../../services/storage/localstorage.service';
import { SessionStorageService } from '../../services/storage/sessionstorage.service';
import { ChefSessionService } from '../../services/chef-session/chef-session.service';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { MockChefSessionService } from 'app/testing/mock-chef-session.service';
import { TriggerWelcomeStatus } from 'app/entities/license/license.reducer';
import { EntityStatus } from 'app/entities/entities';

class MockTelemetryService {
  hasTelemetryResponse = true;
  telemetryEnabled = true;

  public setUserTelemetryPreference(): void {}
  public setTelemetryPreferences(): void {}
  public togglePersonalTelemetryCollection(): void {}
}

function welcomeReducer(): () => { triggerWelcome: TriggerWelcomeStatus } {
    return () => ({
      triggerWelcome: {
        status: EntityStatus.loadingSuccess
      }
    });
  }

describe('WelcomeModalComponent', () => {
  let fixture, component, element;
  let localStorageService: LocalStorageService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        WelcomeModalComponent,
        MockComponent({ selector: 'app-telemetry-component' }),
        MockComponent({ selector: 'chef-checkbox', inputs: ['checked', 'disabled'] })
      ],
      imports: [
        HttpClientTestingModule,
        StoreModule.forRoot({
          licenseStatus: welcomeReducer
        })
      ],
      providers: [
        LocalStorageService,
        SessionStorageService,
        { provide: ChefSessionService, useClass: MockChefSessionService },
        { provide: TelemetryService, useClass: MockTelemetryService }
      ],
      schemas: [
        NO_ERRORS_SCHEMA,
        CUSTOM_ELEMENTS_SCHEMA
      ]
    });

    fixture = TestBed.createComponent(WelcomeModalComponent);
    component = fixture.componentInstance;

    localStorageService = TestBed.get(LocalStorageService);
  });

  describe('header', () => {
    beforeEach(() => {
      component.showModal();
      element = fixture.debugElement;
      fixture.detectChanges();
    });

    it('has header text', () => {
      const header = element.query((By.css('.display2')));
      expect(header.nativeElement.innerText).toEqual('Welcome to Chef Automate');
    });

    describe('"X" link', () => {
      it('closes the modal', () => {
        const link = element.query((By.css('.close-button')));

        spyOn(component, 'closeModal');
        link.triggerEventHandler('click');

        expect(component.closeModal).toHaveBeenCalled();
      });
    });
  });

  describe('content', () => {
    beforeEach(() => {
      component.showModal();
      element = fixture.debugElement;
      fixture.detectChanges();
    });

    it('has welcome text', () => {
      const text = element.query((By.css('.modal-content p')));
      expect(text.nativeElement.innerText).toContain('How can we help you get started?');
    });

    describe('"close" link', () => {
      it('closes the modal', () => {
        const link = element.query((By.css('.close-button')));

        spyOn(component, 'closeModal');
        link.triggerEventHandler('click');

        expect(component.closeModal).toHaveBeenCalled();
      });
    });

    describe('"help me" link', () => {
      it('links to "how to learn chef" page in a new tab', () => {
        const link = element.query((By.css('li:nth-child(1) a')));
        const { href, target } = link.attributes;
        expect(href).toEqual('https://automate.chef.io/docs');
        expect(target).toEqual('_blank');
      });
    });

    describe('"teach me" link', () => {
      it('links to "how to learn chef" page in a new tab', () => {
        const link = element.query((By.css('li:nth-child(2) a')));
        const { href, target } = link.attributes;
        expect(href).toEqual('https://learn.chef.io/');
        expect(target).toEqual('_blank');
      });
    });
  });

  describe('when the user has indicated not to have it shown again', () => {
    beforeEach(() => {
      localStorage.clear();
      sessionStorage.clear();
      localStorageService.putBoolean('show-welcome-modal-on-startup', false);
      sessionStorage.clear();
    });

    it('does not open on startup', () => {
      expect(component.isVisible).toEqual(false);
    });

    it('does not open even when given the go-ahead to open', () => {
      component.handleTriggerWelcome({status: EntityStatus.loadingSuccess});
      expect(component.isVisible).toEqual(false);
    });
  });

  describe('when the user has indicated to have it shown again', () => {
    beforeEach(() => {
      localStorage.clear();
      sessionStorage.clear();
      localStorageService.putBoolean('show-welcome-modal-on-startup', true);
    });

    it('does not open on startup', () => {
      expect(component.isVisible).toEqual(false);
    });

    it('opens when given the go-ahead to open', () => {
      component.handleTriggerWelcome({status: EntityStatus.loadingSuccess});
      expect(component.isVisible).toEqual(true);
    });
  });

  describe('when the user has no indication to have it shown again', () => {
    beforeEach(() => {
      localStorage.clear();
      sessionStorage.clear();
    });

    it('does not open on startup', () => {
      expect(component.isVisible).toEqual(false);
    });

    it('open when given the go-ahead to open', () => {
      component.handleTriggerWelcome({status: EntityStatus.loadingSuccess});
      expect(component.isVisible).toEqual(true);
    });
  });

});
