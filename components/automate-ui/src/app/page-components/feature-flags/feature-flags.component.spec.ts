import { TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';
import { FeatureFlagsComponent, FlagTypes } from './feature-flags.component';

function keyPress(keycode: number) {
  let event: Event;
  event = document.createEvent('Event');
  event.initEvent('keyup', true, true);

  // Hack DOM Level 3 Events "key" prop into keyboard event.
  // From: http://marcysutton.github.io/a11y-testing-angular2/#/11
  Object.defineProperty(event, 'keyCode', {
    value: keycode,
    enumerable: false,
    writable: false,
    configurable: true
  });

  document.dispatchEvent(event);
}

function sendBeta() {
  keyPress(66); // 'b'
  keyPress(69); // 'e'
  keyPress(84); // 't'
  keyPress(65); // 'a'
}

function sendLega() {
  keyPress(76); // 'l'
  keyPress(69); // 'e'
  keyPress(71); // 'g'
  keyPress(65); // 'a'
}

function sendFeat() {
  keyPress(70); // 'f'
  keyPress(69); // 'e'
  keyPress(65); // 'a'
  keyPress(84); // 't'
}

class MockTelemetryService {
  track(_event?: string, _properties?: any): void {

  }
}

describe('FeatureFlags', () => {
  let fixture, component, element;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        FeatureFlagsComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        FeatureFlagsService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(FeatureFlagsComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement.nativeElement;

  });

  describe('when not activated', () => {
    beforeEach(() => {
      component.isVisible = false;
    });

    describe('experimental modal', () => {
      beforeEach(() => {
        fixture.detectChanges();
        sendFeat();
      });

      it('should activate when key combo is entered', () => {
        expect(component.isVisible).toBe(true);
      });

      it('should have the beta warning', () => {
        expect(component.warning).toBe(component.betaWarning);
      });

      it('should have the experimental type', () => {
        expect(component.flagType).toBe(FlagTypes.beta_or_experimental);
      });
    });

    describe('beta modal', () => {

      beforeEach(() => {
        fixture.detectChanges();
        sendBeta();
      });
      it('should activate when key combo is entered', () => {
        expect(component.isVisible).toBe(true);
      });

      it('should have the beta warning', () => {
        expect(component.warning).toBe(component.betaWarning);
      });

      it('should have the beta type', () => {
        expect(component.flagType).toBe(FlagTypes.beta);
      });
    });

    describe('legacy modal', () => {
      beforeEach(() => {
        fixture.detectChanges();
        sendLega();
      });

      it('should activate when key combo is entered', () => {
        expect(component.isVisible).toBe(true);
      });

      it('should have the legacy warning', () => {
        expect(component.warning).toBe(component.legacyWarning);
      });

      it('should have the legacy type', () => {
        expect(component.flagType).toBe(FlagTypes.legacy);
      });
    });
  });

  describe('when activated', () => {

    it('should de-activate when "feat" key combo is entered a second time', () => {
      component.isVisible = true;
      component.flagType = FlagTypes.beta_or_experimental;
      fixture.detectChanges();
      sendFeat();
      expect(component.isVisible).toBe(false);
    });

    it('should de-activate when "beta" key combo is entered a second time', () => {
      component.isVisible = true;
      component.flagType = FlagTypes.beta;
      fixture.detectChanges();
      sendBeta();
      expect(component.isVisible).toBe(false);
    });

    it('should de-activate when "lega" key combo is entered a second time', () => {
      component.isVisible = true;
      component.flagType = FlagTypes.legacy;
      fixture.detectChanges();
      sendLega();
      expect(component.isVisible).toBe(false);
    });

    it('should switch when "feat" key combo is entered after opened with "beta"', () => {
      component.isVisible = true;
      component.flagType = FlagTypes.beta;
      fixture.detectChanges();
      sendFeat();
      expect(component.isVisible).toBe(true);
      expect(component.flagType).toBe(FlagTypes.beta_or_experimental);
    });

    it('should switch when "beta" key combo is entered after opened with "lega"', () => {
      component.isVisible = true;
      component.flagType = FlagTypes.legacy;
      fixture.detectChanges();
      sendBeta();
      expect(component.isVisible).toBe(true);
      expect(component.flagType).toBe(FlagTypes.beta);
    });

    it('should switch when "lega" key combo is entered after opened with "feat"', () => {
      component.isVisible = true;
      component.flagType = FlagTypes.beta_or_experimental;
      fixture.detectChanges();
      sendLega();
      expect(component.isVisible).toBe(true);
      expect(component.flagType).toBe(FlagTypes.legacy);
    });
  });

  describe('when activated', () => {

    beforeEach(() => {
      component.isVisible = true;
    });

    it('should de-activate when "x" is clicked', () => {
      fixture.detectChanges();
      element.querySelector('.close').click();
      expect(component.isVisible).toBe(false);
    });

    it('should de-activate when "Cancel" button is clicked', () => {
      fixture.detectChanges();
      element.querySelector('#cancel-button').click();
      expect(component.isVisible).toBe(false);
    });

    it('should set feature flag when its name clicked', () => {
      spyOn(component, 'updateFlag');
      component.features = ['foo'];
      fixture.detectChanges();
      element.querySelector('.features .title-desc').click();
      expect(component.updateFlag).toHaveBeenCalledWith('foo');
    });

    it('should set feature flag when its slider toggled', () => {
      spyOn(component, 'updateFlag');
      component.features = ['foo'];
      fixture.detectChanges();
      element.querySelector('.features .onoffswitch-label').click();
      expect(component.updateFlag).toHaveBeenCalledWith('foo');
    });
  });

});
