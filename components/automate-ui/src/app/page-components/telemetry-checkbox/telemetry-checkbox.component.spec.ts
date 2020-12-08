import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { By } from '@angular/platform-browser';

import { TelemetryCheckboxComponent } from './telemetry-checkbox.component';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

import { MockChefSessionService } from 'app/testing/mock-chef-session.service';

class MockTelemetryService {
  hasTelemetryResponse = true;
  telemetryEnabled = true;

  public setUserTelemetryPreference(): void {}
  public setTelemetryPreferences(): void {}
  public togglePersonalTelemetryCollection(): void {}
}

class MockTelemetryServiceDisabled {
  hasTelemetryResponse = true;
  telemetryEnabled = false;

  public setUserTelemetryPreference(): void {}
  public setTelemetryPreferences(): void {}
  public togglePersonalTelemetryCollection(): void {}
}

describe('TelemetryCheckboxComponent when telemetry enabled', () => {
  let component: TelemetryCheckboxComponent;
  let fixture: ComponentFixture<TelemetryCheckboxComponent>;
  let element;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        TelemetryCheckboxComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryService },
        { provide: ChefSessionService, useClass: MockChefSessionService }
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TelemetryCheckboxComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
    element = fixture.debugElement;
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should display the checkbox', () => {
    expect(component.isTelemetryServiceEnabled).toBeTruthy();
  });

  it('should check the checkbox', () => {
    expect(component.telemetryPersonalPref).toBeTruthy();
  });

  it('should change the telemetry user preference when clicked', () => {
    const checkbox = element.query((By.css('chef-checkbox')));

    spyOn(component, 'togglePersonalTelemetryCollection');
    checkbox.triggerEventHandler('change');

    expect(component.togglePersonalTelemetryCollection).toHaveBeenCalled();
  });
});

describe('TelemetryCheckboxComponent when telemetry disabled', () => {
  let component: TelemetryCheckboxComponent;
  let fixture: ComponentFixture<TelemetryCheckboxComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        TelemetryCheckboxComponent
      ],
      providers: [
        { provide: TelemetryService, useClass: MockTelemetryServiceDisabled },
        { provide: ChefSessionService, useClass: MockChefSessionService }
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TelemetryCheckboxComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should not display the checkbox', () => {
    expect(component.isTelemetryServiceEnabled).toBeFalsy();
  });
});
