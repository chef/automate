import { DebugElement } from '@angular/core';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { StoreModule, Store } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { SettingsLandingComponent } from 'app/pages/settings-landing/settings-landing.component';
import { NgrxStateAtom, ngrxReducers, defaultInitialState, runtimeChecks } from 'app/ngrx.reducers';
import { checkFirstPerm } from 'app/testing/spec-helpers';
import { SettingsSidebarComponent } from './settings-sidebar.component';

describe('SettingsSidebarComponent', () => {
  let store: Store<NgrxStateAtom>;
  let fixture: ComponentFixture<SettingsSidebarComponent>;
  let component: SettingsSidebarComponent;
  let settingsLandingComponent: SettingsLandingComponent;
  let element: DebugElement;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { initialState: defaultInitialState, runtimeChecks })
      ],
      declarations: [
        SettingsSidebarComponent,
        SettingsLandingComponent,
        // Note: if we want to match contents, we cannot swallow them: thus we
        // need to provide a template here. <ng-content> mocks these as doing
        // nothing but a "pass-through" of what the components wrap.
        MockComponent({ selector: 'app-landing',
                        inputs: ['routePerms'] }),
        MockComponent({ selector: 'app-authorized',
                        inputs: ['allOf', 'anyOf'],
                        template: '<ng-content></ng-content>' }),
        MockComponent({ selector: 'chef-sidebar',
                        template: '<ng-content></ng-content>' }),
        MockComponent({ selector: 'chef-sidebar-entry',
                        inputs: ['route', 'icon', 'exact'],
                        template: '<ng-content></ng-content>' })
      ],
      providers: [
        FeatureFlagsService
      ]
    });
    store = TestBed.inject(Store);
    spyOn(store, 'dispatch').and.callThrough();
    fixture = TestBed.createComponent(SettingsSidebarComponent);
    component = fixture.componentInstance;
    component.featureFlagOn = true;
    settingsLandingComponent =
      TestBed.createComponent(SettingsLandingComponent).componentInstance;
    element = fixture.debugElement;
  });

  it('should be created', () => {
    fixture.detectChanges();

    expect(component).toBeTruthy();
  });

  it('shows all links consistent with settings-landing', () => {
    fixture.detectChanges();
    const links = element.nativeElement
      .querySelectorAll('div.nav-items chef-sidebar-entry');
    expect(links.length).toBe(11);
  });

  it('has route order consistent with settings-landing', () => {
    fixture.detectChanges();
    const links = element.nativeElement
      .querySelectorAll('div.nav-items chef-sidebar-entry');
    for (let i = 0; i < links.length; i++) {
      expect(links[i].getAttribute('route'))
        .toBe(settingsLandingComponent.routeList[i].route);
    }
  });

  it('has paths consistent with settings-landing', () => {
    fixture.detectChanges();
    const elements = Array
      .from<HTMLElement>(
        element.nativeElement.querySelectorAll('div.nav-items app-authorized'))
      .filter(elem =>
        elem.firstElementChild && elem.firstElementChild.tagName === 'CHEF-SIDEBAR-ENTRY');

    for (let i = 0; i < elements.length; i++) {
      checkFirstPerm(
        'anyOf',
        elements[i].getAttribute('ng-reflect-any-of'),
        settingsLandingComponent.routeList[i].anyOfCheck);
      checkFirstPerm(
        'allOf',
        elements[i].getAttribute('ng-reflect-all-of'),
        settingsLandingComponent.routeList[i].allOfCheck);
    }
  });
});
