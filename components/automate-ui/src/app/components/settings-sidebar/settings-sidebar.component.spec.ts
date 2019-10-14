import { of as observableOf } from 'rxjs';
import { DebugElement } from '@angular/core';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { using } from 'app/testing/spec-helpers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { SettingsLandingComponent } from 'app/pages/settings-landing/settings-landing.component';
import { runtimeChecks } from 'app/ngrx.reducers';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';
import { IAMMajorVersion } from 'app/entities/policies/policy.model';
import { checkFirstPerm } from 'app/testing/spec-helpers';
import { SettingsSidebarComponent } from './settings-sidebar.component';

describe('SettingsSidebarComponent', () => {
  let fixture: ComponentFixture<SettingsSidebarComponent>;
  let component: SettingsSidebarComponent;
  let settingsLandingComponent: SettingsLandingComponent;
  let element: DebugElement;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [RouterTestingModule,
        StoreModule.forRoot({
          policies: policyEntityReducer
        }, { runtimeChecks })
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

  describe('IAM v2', () => {
    beforeEach(() => {
      component.iamMajorVersion$ = observableOf(<IAMMajorVersion>'v2');
    });

    using([
      [false, 10, 'v2'],
      [true, 11, 'v2.1']
    ], function (projectsEnabled: boolean, linkCount: number, versionName: string) {

      it(`shows all links consistent with settings-landing for ${versionName}`, () => {
        component.projectsEnabled$ = observableOf(projectsEnabled);
        fixture.detectChanges();
        const links = element.nativeElement
          .querySelectorAll('div.nav-items chef-sidebar-entry');
        expect(links.length).toBe(linkCount);
      });

      it(`has route order consistent with settings-landing for ${versionName}`, () => {
        component.projectsEnabled$ = observableOf(projectsEnabled);
        fixture.detectChanges();
        const links = element.nativeElement
          .querySelectorAll('div.nav-items chef-sidebar-entry');
        for (let i = 0; i < links.length; i++) {
          expect(links[i].getAttribute('route'))
            .toBe(settingsLandingComponent.routeList[i].route);
        }
      });

      it(`has paths consistent with settings-landing for ${versionName}`, () => {
        component.projectsEnabled$ = observableOf(projectsEnabled);
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
  });

  describe('IAM v1', () => {
    beforeEach(() => {
      component.iamMajorVersion$ = observableOf(<IAMMajorVersion>'v1');
    });

    it('shows 8 links', () => {
      fixture.detectChanges();
      const links = element.nativeElement
        .querySelectorAll('div.nav-items chef-sidebar-entry');
      expect(links.length).toBe(8);
    });

    using([
      ['Notifications', '/settings/notifications', 0],
      ['Data Feeds', '/settings/data-feed', 1],
      ['Node Integrations', '/settings/node-integrations', 2],
      ['Node Credentials', '/settings/node-credentials', 3],
      ['Node Lifecycle', '/settings/node-lifecycle', 4],
      ['Users', '/settings/users', 5],
      ['Teams', '/settings/teams', 6],
      ['API Tokens', '/settings/tokens', 7]
    ], (label: string, path: string, position: number) => {
      it(`displays the ${label} navigation link`, () => {
        fixture.detectChanges();
        const links = element.nativeElement
          .querySelectorAll('div.nav-items chef-sidebar-entry');
        expect(links[position].innerText).toBe(label);
        expect(links[position].getAttribute('route')).toBe(path);
      });
    });
  });
});
