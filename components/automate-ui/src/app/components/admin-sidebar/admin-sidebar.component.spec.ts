import { of as observableOf } from 'rxjs';
import { DebugElement } from '@angular/core';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { using } from 'app/testing/spec-helpers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { SettingsLandingComponent } from 'app/pages/settings-landing/settings-landing.component';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';
import { Check } from 'app/components/authorized/authorized.component';
import { AdminSidebarComponent } from './admin-sidebar.component';

describe('AdminSidebarComponent', () => {
  let fixture: ComponentFixture<AdminSidebarComponent>;
  let component: AdminSidebarComponent;
  let settingsLandingComponent: SettingsLandingComponent;
  let element: DebugElement;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [RouterTestingModule,
        StoreModule.forRoot({
          policies: policyEntityReducer
        })
      ],
      declarations: [
        AdminSidebarComponent,
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

    fixture = TestBed.createComponent(AdminSidebarComponent);
    component = fixture.componentInstance;
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
      component.iamMajorVersion$ = observableOf('v2');
    });

    it('shows all links consistent with settings-landing', () => {
      fixture.detectChanges();
      const links = element.nativeElement
        .querySelectorAll('div.nav-items chef-sidebar-entry');
      expect(links.length).toBe(settingsLandingComponent.routeList.length);
    });

    it('has route order consistent with settings-landing', () => {
      fixture.detectChanges();
      const links = element.nativeElement
        .querySelectorAll('div.nav-items chef-sidebar-entry');
      for (let i = 0; i < settingsLandingComponent.routeList.length; i++) {
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

     expect(elements.length).toBe(settingsLandingComponent.routeList.length);
     for (let i = 0; i < settingsLandingComponent.routeList.length; i++) {
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

  describe('IAM v1', () => {
    beforeEach(() => {
      component.iamMajorVersion$ = observableOf('v1');
    });

    it('shows 7 links', () => {
      fixture.detectChanges();
      const links = element.nativeElement
        .querySelectorAll('div.nav-items chef-sidebar-entry');
      expect(links.length).toBe(7);
    });

    using([
      ['Notifications', '/settings/notifications', 0],
      ['Node Integrations', '/settings/node-integrations', 1],
      ['Node Credentials', '/settings/node-credentials', 2],
      ['Node Lifecycle', '/settings/node-lifecycle', 3],
      ['Users', '/settings/users', 4],
      ['Teams', '/settings/teams', 5],
      ['API Tokens', '/settings/tokens', 6]
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

  function checkFirstPerm(label: string, permsFromTemplate: string, permsFromLandingComp: Check[]) {
    // This issue (https://github.com/angular/angular/issues/28786)
    // limits `permsFromTemplate` to a max 30 chars, sufficient
    // only to check the first path and verb, hence the name of this function.
    // No way to guarantee a full check of all paths in one routeList element here. Sigh.

    // The flow and text of these expectations is designed to give meaningful information
    // when you edit either the template or the component but forget to do the other.
    // To see this, go into the sidebar template and either:
    // (1) change an 'anyOf' to an 'allOf' in an entry;
    // (2) change the path or verb inside an anyOf/allOf in an entry.
    if (!permsFromLandingComp) {
      expect(permsFromTemplate).toBeNull(
        label + ': is empty in SettingsLanding but not in admin-sidebar template');
      return;
    } else if (!permsFromTemplate) {
      expect(permsFromLandingComp).toBeNull(
        label + ': is not empty in SettingsLanding but is in admin-sidebar template');
      return;
    }
    const firstCheckItem = permsFromLandingComp[0];
    const [ path, verb ] = permsFromTemplate.split(',');
    expect(path).toBe(firstCheckItem[0]);
    expect(verb).toBe(firstCheckItem[1]);
  }
});
