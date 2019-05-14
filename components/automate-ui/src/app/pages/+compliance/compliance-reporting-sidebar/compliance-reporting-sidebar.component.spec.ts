import { DebugElement } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { MockComponent } from 'ng2-mock-component';

import { checkFirstPerm } from 'app/testing/spec-helpers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import {
  ComplianceLandingComponent
} from 'app/pages/compliance-landing/compliance-landing.component';
import { ComplianceReportingSidebarComponent } from './compliance-reporting-sidebar.component';

describe('ComplianceReportingSidebarComponent', () => {
  let component: ComplianceReportingSidebarComponent;
  let fixture: ComponentFixture<ComplianceReportingSidebarComponent>;
  let complianceLandingComponent: ComplianceLandingComponent;
  let element: DebugElement;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule
      ],
      declarations: [
        ComplianceReportingSidebarComponent,
        ComplianceLandingComponent,
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

    fixture = TestBed.createComponent(ComplianceReportingSidebarComponent);
    component = fixture.componentInstance;
    complianceLandingComponent =
      TestBed.createComponent(ComplianceLandingComponent).componentInstance;
    element = fixture.debugElement;
  });

  it('should be created', () => {
    fixture.detectChanges();
    expect(component).toBeTruthy();
  });

  describe('compliance sidebar', () => {

    it('shows all links consistent with compliance-landing', () => {
      fixture.detectChanges();
      const links = element.nativeElement
        .querySelectorAll('div.nav-items chef-sidebar-entry');
      expect(links.length).toBe(complianceLandingComponent.routeList.length);
    });

    it('has route order consistent with compliance-landing', () => {
      fixture.detectChanges();
      const links = element.nativeElement
        .querySelectorAll('div.nav-items chef-sidebar-entry');
      for (let i = 0; i < complianceLandingComponent.routeList.length; i++) {
        expect(links[i].getAttribute('route'))
          .toBe(complianceLandingComponent.routeList[i].route);
      }
    });

   it('has paths consistent with compliance-landing', () => {
     fixture.detectChanges();
     const elements = Array
       .from<HTMLElement>(
         element.nativeElement.querySelectorAll('div.nav-items app-authorized'))
       .filter(elem =>
         elem.firstElementChild && elem.firstElementChild.tagName === 'CHEF-SIDEBAR-ENTRY');

     expect(elements.length).toBe(complianceLandingComponent.routeList.length);
     for (let i = 0; i < complianceLandingComponent.routeList.length; i++) {
       checkFirstPerm(
         'anyOf',
         elements[i].getAttribute('ng-reflect-any-of'),
         complianceLandingComponent.routeList[i].anyOfCheck);
       checkFirstPerm(
         'allOf',
         elements[i].getAttribute('ng-reflect-all-of'),
         complianceLandingComponent.routeList[i].allOfCheck);
     }
   });
  });
});
