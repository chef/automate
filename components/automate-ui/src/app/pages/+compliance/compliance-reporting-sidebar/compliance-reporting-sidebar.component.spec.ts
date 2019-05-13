import { DebugElement } from '@angular/core';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { MockComponent } from 'ng2-mock-component';

import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import {
  ComplianceLandingComponent
} from 'app/pages/compliance-landing/compliance-landing.component';
import { Check } from 'app/components/authorized/authorized.component';
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
        label + ': is empty in ComplianceLanding but not in compliance-sidebar template');
      return;
    } else if (!permsFromTemplate) {
      expect(permsFromLandingComp).toBeNull(
        label + ': is not empty in ComplianceLanding but is in compliance-sidebar template');
      return;
    }
    const firstCheckItem = permsFromLandingComp[0];
    let [ path, verb ] = permsFromTemplate.split(',');
    // If permsFromTemplate path is more than 30 characters, 
    // verb will be undefined. We will be unable to test the verb 
    // and need to mark firstCheckItem[1] as undefined to pass test.
    // AND If permsFromTemplate verb charaters is less than the
    // firstCheckItem[1] we need to match the character length to pass test.
    firstCheckItem[1] = verb ? firstCheckItem[1].substring(0, verb.length) : undefined;
    // If permsFromTemplate path is more than 30 characters, the path
    // will be cutt off and firstCheckItem[0] will need to match 
    // character length to pass test.
    expect(path).toBe(firstCheckItem[0].substring(0, 30));
    expect(verb).toBe(firstCheckItem[1]);
  }
});
