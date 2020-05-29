import { RouterTestingModule } from '@angular/router/testing';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { StoreModule } from '@ngrx/store';
import { NavbarComponent } from './navbar.component';
import { MockComponent } from 'ng2-mock-component';
import { using } from 'app/testing/spec-helpers';
import { runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import {
  PolicyEntityInitialState,
  policyEntityReducer
} from 'app/entities/policies/policy.reducer';
import { ProductDeployedService } from 'app/services/product-deployed/product-deployed.service';

class MockProductDeployedService {

  constructor() {}

  isProductDeployed(_product: string): boolean {
    return false;
  }
}

class MockDesktopProductDeployedService {

  constructor() {}

  isProductDeployed(product: string): boolean {
    return product === 'desktop';
  }
}

describe('NavbarComponent', () => {
  let element: HTMLElement;
  let fixture: ComponentFixture<NavbarComponent>;

  describe('non-desktop view', () => {
    beforeEach(() => {
      TestBed.configureTestingModule({
        imports: [
          RouterTestingModule,
          StoreModule.forRoot({
            policies: policyEntityReducer
          }, {
            initialState: { policies: PolicyEntityInitialState },
            runtimeChecks
          })
        ],
        declarations: [
          NavbarComponent,
          MockComponent({ selector: 'app-profile' }),
          MockComponent({ selector: 'app-projects-filter' }),
          MockComponent({ selector: 'chef-icon' }),
          MockComponent({ selector: 'chef-tooltip' }),
          MockComponent({ selector: 'app-authorized',
                          inputs: ['allOf', 'anyOf'],
                          template: '<ng-content></ng-content>' })
        ],
        providers: [
          FeatureFlagsService,
          { provide: ProductDeployedService, useClass: MockProductDeployedService }
        ]
      });

      fixture = TestBed.createComponent(NavbarComponent);
      fixture.detectChanges();
      element = fixture.nativeElement;
    });

    it('displays the logo navigation link', () => {
      const logo = element.querySelector('.logo');
      expect(logo).not.toBeNull();
      expect(logo.getAttribute('href')).toBe('/');
    });

    using([
      ['Dashboards',     '/dashboards/event-feed',         1],
      ['Applications',   '/applications/service-groups',   2],
      ['Infrastructure', '/infrastructure',                3],
      ['Compliance',     '/compliance',                    4],
      ['Settings',       '/settings',                      5]
    ], function (label: string, path: string, position: number) {
      it(`displays the ${label} navigation link`, () => {
        const link = element.querySelector(`.navigation-menu > *:nth-child(${position}) a`);
        expect(link.textContent).toBe(label);
        expect(link.getAttribute('href')).toBe(path);
      });
    });

    it('displays the profile dropdown', () => {
      expect(element.querySelector('app-profile')).not.toBeNull();
    });

    it('displays the projects filter', () => {
      expect(element.querySelector('app-projects-filter')).not.toBeNull();
    });
  });

  describe('desktop view', () => {
    beforeEach(() => {
      TestBed.configureTestingModule({
        imports: [
          RouterTestingModule,
          StoreModule.forRoot({
            policies: policyEntityReducer
          }, {
            initialState: { policies: PolicyEntityInitialState },
            runtimeChecks
          })
        ],
        declarations: [
          NavbarComponent,
          MockComponent({ selector: 'app-profile' }),
          MockComponent({ selector: 'app-projects-filter' }),
          MockComponent({ selector: 'chef-icon' }),
          MockComponent({ selector: 'chef-tooltip' }),
          MockComponent({ selector: 'app-authorized',
                          inputs: ['allOf', 'anyOf'],
                          template: '<ng-content></ng-content>' })
        ],
        providers: [
          FeatureFlagsService,
          { provide: ProductDeployedService, useClass: MockDesktopProductDeployedService }
        ]
      });

      fixture = TestBed.createComponent(NavbarComponent);
      fixture.detectChanges();
      element = fixture.nativeElement;
    });

    using([
      ['Dashboards',     '/dashboards/event-feed',         1],
      ['Desktop',        '/desktop',                       2],
      ['Applications',   '/applications/service-groups',   3],
      ['Infrastructure', '/infrastructure',                4],
      ['Compliance',     '/compliance',                    5],
      ['Settings',       '/settings',                      6]
    ], function (label: string, path: string, position: number) {
      it(`displays the ${label} navigation link`, () => {
        const link = element.querySelector(`.navigation-menu > *:nth-child(${position}) a`);
        expect(link.textContent).toBe(label);
        expect(link.getAttribute('href')).toBe(path);
      });
    });
  });
});
