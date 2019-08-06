import { RouterTestingModule } from '@angular/router/testing';
import { ComponentFixture, TestBed } from '@angular/core/testing';
import { StoreModule } from '@ngrx/store';
import { of as observableOf } from 'rxjs';
import { NavbarComponent } from './navbar.component';
import { MockComponent } from 'ng2-mock-component';
import { using } from 'app/testing/spec-helpers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import {
  PolicyEntityInitialState,
  policyEntityReducer
} from 'app/entities/policies/policy.reducer';
import { IAMMajorVersion, IAMMinorVersion } from 'app/entities/policies/policy.model';

describe('NavbarComponent', () => {
  let component: NavbarComponent;
  let element: HTMLElement;
  let fixture: ComponentFixture<NavbarComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        StoreModule.forRoot({
          policies: policyEntityReducer
        }, {
          initialState: { policies: PolicyEntityInitialState }
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
        FeatureFlagsService
      ]
    });

    fixture = TestBed.createComponent(NavbarComponent);
    fixture.detectChanges();
    component = fixture.componentInstance;
    element = fixture.nativeElement;
  });

  it('displays the logo navigation link', () => {
    const logo = element.querySelector('.logo');
    expect(logo).not.toBeNull();
    expect(logo.getAttribute('href')).toBe('/');
  });

  using([
    ['Event Feed',     '/event-feed',                       1],
    ['Infrastructure', '/infrastructure/client-runs',       2],
    ['Compliance',     '/compliance',                       3],
    ['Settings',       '/settings',                         4]
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

  describe('when IAM v2.1 is enabled', () => {
    beforeEach(() => {
      component.iamMajorVersion$ = observableOf(<IAMMajorVersion>'v2');
      component.iamMinorVersion$ = observableOf(<IAMMinorVersion>'v1');
      fixture.detectChanges();
    });

    it('displays the projects filter', () => {
      expect(element.querySelector('app-projects-filter')).not.toBeNull();
    });
  });
});
