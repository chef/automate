import { RouterTestingModule } from '@angular/router/testing';
import { TestBed } from '@angular/core/testing';
import { NavbarComponent } from './navbar.component';
import { MockComponent } from 'ng2-mock-component';
import { using } from 'app/testing/spec-helpers';
import { FeatureFlagsService } from '../../../app/services/feature-flags/feature-flags.service';

describe('NavbarComponent', () => {
  let fixture, element;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule
      ],
      declarations: [
        NavbarComponent,
        MockComponent({ selector: 'app-profile' }),
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
    element = fixture.debugElement;
  });

  it('displays the logo navigation link', () => {
    const logo = element.nativeElement.querySelector('.logo');
    fixture.detectChanges();

    expect(logo).not.toBeNull();
    expect(logo.getAttribute('href')).toBe('/');
  });

  using([
    ['Event Feed',  '/event-feed',           1],
    ['Client Runs', '/client-runs',          2],
    ['Compliance',  '/compliance/reporting', 3],
    ['Scan Jobs',   '/compliance/scanner',   4],
    ['Asset Store', '/profiles',             5],
    ['Settings',    '/settings',             6]
  ], function (label: string, path: string, position: number) {
    it(`displays the ${label} navigation link`, () => {
      const link = element.nativeElement
        .querySelector(`.navigation-menu > *:nth-child(${position}) a`);
      fixture.detectChanges();

      expect(link.innerText).toBe(label);
      expect(link.getAttribute('href')).toBe(path);
    });
  });

  it('should display the profile dropdown', () => {
    element = fixture.nativeElement;
    fixture.detectChanges();

    expect(element.querySelector('app-profile')).not.toBeNull();
  });
});
