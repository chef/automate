import { TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { MockComponent } from 'ng2-mock-component';
import { using } from 'app/testing/spec-helpers';
import { ProfileSidebarComponent } from './profile-sidebar.component';

describe('ProfileSidebarComponent', () => {
  let fixture, component, element, link;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [RouterTestingModule
      ],
      declarations: [
        ProfileSidebarComponent,
        // Note: if we want to match contents, we cannot swallow them: thus we
        // need to provide a template here. <ng-content> mocks these as doing
        // nothing but a "pass-through" of what the components wrap.
        MockComponent({ selector: 'chef-sidebar',
                        template: '<ng-content></ng-content>' }),
        MockComponent({ selector: 'chef-sidebar-entry',
                        inputs: ['route', 'icon', 'exact'],
                        template: '<ng-content></ng-content>' })
      ]
    });

    fixture = TestBed.createComponent(ProfileSidebarComponent);
    element = fixture.debugElement;
    component = fixture.componentInstance;
  });

  it('should be created', () => {
    fixture = TestBed.createComponent(ProfileSidebarComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();

    expect(component).toBeTruthy();
  });

  using([
    ['Your Profile', null, 1]
  ], function (label: string, path: string, position: number) {
    it(`displays the ${label} navigation link`, () => {

      fixture.detectChanges();
      link = element.nativeElement
        .querySelector(`div.nav-items *:nth-child(${position})`);

      expect(link.innerText).toBe(label);
      expect(link.getAttribute('route')).toBe(path);
    });
  });
});
