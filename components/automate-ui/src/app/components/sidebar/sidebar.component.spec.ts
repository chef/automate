import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { NO_ERRORS_SCHEMA, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { StoreModule, Store } from '@ngrx/store';

import { NgrxStateAtom, ngrxReducers, defaultInitialState, runtimeChecks } from 'app/ngrx.reducers';
import { using } from 'app/testing/spec-helpers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { SettingsLandingComponent } from 'app/pages/settings-landing/settings-landing.component';
import { ComplianceLandingComponent } from 'app/pages/compliance-landing/compliance-landing.component';
import { SidebarEntryComponent } from 'app/components/sidebar-entry/sidebar-entry.component';
import { SidebarComponent } from './sidebar.component';

describe('SidebarComponent', () => {
  let store: Store<NgrxStateAtom>;
  let component: SidebarComponent;
  let fixture: ComponentFixture<SidebarComponent>;
  let element: HTMLElement;
  let layoutFacade: LayoutFacadeService;
  let featureFlags: FeatureFlagsService;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [SidebarComponent],
      schemas: [
        NO_ERRORS_SCHEMA,
        CUSTOM_ELEMENTS_SCHEMA
      ],
      providers: [
        FeatureFlagsService,
        LayoutFacadeService
      ],
      imports: [
        StoreModule.forRoot(ngrxReducers, { initialState: defaultInitialState, runtimeChecks })
      ]
    }).compileComponents();
    store = TestBed.inject(Store);
    featureFlags = TestBed.inject(FeatureFlagsService);
    spyOn(store, 'dispatch').and.callThrough();
  }));

  describe('component is created', () => {

    beforeEach(() => {
      fixture = TestBed.createComponent(SidebarComponent);
      component = fixture.componentInstance;
    });

    it('should be created', () => {
      expect(component).toBeTruthy();
    });
  });

  // Only need to check components that copy the route structure defined by LayoutSidebarService.
  // The unit tests here are a meta-check to ensure that if you make a change in one place,
  // you must also make the same change in the copy.
  using([
    [new SettingsLandingComponent(), Sidebar.Settings, 'servicenow_cmdb', 'Settings Landing Component'],
    [new ComplianceLandingComponent(), Sidebar.Compliance, '', 'Compliance Landing Component']

  ], function (landingComponent: any, sidebar: Sidebar,  featureFlag: string, description: string) {
    describe(`${description} route list`, () => {

      beforeEach(() => {
        if (featureFlag) {
          featureFlags.setFeature(featureFlag, true);
        }
        // must be after feature flag setting!
        layoutFacade = TestBed.inject(LayoutFacadeService);

        fixture = TestBed.createComponent(SidebarComponent);
        element = fixture.debugElement.nativeElement;

        layoutFacade.showSidebar(sidebar);
        fixture.detectChanges();
      });

      it('has route order consistent with sidebar', () => {
        const links = element.querySelectorAll('div.nav-items chef-sidebar-entry');
        expect(links.length).toBe(landingComponent.routeList.length);
        for (let i = 0; i < links.length; i++) {
          // link is both an Element and a SidebarEntryComponent, but we only care about the latter
          const link = links[i] as unknown as SidebarEntryComponent;
          expect(link.route).toBe(landingComponent.routeList[i].route);
        }
      });
    });
  });
});
