import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { NO_ERRORS_SCHEMA, CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, defaultInitialState, runtimeChecks } from 'app/ngrx.reducers';

import { SidebarComponent } from './sidebar.component';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { LayoutFacadeService, Sidebar } from 'app/entities/layout/layout.facade';
import { SettingsLandingComponent } from 'app/pages/settings-landing/settings-landing.component';
import { using } from 'app/testing/spec-helpers';
import { ComplianceLandingComponent } from 'app/pages/compliance-landing/compliance-landing.component';

describe('SidebarComponent', () => {
  let store: Store<NgrxStateAtom>;
  let component: SidebarComponent;
  let fixture: ComponentFixture<SidebarComponent>;
  // let element: HTMLElement;
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
    layoutFacade = TestBed.inject(LayoutFacadeService);
    featureFlags = TestBed.inject(FeatureFlagsService);
    spyOn(store, 'dispatch').and.callThrough();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(SidebarComponent);
    component = fixture.componentInstance;
    // element = fixture.debugElement.nativeElement;

    // enable all feature flags, if any, for testing
    featureFlags.setFeature('servicenow_cmdb', true);

    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });

  using([
    [new SettingsLandingComponent(), Sidebar.Settings, 'Settings Landing Component'],
    [new ComplianceLandingComponent(), Sidebar.Compliance, 'Compliance Landing Component']

  ], function (_landingComponent: any, sidebar: Sidebar,  description: string) {
    describe(`${description} route list`, () => {

      beforeEach(() => {
        layoutFacade.showSidebar(sidebar);
        fixture.detectChanges();
      });

      // FIXME(tc): Sometimes missing elements. Needs investigation.
      // it('has length consistent with sidebar', () => {
      //   const links = element.querySelectorAll('div.nav-items chef-sidebar-entry');
      //   expect(links.length).toBe(landingComponent.routeList.length);
      // });

      // FIXME(sr): This test randomly fails, insofar as the elements are all there,
      //            but in the wrong order.
      // it('has route order consistent with sidebar', () => {
      //   const links = element.querySelectorAll('div.nav-items chef-sidebar-entry');
      //   for (let i = 0; i < links.length; i++) {
      //     const link: any = links[i];
      //     expect(link.route).toBe(landingComponent.routeList[i].route);
      //   }
      // });
    });
  });
});
