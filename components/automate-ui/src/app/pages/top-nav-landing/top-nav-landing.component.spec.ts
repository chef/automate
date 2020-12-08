import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { TopNavLandingComponent } from './top-nav-landing.component';
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

describe('TopNavLandingComponent', () => {
  let component: TopNavLandingComponent;
  let fixture: ComponentFixture<TopNavLandingComponent>;

  describe('non Desktop view', () => {
    beforeEach(waitForAsync(() => {
      TestBed.configureTestingModule({
        imports: [
          StoreModule.forRoot(ngrxReducers, { runtimeChecks })
        ],
        providers: [
          FeatureFlagsService,
          { provide: ProductDeployedService, useClass: MockProductDeployedService }
        ],
        declarations: [
          TopNavLandingComponent,
          MockComponent({ selector: 'app-landing', inputs: ['routePerms', 'onNotFound'] })
        ]
      })
      .compileComponents();
    }));

    beforeEach(() => {
      fixture = TestBed.createComponent(TopNavLandingComponent);
      component = fixture.componentInstance;
      fixture.detectChanges();
    });

    it('should create', () => {
      expect(component).toBeTruthy();
    });

    it('order of start pages', () => {
      expect(component.routeList[0].route).toEqual('/dashboards/event-feed');
      expect(component.routeList[1].route).toEqual('/applications/service-groups');
      expect(component.routeList[2].route).toEqual('/infrastructure');
      expect(component.routeList[3].route).toEqual('/compliance');
      expect(component.routeList[4].route).toEqual('/settings');
    });
  });

  describe('Desktop view', () => {
    beforeEach(waitForAsync(() => {
      TestBed.configureTestingModule({
        imports: [
          StoreModule.forRoot(ngrxReducers, { runtimeChecks })
        ],
        providers: [
          FeatureFlagsService,
          { provide: ProductDeployedService, useClass: MockDesktopProductDeployedService }
        ],
        declarations: [
          TopNavLandingComponent,
          MockComponent({ selector: 'app-landing', inputs: ['routePerms', 'onNotFound'] })
        ]
      })
      .compileComponents();
    }));

    beforeEach(() => {
      fixture = TestBed.createComponent(TopNavLandingComponent);
      component = fixture.componentInstance;
      fixture.detectChanges();
    });

    it('should create', () => {
      expect(component).toBeTruthy();
    });

    it('order of start pages', () => {
      expect(component.routeList[0].route).toEqual('/dashboards/event-feed');
      expect(component.routeList[1].route).toEqual('/desktop');
      expect(component.routeList[2].route).toEqual('/applications/service-groups');
      expect(component.routeList[3].route).toEqual('/infrastructure');
      expect(component.routeList[4].route).toEqual('/compliance');
      expect(component.routeList[5].route).toEqual('/settings');
    });

  });
});
