import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { Store, StoreModule } from '@ngrx/store';
import { NgrxStateAtom, runtimeChecks } from 'app/ngrx.reducers';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { CookieModule } from 'ngx-cookie';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { MockChefSessionService } from 'app/testing/mock-chef-session.service';
import { NodesAddComponent } from './nodes-add.component';
import * as fromClientRuns from 'app/entities/client-runs/client-runs.reducer';
import * as fromNotifications from 'app/entities/notifications/notification.reducer';
import * as fromLayout from 'app/entities/layout/layout.reducer';

describe('NodesAddComponent', () => {
  let store: Store<NgrxStateAtom>;
  let fixture: ComponentFixture<NodesAddComponent>;
  let component: NodesAddComponent;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        FormsModule,
        ReactiveFormsModule,
        CookieModule.forRoot(),
        HttpClientTestingModule,
        StoreModule.forRoot({
          clientRunsEntity: fromClientRuns.clientRunsEntityReducer,
          notifications: fromNotifications.notificationEntityReducer,
          layout: fromLayout.layoutEntityReducer
        }, { runtimeChecks })
      ],
      declarations: [
        NodesAddComponent
      ],
      providers: [
        { provide: ChefSessionService, useClass: MockChefSessionService },
        FeatureFlagsService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });
    store = TestBed.get(Store);
    spyOn(store, 'dispatch').and.callThrough();
    fixture = TestBed.createComponent(NodesAddComponent);
    component = fixture.componentInstance;
  });

  beforeEach(() => {
    component.ngOnInit();
  });

  describe('navToStep()', () => {
    it('sets activeStep to provided number', () => {
      component.activeStep = 1;
      component.navToStep(2);
      expect(component.activeStep).toEqual(2);
    });
  });

  describe('stepIsActive()', () => {
    it('returns boolean for active state of provided step', () => {
      component.activeStep = 1;
      expect(component.stepIsActive(1)).toEqual(true);
      expect(component.stepIsActive(2)).toEqual(false);
    });
  });

  describe('stepIsValid()', () => {
    it('returns boolean for form group validity of provided step', () => {
      expect(component.stepIsValid(1))
        .toEqual(component.form.controls['wizardStep1'].valid);
    });
  });
});
