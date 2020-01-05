import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { RouterTestingModule } from '@angular/router/testing';
import { StoreModule } from '@ngrx/store';

import { runtimeChecks } from 'app/ngrx.reducers';
import { notificationEntityReducer } from 'app/entities/notifications/notification.reducer';
import { clientRunsEntityReducer } from 'app/entities/client-runs/client-runs.reducer';
import { managerEntityReducer } from '../../../entities/managers/manager.reducer';
import { ChefPipesModule } from '../../../pipes/chef-pipes.module';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { IntegrationsListComponent } from './integrations-list.component';

describe('IntegrationsListComponent', () => {
  let component: IntegrationsListComponent;
  let fixture: ComponentFixture<IntegrationsListComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        ChefPipesModule,
        StoreModule.forRoot({
          router: () => ({
            state: {
              url: '/',
              queryParams: {},
              params: {},
              fragment: '',
              path: ['/']
            },
            previousRoute: {},
            navigationId: 0
          }),
          managers: managerEntityReducer,
          notifications: notificationEntityReducer, // not used here but needed to suppress warnings
          clientRunsEntity: clientRunsEntityReducer // not used here but needed to suppress warnings
        }, { runtimeChecks })
      ],
      declarations: [
        IntegrationsListComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      schemas: [
        CUSTOM_ELEMENTS_SCHEMA
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(IntegrationsListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('exists', () => {
    expect(component).toBeTruthy();
  });
});
