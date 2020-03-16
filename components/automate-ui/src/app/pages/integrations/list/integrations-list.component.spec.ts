import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { RouterTestingModule } from '@angular/router/testing';
import { StoreModule } from '@ngrx/store';

import { runtimeChecks, ngrxReducers } from 'app/ngrx.reducers';
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
          ...ngrxReducers,
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
          })
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
