import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { StoreModule } from '@ngrx/store';
import { ReactiveFormsModule } from '@angular/forms';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { RouterTestingModule } from '@angular/router/testing';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { IntegrationsAddComponent } from './integrations-add.component';
import { runtimeChecks } from 'app/ngrx.reducers';
import { managerEntityReducer } from '../../../entities/managers/manager.reducer';
import { integrationsAddReducer } from './integration-add.reducer';

describe('IntegrationsAddComponent', () => {
  let component: IntegrationsAddComponent;
  let fixture: ComponentFixture<IntegrationsAddComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [
        RouterTestingModule,
        ReactiveFormsModule,
        StoreModule.forRoot({
          managers: managerEntityReducer,
          integrations_add: integrationsAddReducer
        }, { runtimeChecks })
      ],
      providers: [
        FeatureFlagsService
      ],
      declarations: [
        IntegrationsAddComponent
      ],
      schemas: [
        CUSTOM_ELEMENTS_SCHEMA
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(IntegrationsAddComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('exists', () => {
    expect(component).toBeTruthy();
  });
});
