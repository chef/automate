import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { StoreModule } from '@ngrx/store';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { RouterTestingModule } from '@angular/router/testing';
import { IntegrationsListComponent } from './integrations-list.component';
import { managerEntityReducer } from '../../../entities/managers/manager.reducer';
import { ChefPipesModule } from '../../../pipes/chef-pipes.module';
import { MockComponent } from 'ng2-mock-component';

describe('IntegrationsComponent', () => {
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
          managers: managerEntityReducer
        })
      ],
      declarations: [
        MockComponent({ selector: 'app-admin-sidebar' }),
        IntegrationsListComponent
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
