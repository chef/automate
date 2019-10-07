import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { runtimeChecks } from 'app/ngrx.reducers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { roleEntityReducer } from 'app/entities/roles/role.reducer';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';
import { RolesListComponent } from './roles-list.component';

describe('RolesListComponent', () => {
  let component: RolesListComponent;
  let fixture: ComponentFixture<RolesListComponent>;

  beforeEach(async(() => {

    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'app-authorized',
                        inputs: ['allOf'],
                        template: '<ng-content></ng-content>' }),
        MockComponent({ selector: 'app-settings-sidebar' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-table' }),
        MockComponent({ selector: 'chef-tbody' }),
        MockComponent({ selector: 'chef-td' }),
        MockComponent({ selector: 'chef-th' }),
        MockComponent({ selector: 'chef-thead' }),
        MockComponent({ selector: 'chef-tr' }),
        MockComponent({ selector: 'a', inputs: ['routerLink']}),
        RolesListComponent
      ],
      imports: [
        ChefPipesModule,
        StoreModule.forRoot({
          policies: policyEntityReducer,
          roles: roleEntityReducer
        }, { runtimeChecks })
      ],
      providers: [
        FeatureFlagsService
      ]
    }).compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(RolesListComponent);
    component = fixture.componentInstance;

    fixture.detectChanges();
  });

  it('should be created', () => {
    expect(component).toBeTruthy();
  });
});
