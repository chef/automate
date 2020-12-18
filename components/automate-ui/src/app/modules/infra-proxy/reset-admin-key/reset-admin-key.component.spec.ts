import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { ResetAdminKeyComponent } from './reset-admin-key.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';

describe('ResetAdminKeyComponent', () => {
  let component: ResetAdminKeyComponent;
  let fixture: ComponentFixture<ResetAdminKeyComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
        MockComponent({ selector: 'chef-heading' }),
        MockComponent({ selector: 'chef-form-field' }),
        MockComponent({ selector: 'chef-icon' }),
        MockComponent({ selector: 'chef-loading-spinner' }),
        MockComponent({ selector: 'chef-page-header' }),
        MockComponent({ selector: 'chef-subheading' }),
        MockComponent({ selector: 'chef-toolbar' }),
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
        ResetAdminKeyComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      imports: [
        FormsModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ResetAdminKeyComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('resets  admin_key to empty string', () => {
    component.saveSuccessful = true;
    expect(component.resetKeyForm.controls['admin_key'].value).toEqual('');
  });
});
