import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { ResetAdminKeyComponent } from './reset-admin-key.component';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';
import { MockChefButton, MockChefFormField, MockChefHeading, MockChefIcon, MockChefLoadingSpinner, MockChefPageHeader, MockChefSubheading, MockChefToolbar } from 'app/testing/mock-components';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';

describe('ResetAdminKeyComponent', () => {
  let component: ResetAdminKeyComponent;
  let fixture: ComponentFixture<ResetAdminKeyComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [
        ResetAdminKeyComponent
      ],
      providers: [
        FeatureFlagsService
      ],
      imports: [
        MockComponent({ selector: 'a', inputs: ['routerLink'] }),
        MockChefButton,
        MockChefHeading,
        MockChefFormField,
        MockChefIcon,
        MockChefLoadingSpinner,
        MockChefPageHeader,
        MockChefSubheading,
        MockChefToolbar,
        MockComponent({ selector: 'input', inputs: ['resetOrigin'] }),
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
