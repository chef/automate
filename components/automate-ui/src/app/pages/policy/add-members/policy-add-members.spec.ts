import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
// import { of as observableOf } from 'rxjs';
import { MockComponent } from 'ng2-mock-component';

// import { NgrxStateAtom } from 'app/ngrx.reducers';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';

import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { customMatchers } from 'app/testing/custom-matchers';
import { PolicyAddMembersComponent } from './policy-add-members.component';





describe('PolicyAddMembersComponent', () => {
    let component: PolicyAddMembersComponent;
    let fixture: ComponentFixture<PolicyAddMembersComponent>;
    let element: HTMLElement;

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            declarations: [
                MockComponent({ selector: 'chef-page' }),
                MockComponent({ selector: 'chef-modal', inputs: ['visible'] }),
                MockComponent({ selector: 'chef-form-field' }),
                MockComponent({ selector: 'chef-error'}),
                MockComponent({ selector: 'chef-button', inputs: ['disabled'] }),
                MockComponent({ selector: 'chef-input'}),
                MockComponent({ selector: 'chef-table' }),
                MockComponent({ selector: 'chef-thead' }),
                MockComponent({ selector: 'chef-tr' }),
                MockComponent({ selector: 'chef-th' }),
                MockComponent({ selector: 'chef-tbody' }),
                MockComponent({ selector: 'chef-td' }),
                MockComponent({ selector: 'chef-checkbox', inputs: ['checked'] }),
                MockComponent({ selector: 'a', inputs: ['routerLink'] }),
                PolicyAddMembersComponent
            ],
            imports: [
                ChefPipesModule,
                FormsModule,
                ReactiveFormsModule,
                RouterTestingModule,
                StoreModule.forRoot({
                    policies: policyEntityReducer
                })
            ]
        }).compileComponents();
    }));

    beforeEach(() => {
        jasmine.addMatchers(customMatchers);
        fixture = TestBed.createComponent(PolicyAddMembersComponent);
        component = fixture.componentInstance;
        element = fixture.debugElement.nativeElement;
        fixture.detectChanges();
    });

    it('should be created', () => {
        expect(component).toBeTruthy();
    });

    it('contains key elements', () => {
        component.loading = false;
        component.modalVisible = true;
        component.sortedMembersAvailable = [];
        fixture.detectChanges();
        expect(element).toContainPath('chef-page');
        // expect(thisElement).toContainPath('chef-page');
    });

});
