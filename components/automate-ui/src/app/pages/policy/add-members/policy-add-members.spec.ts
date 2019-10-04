import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule, FormBuilder, Validators } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { Regex } from 'app/helpers/auth/regex';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { customMatchers } from 'app/testing/custom-matchers';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';
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
        component.expressionForm = new FormBuilder().group({
            type: ['', [Validators.required, Validators.pattern(Regex.patterns.NON_BLANK)]],
            identity: [''],
            name: ['']
        });
        element = fixture.debugElement.nativeElement;
        fixture.detectChanges();
    });

    it('should be created', () => {
        expect(component).toBeTruthy();
    });

    describe('showInputs', () => {

        it('should return false when inputName is not identity or name', () => {
            expect(component.showInputs('blank')).toBe(false);
        });

        it('should return true when inputName is identity and when typeValue equals USER', () => {
            component.expressionForm.setValue({type: 'USER', identity: '', name: ''});
            expect(component.showInputs('identity')).toBe(true);
        });

        it('should return true when inputName is identity and when typeValue equals TEAM', () => {
            component.expressionForm.setValue({ type: 'TEAM', identity: '', name: '' });
            expect(component.showInputs('identity')).toBe(true);
        });

        // tslint:disable-next-line: max-line-length
        it('should return false when inputName is identity and typeValue does not equal TEAM or USER', () => {
            component.expressionForm.setValue({ type: '*', identity: '', name: '' });
            expect(component.showInputs('identity')).toBe(false);
        });

        it('should return true when inputName is name and when typeValue equals TOKEN', () => {
            component.expressionForm.setValue({ type: 'TOKEN', identity: '', name: '' });
            expect(component.showInputs('name')).toBe(true);
        });

    });

});
