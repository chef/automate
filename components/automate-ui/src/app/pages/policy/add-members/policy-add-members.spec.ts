import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule, FormBuilder, Validators } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';

import { Regex } from 'app/helpers/auth/regex';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { customMatchers } from 'app/testing/custom-matchers';
import { policyEntityReducer } from 'app/entities/policies/policy.reducer';
import { runtimeChecks } from 'app/ngrx.reducers';
import { PolicyAddMembersComponent } from './policy-add-members.component';
import { using } from 'app/testing/spec-helpers';





describe('PolicyAddMembersComponent', () => {
    let component: PolicyAddMembersComponent;
    let fixture: ComponentFixture<PolicyAddMembersComponent>;

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
                }, { runtimeChecks })
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
        fixture.detectChanges();
    });

    it('should be created', () => {
        expect(component).toBeTruthy();
    });

    describe('showInputs', () => {

        it('should return false when inputName is not identity or name', () => {
            expect(component.showInputs('blank')).toBe(false);
        });

        using([
            ['user', true, 'identity and when typeValue equals user', 'identity'],
            ['team', true, 'identity and when typeValue equals team', 'identity'],
            ['*', false, 'identity and typeValue does not equal team or user', 'identity'],
            ['token', true, 'name and when typeValue equals token', 'name'],
            ['team', false, 'name + typeValue is not token + identityValue does not exist', 'name']
        ], function (type: string, outcome: boolean, description: string, input: string) {
            it(`should return ${outcome} when inputName is ${description}`, () => {
                component.expressionForm.setValue({ type: type, identity: '', name: '' });
                expect(component.showInputs(input)).toBe(outcome);
            });
        });

        using([
            ['something', true, 'name and when identityValue exists and isnt a star', 'name'],
            ['*', false, 'name and when identityValue exists and is a star', 'name']
        ], function(identity: string, outcome: boolean, description: string, input: string) {
            it(`should return ${outcome} when inputName is ${description}`, () => {
                component.expressionForm.setValue({ type: '', identity: identity, name: '' });
                expect(component.showInputs(input)).toBe(outcome);
            });
        });
    });
});
