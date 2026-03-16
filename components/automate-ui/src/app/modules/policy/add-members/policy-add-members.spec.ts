import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { ReactiveFormsModule, FormBuilder, Validators } from '@angular/forms';
import { StoreModule } from '@ngrx/store';
import { MockComponent } from 'ng2-mock-component';
import { MockChefButton, MockChefCheckbox, MockChefError, MockChefFormField, MockChefInput, MockChefModal, MockChefPage, MockChefTable, MockChefTbody, MockChefTd, MockChefTh, MockChefThead, MockChefTr } from 'app/testing/mock-components';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { runtimeChecks } from 'app/ngrx.reducers';
import { PolicyAddMembersComponent, FieldName } from './policy-add-members.component';
import { using } from 'app/testing/spec-helpers';
import { TelemetryService } from 'app/services/telemetry/telemetry.service';

class MockTelemetryService {
  track() { }
}

describe('PolicyAddMembersComponent', () => {
    let component: PolicyAddMembersComponent;
    let fixture: ComponentFixture<PolicyAddMembersComponent>;

    beforeEach(waitForAsync(() => {
        TestBed.configureTestingModule({
            declarations: [
                PolicyAddMembersComponent
            ],
            imports: [
                MockChefPage,
                MockChefModal,
                MockChefFormField,
                MockChefError,
                MockChefButton,
                MockChefInput,
                MockChefTable,
                MockChefThead,
                MockChefTr,
                MockChefTh,
                MockChefTbody,
                MockChefTd,
                MockChefCheckbox,
                MockComponent({ selector: 'a', inputs: ['routerLink'] }),
                ChefPipesModule,
                ReactiveFormsModule,
                RouterTestingModule,
                StoreModule.forRoot({
                    router: () => ({
                        state: {
                            url: '/settings/policies/editor-access/add-members',
                            params: {}
                        }
                    })
                }, { runtimeChecks })
            ],
            providers: [
              { provide: TelemetryService, useClass: MockTelemetryService }
            ]
        }).compileComponents();
    }));

    beforeEach(() => {
        fixture = TestBed.createComponent(PolicyAddMembersComponent);
        component = fixture.componentInstance;
        component.expressionForm = new FormBuilder().group({
            type: ['', Validators.required],
            identity: '',
            name: ''
        });
    });

    it('should be created', () => {
        expect(component).toBeTruthy();
    });

    describe('updateFormDisplay', () => {

        using([
            [{ type: 'team', identity: null, name: null }, 'type', 'team'],
            [{ type: '*', identity: null, name: null}, 'type', '*'],
            [{ type: 'team', identity: 'saml', name: null}, 'identity', 'team:saml'],
            [{ type: 'team', identity: 'saml', name: '*'}, 'identity', 'team:saml:*'],
            [{ type: 'user', identity: 'local', name: null}, 'identity', 'user:local'],
            [{ type: 'user', identity: 'ldap', name: 'square'}, 'name', 'user:ldap:square'],
            [{ type: 'token', identity: null, name: 'whatever'}, 'name', 'token:whatever'],
            [{ type: 'token', identity: null, name: 'something'}, 'name', 'token:something'],
            [{ type: 'user', identity: null, name: '*'}, 'name', 'user:*'],
            [{ type: 'team', identity: null, name: '*'}, 'name', 'team:*'],
            [{ type: 'token', identity: null, name: '*'}, 'name', 'token:*']
        ], function (formValues: {}, inputName: FieldName, output: string) {
            it(`sets expressionOutput to ${output} when formValues are ${formValues} and inputName is ${inputName}`, () => {
                component.expressionForm.setValue(formValues);
                component.updateFormDisplay(inputName);
                expect(component.expressionOutput).toBe(output);
            });
        });
    });
});
