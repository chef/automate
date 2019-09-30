import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MockComponent } from 'ng2-mock-component';

import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { customMatchers } from 'app/testing/custom-matchers';
import { PolicyAddMembersComponent } from './policy-add-members.component';

describe('PolicyListComponent', () => {
    let component: PolicyAddMembersComponent;
    let fixture: ComponentFixture<PolicyAddMembersComponent>;
    let element: HTMLElement;

    beforeEach(async(() => {
        TestBed.configureTestingModule({
            imports: [
                ChefPipesModule,
                FormsModule,
                ReactiveFormsModule
            ],
            declarations: [
                MockComponent({ selector: 'chef-page' }),
                MockComponent({ selector: 'chef-modal' }),
                MockComponent({ selector: 'chef-form-field' }),
                MockComponent({ selector: 'chef-error'}),
                MockComponent({ selector: 'chef-button'}),
                MockComponent({ selector: 'chef-input'}),
                MockComponent({ selector: 'chef-table' }),
                MockComponent({ selector: 'chef-thead' }),
                MockComponent({ selector: 'chef-tr' }),
                MockComponent({ selector: 'chef-th' }),
                MockComponent({ selector: 'chef-tbody' }),
                MockComponent({ selector: 'chef-td' }),
                PolicyAddMembersComponent
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
        expect(element).toContainPath('chef-modal');
    });

});
