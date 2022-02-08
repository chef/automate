import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { RouterTestingModule } from '@angular/router/testing';
import { TestBed, ComponentFixture } from '@angular/core/testing';
import { ReactiveFormsModule } from '@angular/forms';
import { StoreModule, Store } from '@ngrx/store';
import { NgrxStateAtom, ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';
import { ChefPipesModule } from 'app/pipes/chef-pipes.module';
import { ChefSessionService } from 'app/services/chef-session/chef-session.service';
import { FeatureFlagsService } from 'app/services/feature-flags/feature-flags.service';
import { MockChefSessionService } from 'app/testing/mock-chef-session.service';
import { JobNodesFormComponent } from './job-nodes-form.component';
import { MockComponent } from 'ng2-mock-component';




describe('JobNodesFormComponent', () => {
  let fixture: ComponentFixture<JobNodesFormComponent>;
  let component: JobNodesFormComponent;
  let store: Store<NgrxStateAtom>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
        ChefPipesModule,
        ReactiveFormsModule,
        RouterTestingModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      declarations: [
        JobNodesFormComponent,
        MockComponent({ selector: 'chef-checkbox', inputs: ['checked'] })
      ],
      providers: [
        { provide: ChefSessionService, useClass: MockChefSessionService },
        FeatureFlagsService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });
    store = TestBed.inject(Store);
    spyOn(store, 'dispatch').and.callThrough();
    fixture = TestBed.createComponent(JobNodesFormComponent);
    component = fixture.componentInstance;
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('it should click the checkbox for aws', () => {
    component.searchval = jasmine.createSpyObj('searchval', ['nativeElement']);
    component.searchval.nativeElement = {value: ''};
    component.onclickCheckbox({target: {checked: true}}, 'aws');
    expect(component.nodeSource[0]).toEqual('aws-ec2');
    expect(component.nodeSource[1]).toEqual('aws-api');


  });

  it('it should click the checkbox for automate', () => {
    component.searchval = jasmine.createSpyObj('searchval', ['nativeElement']);
    component.searchval.nativeElement = {value: ''};
    component.onclickCheckbox({target: {checked: true}}, 'automate');
    expect(component.nodeSource[0]).toEqual('automate');
  });

  it('it should click the checkbox for gcp', () => {
    component.searchval = jasmine.createSpyObj('searchval', ['nativeElement']);
    component.searchval.nativeElement = {value: ''};
    component.onclickCheckbox({target: {checked: true}}, 'gcp');
    expect(component.nodeSource[0]).toEqual('gcp-api');
  });

  it('it should click the checkbox for azure', () => {
    component.searchval = jasmine.createSpyObj('searchval', ['nativeElement']);
    component.searchval.nativeElement = {value: ''};
    component.onclickCheckbox({target: {checked: true}}, 'azure');
    expect(component.nodeSource[0]).toEqual('azure-api');
    expect(component.nodeSource[1]).toEqual('azure-vm');
  });
});
