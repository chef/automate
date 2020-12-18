import { CUSTOM_ELEMENTS_SCHEMA, SimpleChange } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';
import { StoreModule, Store } from '@ngrx/store';

import { ngrxReducers, runtimeChecks, NgrxStateAtom } from 'app/ngrx.reducers';
import { LoadOptionsSuccess } from 'app/services/projects-filter/projects-filter.actions';
import { ProjectsFilterOptionTuple } from 'app/services/projects-filter/projects-filter.reducer';
import { ProjectsDropdownComponent } from './projects-dropdown.component';

describe('ProjectsDropdownComponent', () => {
  let component: ProjectsDropdownComponent;
  let fixture: ComponentFixture<ProjectsDropdownComponent>;
  let store: Store<NgrxStateAtom>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ ProjectsDropdownComponent ],
      imports: [
        FormsModule,
        StoreModule.forRoot(ngrxReducers, { runtimeChecks })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ProjectsDropdownComponent);
    component = fixture.componentInstance;
    store = TestBed.inject(Store);
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('upon opening, checked status of all projects is set to false', () => {
    component.projects = [
      {
        itemList: [
          { id: 'proj1', name: 'proj1', checked: true },
          { id: 'proj3', name: 'proj3', checked: false },
          { id: 'proj2', name: 'proj2', checked: true }
        ]
      }];

    component.ngOnChanges(
      { checkedProjectIDs: new SimpleChange([], [], true) });

    component.projects.forEach(p =>
      p.itemList.forEach(i => expect(i.checked).toBe(false)));
  });

  it('receives projects in sorted order', () => {
    const payload: ProjectsFilterOptionTuple = {
      fetched: [
        { label: 'zz-proj', checked: true, value: '', type: '' },
        { label: 'c-proj', checked: true, value: '', type: '' },
        { label: 'a-proj', checked: true, value: '', type: '' },
        { label: 'b-proj', checked: true, value: '', type: '' }
      ],
      restored: []
    };
    const expectedNames = payload.fetched.map(p => p.label);

    store.dispatch(new LoadOptionsSuccess(payload));

    expect(component.projects[0].itemList.map(p => p.name))
      .toEqual([
        expectedNames[2],
        expectedNames[3],
        expectedNames[1],
        expectedNames[0]
      ]);
  });
});
