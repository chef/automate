import { CUSTOM_ELEMENTS_SCHEMA, SimpleChange } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ProjectsDropdownComponent } from './projects-dropdown.component';
import { StoreModule } from '@ngrx/store';
import { ngrxReducers, runtimeChecks } from 'app/ngrx.reducers';

describe('ProjectsDropdownComponent', () => {
  let component: ProjectsDropdownComponent;
  let fixture: ComponentFixture<ProjectsDropdownComponent>;

  beforeEach(async(() => {
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


});
