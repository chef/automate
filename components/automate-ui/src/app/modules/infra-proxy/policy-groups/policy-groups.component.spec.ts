import { ComponentFixture, TestBed } from '@angular/core/testing';

import { PolicyGroupsComponent } from './policy-groups.component';

describe('PolicyGroupsComponent', () => {
  let component: PolicyGroupsComponent;
  let fixture: ComponentFixture<PolicyGroupsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ PolicyGroupsComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(PolicyGroupsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
