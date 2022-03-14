import { ComponentFixture, TestBed } from '@angular/core/testing';

import { OrgUsersComponent } from './org-users.component';

describe('OrgUsersComponent', () => {
  let component: OrgUsersComponent;
  let fixture: ComponentFixture<OrgUsersComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ OrgUsersComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(OrgUsersComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
