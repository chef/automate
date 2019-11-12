import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { OrgCreateEditModalComponent } from './org-create-edit-modal.component';

describe('OrgCreateEditModalComponent', () => {
  let component: OrgCreateEditModalComponent;
  let fixture: ComponentFixture<OrgCreateEditModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ OrgCreateEditModalComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(OrgCreateEditModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
