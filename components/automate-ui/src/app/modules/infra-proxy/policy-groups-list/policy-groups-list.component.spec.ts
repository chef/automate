import { ComponentFixture, TestBed } from '@angular/core/testing';
import { PolicyGroupsListComponent } from './policy-groups-list.component';

describe('PolicyGroupsListComponent', () => {
  let component: PolicyGroupsListComponent;
  let fixture: ComponentFixture<PolicyGroupsListComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ PolicyGroupsListComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(PolicyGroupsListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
