import { ComponentFixture, TestBed } from '@angular/core/testing';
import { PolicyGroupDetailsComponent } from './policy-group-details.component';

describe('PolicyGroupDetailsComponent', () => {
  let component: PolicyGroupDetailsComponent;
  let fixture: ComponentFixture<PolicyGroupDetailsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ PolicyGroupDetailsComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(PolicyGroupDetailsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
