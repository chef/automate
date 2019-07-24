import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { CreateV1TeamModalComponent } from './create-v1-team-modal.component';

describe('CreateV1TeamModalComponent', () => {
  let component: CreateV1TeamModalComponent;
  let fixture: ComponentFixture<CreateV1TeamModalComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ CreateV1TeamModalComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateV1TeamModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
