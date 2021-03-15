import { ComponentFixture, TestBed } from '@angular/core/testing';

import { DatabagItemModalComponent } from './databag-item-modal.component';

describe('DatabagItemModalComponent', () => {
  let component: DatabagItemModalComponent;
  let fixture: ComponentFixture<DatabagItemModalComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ DatabagItemModalComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(DatabagItemModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
