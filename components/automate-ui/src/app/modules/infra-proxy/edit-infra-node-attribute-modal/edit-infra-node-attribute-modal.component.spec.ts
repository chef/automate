import { ComponentFixture, TestBed } from '@angular/core/testing';

import { EditInfraNodeAttributeModalComponent } from './edit-infra-node-attribute-modal.component';

describe('EditInfraNodeAttributeModalComponent', () => {
  let component: EditInfraNodeAttributeModalComponent;
  let fixture: ComponentFixture<EditInfraNodeAttributeModalComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ EditInfraNodeAttributeModalComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(EditInfraNodeAttributeModalComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
