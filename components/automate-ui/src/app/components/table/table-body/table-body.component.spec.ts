import { waitForAsync, ComponentFixture, TestBed } from '@angular/core/testing';

import { TableBodyComponent } from './table-body.component';

describe('TableBodyComponent', () => {
  let component: TableBodyComponent;
  let fixture: ComponentFixture<TableBodyComponent>;

  beforeEach(waitForAsync(() => {
    TestBed.configureTestingModule({
      declarations: [ TableBodyComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(TableBodyComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
