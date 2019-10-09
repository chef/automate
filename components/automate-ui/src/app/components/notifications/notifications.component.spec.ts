import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { ChefNotificationsComponent } from './notifications.component';

describe('ChefNotificationsComponent', () => {
  let component: ChefNotificationsComponent;
  let fixture: ComponentFixture<ChefNotificationsComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ ChefNotificationsComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ChefNotificationsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
