import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { async, ComponentFixture, TestBed } from '@angular/core/testing';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { MockComponent } from 'ng2-mock-component';
import { using } from 'app/testing/spec-helpers';

import { ChefNotificationComponent } from './notification.component';
import { Type } from 'app/entities/notifications/notification.model';

describe('ChefNotificationComponent', () => {
  let component: ChefNotificationComponent;
  let fixture: ComponentFixture<ChefNotificationComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      imports: [ BrowserAnimationsModule ],
      declarations: [
        ChefNotificationComponent,
        MockComponent({ selector: 'chef-icon' })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    })
      .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(ChefNotificationComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  const genNotification = (type: Type, timeout: number) => {
    component.type = type;
    component.timeout = timeout;
  };

  describe('generates a notification', () => {

    it('defaults to type of info and timeout of 8', () => {
      expect(component.type).toBe('info');
      expect(component.timeout).toEqual(8);
    });

    using([
      [Type.info, 10],
      [Type.info, 7],
      [Type.info, 4],
      [Type.info, 2],
      [Type.info, 0],
      [Type.error, 43],
      [Type.error, 13],
      [Type.error, 3],
      [Type.error, 23]
    ], function (type: Type, timeout: number) {
          it('reads type and timeout from inputs', () => {
            genNotification(type, timeout);

            expect(component.type).toBe(type);
            expect(component.timeout).toEqual(timeout);
          });
        });

  });
});
