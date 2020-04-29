import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { async, ComponentFixture, TestBed, tick, fakeAsync, flush } from '@angular/core/testing';
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

    it('and defaults to type of "info" and timeout of 8', () => {
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
          it('and reads type and timeout from inputs', () => {
            genNotification(type, timeout);

            expect(component.type).toBe(type);
            expect(component.timeout).toEqual(timeout);
          });
        });
  });

  describe('ngAfterViewInit', () => {

    using([
      [Type.info, 10, false],
      [Type.info, 0, true],
      [Type.info, 4, false],
      [Type.info, 2, false],
      [Type.info, 0, true],
      [Type.error, 43, false],
      [Type.error, 0, true],
      [Type.error, 7, false]
    ], function (type: Type, timeout: number, isInfinite: boolean) {
      it('determines notification timeout is infinite when timeout equals 0', () => {
        genNotification(type, timeout);
        component.ngAfterViewInit();

        expect(component.isInfinite).toBe(isInfinite);
      });
    });

    using([
      [Type.info, 1, false],
      [Type.info, 2, false],
      [Type.error, 3, false]
    ], function (type: Type, timeout: number, isInfinite: boolean) {
      it('notification should close when infinite', fakeAsync(() => {
        spyOn(component, 'handleClose');
        genNotification(type, timeout);
        component.ngAfterViewInit();

        expect(component.isInfinite).toBe(isInfinite);
        tick((timeout * 1000) + 1);
        expect(component.handleClose).toHaveBeenCalled();

        fixture.destroy();
        flush();
      }));
    });

    using([
      [Type.info, 0, true],
      [Type.info, 0, true],
      [Type.error, 0, true]
    ], function (type: Type, timeout: number, isInfinite: boolean) {
      it('notification does not close when infinite', fakeAsync(() => {
        spyOn(component, 'handleClose');
        genNotification(type, timeout);
        component.ngAfterViewInit();

        expect(component.isInfinite).toBe(isInfinite);
        tick(500);
        expect(component.handleClose).not.toHaveBeenCalled();

        fixture.destroy();
        flush();
      }));
    });
  });

  describe('clicking the close button', () => {
    it('calls the handle close function', fakeAsync(() => {
      spyOn(component, 'handleClose');
      genNotification(Type.info, 2);
      component.ngAfterViewInit();

      const closeButton = fixture.debugElement.nativeElement.querySelector('chef-icon');
      closeButton.click();
      tick(500);
      expect(component.handleClose).toHaveBeenCalled();

      fixture.destroy();
      flush();
    }));
  });
});
