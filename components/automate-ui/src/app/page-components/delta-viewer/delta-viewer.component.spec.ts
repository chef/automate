import { ComponentFixture, TestBed } from '@angular/core/testing';
import { DeltaViewerComponent } from './delta-viewer.component';

describe('DeltaViewerComponent', () => {
  let component: DeltaViewerComponent;
  let fixture: ComponentFixture<DeltaViewerComponent>;
  let delta, element;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        DeltaViewerComponent
      ]
    });

    fixture = TestBed.createComponent(DeltaViewerComponent);
    component = fixture.componentInstance;
    element = fixture.debugElement;
  });

  describe('when a delta is provided', () => {

    beforeEach(() => {
      delta = '--- /opt/something/important	2016-07-19 17:24:33.000000000 ' +
        '-0700\n+++ /opt/.chef-something/important20160725-80249-13d29yf	2016-07-25 ' +
        '13:42:17.000000000 -0700\n@@ -1,2 +1,2 @@\n-blah\n+blahblah';
    });

    it('renders it', () => {
      component.delta = delta;
      fixture.detectChanges();
      expect(element.nativeNode.innerHTML).toContain('something/important');
    });
  });
});
