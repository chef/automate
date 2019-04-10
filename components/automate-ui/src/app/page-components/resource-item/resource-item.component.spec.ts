import { ComponentFixture, TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA } from '@angular/core';
import { By } from '@angular/platform-browser';
import { NodeDetailsService } from '../../services/node-details/node-details.service';
import { ResourceItemComponent } from './resource-item.component';
import { DeltaViewerComponent } from '../delta-viewer/delta-viewer.component';
import { ChefStatusIconPipe } from '../../pipes/chef-status-icon.pipe';

describe('ResourceItemComponent', () => {
  let component: ResourceItemComponent;
  let fixture: ComponentFixture<ResourceItemComponent>;
  let resource, eventService, element;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [
      ],
      declarations: [
        DeltaViewerComponent,
        ResourceItemComponent,
        ChefStatusIconPipe
      ],
      providers: [
        NodeDetailsService
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(ResourceItemComponent);
    eventService = TestBed.get(NodeDetailsService);
    component = fixture.componentInstance;
    element = fixture.debugElement;

    resource = {
      type: 'file',
      status: 'updated',
      result: 'create',
      name: '/opt/something/important',
      cookbook_name: 'things',
      recipe_name: 'change',
      delta: null
    };
  });

  it ('shows a view-action placeholder', () => {
    component.resource = resource;
    fixture.detectChanges();

    const placeholder = element.query(By.css('.view-action .none'));
    expect(placeholder).not.toBeNull();
    expect(placeholder.nativeElement.innerText).toBe('- -');
  });

  describe('#toggleDelta()', () => {

    it('toggles the value of showDelta', () => {
      expect(component.showDelta).toBe(false);
      component.toggleDelta();
      expect(component.showDelta).toBe(true);
    });
  });

  describe('#showModal()', () => {

    it('calls the node-details event service', () => {
      spyOn(eventService, 'showModal');
      component.showModal();
      expect(eventService.showModal).toHaveBeenCalledWith(true);
    });
  });

  describe('when the resource has a diff', () => {

    beforeEach(() => {
      resource.delta = '--- /opt/something/important	2016-07-19 17:24:33.000000000 ' +
        '-0700\n+++ /opt/.chef-something/important20160725-80249-13d29yf	2016-07-25 ' +
        '13:42:17.000000000 -0700\n@@ -1,2 +1,2 @@\n-blah\n+blahblah';
    });

    it('shows a diff link', () => {
      component.resource = resource;
      fixture.detectChanges();

      const diffLink = element.query(By.css('.view-action .link span'));
      expect(diffLink).not.toBeNull();
      expect(diffLink.nativeElement.innerText).toBe('diff');
    });
  });

  describe('when the resource does not have a diff', () => {

    it('does not show a diff link', () => {
      component.resource = resource;
      fixture.detectChanges();
      expect(element.query(By.css('.view .link'))).toBeNull();
    });
  });

  describe('when the resource has failed', () => {

    beforeEach(() => {
      resource.status = 'failed';
    });

    it ('shows an error-log link', () => {
      component.resource = resource;
      fixture.detectChanges();

      const link = element.query(By.css('.view-action .link'));
      expect(link).not.toBeNull();
      expect(link.nativeElement.innerText).toBe('error log');
    });
  });
});
