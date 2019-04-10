import { TestBed } from '@angular/core/testing';
import { Observable, Observer } from 'rxjs';
import { By } from '@angular/platform-browser';
import { MockComponent } from 'ng2-mock-component';
import { SidebarSelectListComponent } from './sidebar-select-list.component';
import { SelectListItemComponent } from '../select-list-item/select-list-item.component';
import { SelectListInputPipe } from '../../pipes/select-list-input.pipe';

class MockEvent {
  stopPropagation() {}
}

describe('SidebarSelectListComponent', () => {
  let fixture, component, element;

    beforeEach(() => {
      TestBed.configureTestingModule({
        declarations: [
          SidebarSelectListComponent,
          SelectListInputPipe,
          SelectListItemComponent,
          MockComponent({ selector: 'chef-icon' })
        ]
      });

      fixture = TestBed.createComponent(SidebarSelectListComponent);
      component = fixture.componentInstance;
      element = fixture.debugElement;
    });

    it('interprets the label correctly', () => {
      component.label = 'Servers';

      component.allItemsObs = Observable.create((observer: Observer<Array<string>>) => {
        observer.next([]);
        observer.complete();
      });

      fixture.detectChanges();
      expect(element.query(By.css('li')).nativeElement.innerText).toBe('All Chef Servers (0)');
    });

    it('no items are selected', () => {
      component.allItemsObs = Observable.create((observer: Observer<Array<string>>) => {
        observer.next(['Ice Cream', 'Cookies', 'Cake']);
        observer.complete();
      });

      component.selectedItemsObs = Observable.create((observer: Observer<Array<string>>) => {
        observer.next([]);
        observer.complete();
      });

      fixture.detectChanges();

      component.allItems.forEach( item => {
        expect(component.isSelected(item)).toBe(false);
      });
    });

    it('some items are selected', () => {
      component.allItemsObs = Observable.create((observer: Observer<Array<string>>) => {
        observer.next(['Ice Cream', 'Cookies', 'Cake']);
        observer.complete();
      });

      component.selectedItemsObs = Observable.create((observer: Observer<Array<string>>) => {
        observer.next(['Ice Cream', 'Cookies']);
        observer.complete();
      });

      fixture.detectChanges();

      expect(component.isSelected('Ice Cream')).toBe(true);
      expect(component.isSelected('Cookies')).toBe(true);
      expect(component.isSelected('Cake')).toBe(false);
    });

    describe('remove selection', () => {
      it('should remove the Cookies from being selected', (done) => {
        component.allItemsObs = Observable.create((observer: Observer<Array<string>>) => {
          observer.next(['Ice Cream', 'Cookies', 'Cake']);
          observer.complete();
        });

        component.selectedItemsObs = Observable.create((observer: Observer<Array<string>>) => {
          observer.next(['Ice Cream', 'Cookies']);
          observer.complete();
        });

        fixture.detectChanges();

        component.selected.subscribe(selectedItems => {
          expect(['Ice Cream']).toEqual(selectedItems);
          done(); // ensure this is called
        });

        component.updateSelection(new MockEvent(), 'Cookies');
      });

      it('should remove Cake and leave Ice Cream and Cookies', (done) => {
        component.allItemsObs = Observable.create((observer: Observer<Array<string>>) => {
          observer.next(['Ice Cream', 'Cookies', 'Cake']);
          observer.complete();
        });

        component.selectedItemsObs = Observable.create((observer: Observer<Array<string>>) => {
          observer.next(['Ice Cream', 'Cookies', 'Cake']);
          observer.complete();
        });

        fixture.detectChanges();

        component.selected.subscribe(selectedItems => {
          expect(selectedItems.length).toBe(2);
          expect(selectedItems.includes('Ice Cream')).toBe(true);
          expect(selectedItems.includes('Cookies')).toBe(true);

          done(); // ensure this is called
        });

        component.updateSelection(new MockEvent(), 'Cake');
      });

      it('should remove Cake and have an empty selection', (done) => {
        component.allItemsObs = Observable.create((observer: Observer<Array<string>>) => {
          observer.next(['Ice Cream', 'Cookies', 'Cake']);
          observer.complete();
        });

        component.selectedItemsObs = Observable.create((observer: Observer<Array<string>>) => {
          observer.next(['Cake']);
          observer.complete();
        });

        fixture.detectChanges();

        component.selected.subscribe(selectedItems => {
          expect(selectedItems.length).toBe(0);

          done(); // ensure this is called
        });

        component.updateSelection(new MockEvent(), 'Cake');
      });
    });

    describe('add selection', () => {
      it('should add Ice Cream to selections', (done) => {
        component.allItemsObs = Observable.create((observer: Observer<Array<string>>) => {
          observer.next(['Ice Cream', 'Cookies', 'Cake']);
          observer.complete();
        });

        component.selectedItemsObs = Observable.create((observer: Observer<Array<string>>) => {
          observer.next([]);
          observer.complete();
        });

        fixture.detectChanges();

        component.selected.subscribe(selectedItems => {
          expect(['Ice Cream']).toEqual(selectedItems);
          done(); // ensure this is called
        });

        component.updateSelection(new MockEvent(), 'Ice Cream');
      });

      it('should add Cookies to selections with Ice Cream', (done) => {
        component.allItemsObs = Observable.create((observer: Observer<Array<string>>) => {
          observer.next(['Ice Cream', 'Cookies', 'Cake']);
          observer.complete();
        });

        component.selectedItemsObs = Observable.create((observer: Observer<Array<string>>) => {
          observer.next(['Ice Cream']);
          observer.complete();
        });

        fixture.detectChanges();

        component.selected.subscribe(selectedItems => {
          expect(selectedItems.length).toBe(2);
          expect(selectedItems.includes('Ice Cream')).toBe(true);
          expect(selectedItems.includes('Cookies')).toBe(true);
          done(); // ensure this is called
        });

        component.updateSelection(new MockEvent(), 'Cookies');
      });

      it('should not add missing item to selections', () => {
        spyOn(component.selected, 'emit');
        component.allItemsObs = Observable.create((observer: Observer<Array<string>>) => {
          observer.next(['Ice Cream', 'Cookies', 'Cake']);
          observer.complete();
        });

        component.selectedItemsObs = Observable.create((observer: Observer<Array<string>>) => {
          observer.next(['Ice Cream']);
          observer.complete();
        });

        fixture.detectChanges();

        expect(component.selected.emit).not.toHaveBeenCalled();

        component.updateSelection(new MockEvent(), 'fake');
      });
    });
});
