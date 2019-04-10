import { ComponentFixture, TestBed } from '@angular/core/testing';
import { CUSTOM_ELEMENTS_SCHEMA, SimpleChanges, SimpleChange } from '@angular/core';
import {
  EventFeedGuitarStringsComponent,
  GraphicProportions,
  GuitarStringDataContainer,
  BucketSizeControl
} from './event-feed-guitar-strings.component';
import { EventFeedService } from '../../services/event-feed/event-feed.service';
import { MockComponent } from 'ng2-mock-component';
import { combineReducers, StoreModule } from '@ngrx/store';
import * as eventFeed from '../../services/event-feed/event-feed.reducer';
import {
  GuitarString,
  GuitarStringItem,
  RespEventCount,
  GuitarStringCollection
} from '../../types/types';
import * as moment from 'moment';
import { find } from 'lodash';
import { initialState } from '../../services/event-feed/event-feed.reducer';

function create_changes(
  previousValue: GuitarStringCollection,
  currentValue: GuitarStringCollection): SimpleChanges {
  const changesObj: SimpleChanges = {
    guitarStringCollection: new SimpleChange(previousValue, currentValue, true)
  };
  return changesObj;
}

describe('EventFeedGuitarStringsComponent', () => {
  let component: EventFeedGuitarStringsComponent;
  let fixture: ComponentFixture<EventFeedGuitarStringsComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [
        EventFeedGuitarStringsComponent,
        MockComponent({ selector: 'chef-guitar-string-collection', inputs: ['height']}),
        MockComponent({ selector: ':svg:g',
          inputs: ['y', 'eventType', 'action', 'start', 'end', 'y',
            'index', 'space', 'iconRadius', 'width', 'multiple', 'task' ]})
      ],
      providers: [
        EventFeedService
      ],
      imports: [
        StoreModule.forRoot({
          'event_feed': combineReducers(eventFeed.eventFeedReducer)
        })
      ],
      schemas: [ CUSTOM_ELEMENTS_SCHEMA ]
    });

    fixture = TestBed.createComponent(EventFeedGuitarStringsComponent);
    component = fixture.componentInstance;
  });

  it('initial empty strings', () => {
    const updatedGuitarStringCollection =
      new GuitarStringCollection([],
      moment('2018-03-12').subtract(6, 'days').startOf('day'),
      moment('2018-03-12').endOf('day'));
    component.ngOnChanges(create_changes(initialState.guitarStringCollection,
      updatedGuitarStringCollection));

    expect(component.guitarStringDataContainer.startDateWindow).
      toEqual(moment('2018-03-12').subtract(6, 'days').startOf('day'));
    expect(component.guitarStringDataContainer.endDateWindow).
      toEqual(moment('2018-03-12').endOf('day'));
  });

  it('displays the correct tooltip text for singular items', () => {
    const item = createGuitarStringItem(false);
    const guitarString = createGuitarString(false);
    const text = component.getTooltipText(item, item.isMultiple(), guitarString);

    expect(text).toBe('A node was created.');
  });

  it('displays the correct tooltip text for a single environment', () => {
    const types = [{'name': 'environment', 'count': 1}];
    const item = new GuitarStringItem(types, moment(0), moment(0));
    const guitarString = new GuitarString( 'update', [item]);
    const text = component.getTooltipText(item, item.isMultiple(), guitarString);

    expect(text).toBe('An environment was updated.');
  });

  it('displays the correct tooltip text for a single organization', () => {
    const types = [{'name': 'organization', 'count': 1}];
    const item = new GuitarStringItem(types, moment(0), moment(0));
    const guitarString = new GuitarString( 'update', [item]);
    const text = component.getTooltipText(item, item.isMultiple(), guitarString);

    expect(text).toBe('An organization was updated.');
  });

  it('displays the correct tooltip text for multiple items', () => {
    const item = createGuitarStringItem(true);
    const guitarString = createGuitarString(true);
    const text = component.getTooltipText(item, item.isMultiple(), guitarString);

    expect(text).toBe('<strong>5 nodes</strong>, <strong>9 cookbooks</strong>,' +
      ' and <strong>7 roles</strong> were created.');
  });

  describe('GuitarStringDataContainer', () => {

    it('getNumberOfDaysInView defaults to 7 when not loaded', () => {
      const guitarStringDataContainer = new GuitarStringDataContainer();

      expect(guitarStringDataContainer.getNumberOfDaysInView()).toBe(7);
    });

    it('getNumberOfDaysInView is not set when loaded with the same date', () => {
      spyOn(console, 'error');
      const guitarStringDataContainer = new GuitarStringDataContainer();
      const date = moment();
      const guitarStringCollection: GuitarStringCollection =
        new GuitarStringCollection([], date, date);
      guitarStringDataContainer.updateGuitarStringCollection(guitarStringCollection);

      expect(guitarStringDataContainer.getNumberOfDaysInView()).toBe(7);
      expect(guitarStringDataContainer.dynamicallyBucketedGuitarStrings.length).toBe(0);
      expect(console.error).toHaveBeenCalled();
    });

    it('getNumberOfDaysInView is not set when start date is after end date', () => {
      spyOn(console, 'error');
      const guitarStringDataContainer = new GuitarStringDataContainer();
      const guitarStringCollection: GuitarStringCollection =
        new GuitarStringCollection([], moment().endOf('day'), moment().startOf('day'));
      guitarStringDataContainer.updateGuitarStringCollection(guitarStringCollection);

      expect(guitarStringDataContainer.getNumberOfDaysInView()).toBe(7);
      expect(guitarStringDataContainer.dynamicallyBucketedGuitarStrings.length).toBe(0);
      expect(console.error).toHaveBeenCalled();
    });

    it('getNumberOfDaysInView is 1 when one day of strings are used', () => {
      const guitarStringDataContainer = new GuitarStringDataContainer();
      const guitarStringCollection: GuitarStringCollection =
        new GuitarStringCollection([], moment().startOf('day'), moment().endOf('day'));
      guitarStringDataContainer.updateGuitarStringCollection(guitarStringCollection);

      expect(guitarStringDataContainer.getNumberOfDaysInView()).toBe(1);
    });

    it('getNumberOfDaysInView is 6 when 6 days of strings are used', () => {
      const guitarStringDataContainer = new GuitarStringDataContainer();
      const guitarStringCollection: GuitarStringCollection =
        new GuitarStringCollection([], moment().subtract(5, 'days').startOf('day'),
          moment().endOf('day'));
      guitarStringDataContainer.updateGuitarStringCollection(guitarStringCollection);

      expect(guitarStringDataContainer.getNumberOfDaysInView()).toBe(6);
    });

    it('setHourlyBucketSize only update the dynamicallyBucketedGuitarStrings' +
    'with bucket size is different', () => {
      const guitarStringDataContainer = new GuitarStringDataContainer();
      const guitarStringCollection: GuitarStringCollection =
        new GuitarStringCollection([createGuitarString(false)],
        moment().subtract(5, 'days').startOf('day'),
          moment().endOf('day'));
      guitarStringDataContainer.updateGuitarStringCollection(guitarStringCollection);

      let lastDynamicallyBucketedGuitarStrings =
        guitarStringDataContainer.dynamicallyBucketedGuitarStrings;
      guitarStringDataContainer.setHourlyBucketSize(4);

      expect(guitarStringDataContainer.dynamicallyBucketedGuitarStrings).
        not.toEqual(lastDynamicallyBucketedGuitarStrings);

      lastDynamicallyBucketedGuitarStrings =
        guitarStringDataContainer.dynamicallyBucketedGuitarStrings;
      guitarStringDataContainer.setHourlyBucketSize(4);

      expect(guitarStringDataContainer.dynamicallyBucketedGuitarStrings)
        .toEqual(lastDynamicallyBucketedGuitarStrings);
    });

    it('rebucket 24 hourly buckets to one 24 hour bucket', () => {
      const guitarStringDataContainer = new GuitarStringDataContainer();
      const start = moment().startOf('day');
      const end = moment().endOf('day');

      const guitarStringItems = [];
      let currentDate = start.clone();
      let count = 0;
      while (currentDate.isBefore(end)) {
        const guitarStringItem = new GuitarStringItem([{'name': 'node', 'count': count}],
          currentDate.clone().startOf('hour'), currentDate.clone().endOf('hour'));
        currentDate = currentDate.add(1, 'hour');

        guitarStringItems.push(guitarStringItem);
        count++;
      }

      const guitarString = new GuitarString('create', guitarStringItems);
      const guitarStringCollection: GuitarStringCollection =
        new GuitarStringCollection([guitarString], start, end);
      guitarStringDataContainer.updateGuitarStringCollection(guitarStringCollection);

      guitarStringDataContainer.setHourlyBucketSize(24);
      expect(guitarStringDataContainer.getNumberOfDaysInView()).toBe(1);

      expect(guitarStringDataContainer.dynamicallyBucketedGuitarStrings[0].items.length).toBe(1);

      const dayBucket = guitarStringDataContainer.dynamicallyBucketedGuitarStrings[0].items[0];

      expect(dayBucket.eventTypeCount.length).toBe(24);
      expect(dayBucket.start).toEqual(start);
      expect(dayBucket.end).toEqual(end);
    });

    it('rebucket 24 hourly buckets to two 12 hour buckets', () => {
      const guitarStringDataContainer = new GuitarStringDataContainer();
      const start = moment().startOf('day');
      const end = moment().endOf('day');

      const guitarStringItems = [];
      let currentDate = start.clone();
      let count = 0;
      while (currentDate.isBefore(end)) {
        const guitarStringItem = new GuitarStringItem([{'name': 'node', 'count': count}],
          currentDate.clone().startOf('hour'), currentDate.clone().endOf('hour'));
        currentDate = currentDate.add(1, 'hour');

        guitarStringItems.push(guitarStringItem);
        count++;
      }

      const guitarString = new GuitarString('create', guitarStringItems);
      const guitarStringCollection: GuitarStringCollection =
        new GuitarStringCollection([guitarString], start, end);
      guitarStringDataContainer.updateGuitarStringCollection(guitarStringCollection);

      guitarStringDataContainer.setHourlyBucketSize(12);
      expect(guitarStringDataContainer.getNumberOfDaysInView()).toBe(1);

      expect(guitarStringDataContainer.dynamicallyBucketedGuitarStrings[0].items.length).toBe(2);

      const morningBucket = guitarStringDataContainer.dynamicallyBucketedGuitarStrings[0].items[0];
      expect(morningBucket.eventTypeCount.length).toBe(12);
      expect(morningBucket.start).toEqual(start);

      for ( let i = 0; i < 12 ; i++ ) {
      expect(find(morningBucket.eventTypeCount,
        (eventTypeCount) => eventTypeCount.count === i)).not.toBeUndefined();
      }

      const afternoonBucket = guitarStringDataContainer.
        dynamicallyBucketedGuitarStrings[0].items[1];
      expect(afternoonBucket.eventTypeCount.length).toBe(12);
      expect(afternoonBucket.end).toEqual(end);

      for ( let i = 12; i < 24 ; i++ ) {
        expect(find(afternoonBucket.eventTypeCount,
          (eventTypeCount) => eventTypeCount.count === i)).not.toBeUndefined();
        }
    });

  });

  describe('BucketSizeControl', () => {
    it('increaseBucketSize goes to a larger bucket size ', () => {
      const bucketSizeControl = new BucketSizeControl();

      // decrease a few time to ensure we are not at the largest bucket size
      bucketSizeControl.decreaseBucketSize();
      bucketSizeControl.decreaseBucketSize();
      bucketSizeControl.decreaseBucketSize();

      const currentBucketSize = bucketSizeControl.bucketSizeInHours();

      bucketSizeControl.increaseBucketSize();

      expect(currentBucketSize).toBeLessThan(bucketSizeControl.bucketSizeInHours());
    });

    it('increaseBucketSize when already the largest bucket size', () => {
      const bucketSizeControl = new BucketSizeControl();

      while ( !bucketSizeControl.isLargestBucketSize() ) {
        bucketSizeControl.increaseBucketSize();
      }

      const currentBucketSize = bucketSizeControl.bucketSizeInHours();

      bucketSizeControl.increaseBucketSize();

      expect(currentBucketSize).toEqual(bucketSizeControl.bucketSizeInHours());
    });

    it('decreaseBucketSize goes to a smaller bucket size ', () => {
      const bucketSizeControl = new BucketSizeControl();

      // increase a few time to ensure we are not at the smallest bucket size
      bucketSizeControl.increaseBucketSize();
      bucketSizeControl.increaseBucketSize();
      bucketSizeControl.increaseBucketSize();

      const currentBucketSize = bucketSizeControl.bucketSizeInHours();

      bucketSizeControl.decreaseBucketSize();

      expect(currentBucketSize).toBeGreaterThan(bucketSizeControl.bucketSizeInHours());
    });

    it('decreaseBucketSize when already the smallest bucket size', () => {
      const bucketSizeControl = new BucketSizeControl();

      while ( !bucketSizeControl.isSmallestBucketSize() ) {
        bucketSizeControl.decreaseBucketSize();
      }

      const currentBucketSize = bucketSizeControl.bucketSizeInHours();

      bucketSizeControl.decreaseBucketSize();

      expect(currentBucketSize).toEqual(bucketSizeControl.bucketSizeInHours());
    });
  });

  describe('GraphicProportions', () => {
    it('when the component goes below minimum size, ' +
      'the proportions should be the same size as minimum size width', () => {
      const numberOfDaysInView = 7;
      const graphicProportions = new GraphicProportions();

      // minimum size is graphicProportions.minComponentWidth.
      graphicProportions.update(
        graphicProportions.minComponentWidth, numberOfDaysInView);

      expect(graphicProportions.spaceWidthBetweenItems).toEqual(5.2272727272727275);
      expect(graphicProportions.sectionWidth).toEqual(76.42857142857143);

      graphicProportions.update(
        graphicProportions.minComponentWidth, numberOfDaysInView);

      expect(graphicProportions.spaceWidthBetweenItems).toEqual(5.2272727272727275);
      expect(graphicProportions.sectionWidth).toEqual(76.42857142857143);
    });

    it('when the component has an acceptable width', () => {
      const numberOfDaysInView = 7;
      const graphicProportions = new GraphicProportions();
      graphicProportions.update(1000, numberOfDaysInView);

      expect(graphicProportions.spaceWidthBetweenItems).toEqual(3.7209302325581395);
      expect(graphicProportions.sectionWidth).toEqual(142.85714285714286);
    });

    it('when there is no days in view, the component does not render', () => {
      const numberOfDaysInView = 0;
      const graphicProportions = new GraphicProportions();

      graphicProportions.update(1000, numberOfDaysInView);

      expect(graphicProportions.spaceWidthBetweenItems).toBeUndefined();
      expect(graphicProportions.sectionWidth).toBeUndefined();
    });

    it('The bucket size changes dependent on the component with', () => {
      const numberOfDaysInView = 7;
      const graphicProportions = new GraphicProportions();

      graphicProportions.update(graphicProportions.minComponentWidth, numberOfDaysInView);

      const currentBucketSize = graphicProportions.getBucketSize();

      graphicProportions.update(graphicProportions.minComponentWidth * 100, numberOfDaysInView);

      expect(currentBucketSize).toBeGreaterThan(graphicProportions.getBucketSize());

      graphicProportions.update(graphicProportions.minComponentWidth, numberOfDaysInView);

      expect(currentBucketSize).toBe(graphicProportions.getBucketSize());
    });
  });


  function createGuitarStringItem(isMultiple: boolean): GuitarStringItem {
    let types: RespEventCount[];

    if (isMultiple) {
      types = [
        {'name': 'node', 'count': 5},
        {'name': 'cookbook', 'count': 9},
        {'name': 'role', 'count': 7}
      ];
    } else {
      types = [
        {'name': 'node', 'count': 1}
      ];
    }

    return new GuitarStringItem(types, moment(0), moment(0));
  }

  function createGuitarString(isMultiple: boolean): GuitarString {
    return new GuitarString(
      'create',
      [ createGuitarStringItem(isMultiple) ]
    );
  }
});
