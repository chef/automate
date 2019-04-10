import {
  Component,
  OnInit,
  OnDestroy,
  OnChanges,
  ElementRef,
  HostListener,
  Input,
  Output,
  EventEmitter,
  SimpleChanges,
  ViewChild
} from '@angular/core';
import { GuitarString,
  GuitarStringCollection,
  GuitarStringItem,
  DateRange
} from '../../types/types';
import * as moment from 'moment';
import { initialState } from '../../services/event-feed/event-feed.reducer';
import { Subject } from 'rxjs';
import { reduce } from 'lodash/fp';
import * as d3 from 'd3';


export class GuitarStringDataContainer {
  dynamicallyBucketedGuitarStrings: GuitarString[] = [];
  hourlyBucketedGuitarStrings: GuitarString[] = [];
  hourlyBucketSize = initialState.filters.hoursBetween;
  public startDateWindow = initialState.filters.startDate;
  public endDateWindow = initialState.filters.endDate;
  sectionDates = this.createSections();
  public buttonDates = this.sectionDates;

  constructor() {}

  updateGuitarStringCollection(guitarStringCollection: GuitarStringCollection): void {
    if ( guitarStringCollection.start.isBefore(guitarStringCollection.end) ) {
      this.startDateWindow = guitarStringCollection.start;
      this.endDateWindow = guitarStringCollection.end;

      this.hourlyBucketedGuitarStrings = guitarStringCollection.strings;
      this.dynamicallyBucketedGuitarStrings = this.rebucketGuitarStrings();
      this.sectionDates = this.createSections();
    } else {
      console.error('GuitarStringCollection had the start date older than the end date');
    }
  }

  setHourlyBucketSize(updatedHourlyBucketSize: number): void {
    // Updating the 'this.guitarStringCollection' variable causes the whole graphic to be re-drawn
    if (this.hourlyBucketSize !== updatedHourlyBucketSize) {
      this.hourlyBucketSize = updatedHourlyBucketSize;
      this.dynamicallyBucketedGuitarStrings = this.rebucketGuitarStrings();
    }
  }

  getNumberOfDaysInView(): number {
    return this.sectionDates.length;
  }

  private rebucketGuitarStrings(): GuitarString[] {
    return this.hourlyBucketedGuitarStrings.map(guitarString => {
      const initialItem = new GuitarStringItem([], moment().add(2000, 'year'), moment(0));

      const guitarStringItemAccum: GuitarStringItemAccum =
        reduce((acc: GuitarStringItemAccum, item: GuitarStringItem) => {
          const mergedItem = this.mergeItems(item, acc.currentItem);
          if (acc.count === this.hourlyBucketSize) {
            acc.items.push(mergedItem);
            acc.count = 1;
            acc.currentItem = initialItem;
          } else {
            acc.count = acc.count + 1;
            acc.currentItem = mergedItem;
          }

          return acc;
        }, {
          items: [],
          count: 1,
          currentItem: initialItem
        })(guitarString.items);

      return new GuitarString(guitarString.eventAction, guitarStringItemAccum.items);
    });
  }

  private mergeItems(item1: GuitarStringItem, item2: GuitarStringItem): GuitarStringItem {
    const eventTypeCount = item1.eventTypeCount.concat(item2.eventTypeCount);
    let start: moment.Moment;
    if (item1.start.isBefore(item2.start)) {
      start = item1.start;
    } else {
      start = item2.start;
    }

    let end: moment.Moment;
    if (item1.end.isAfter(item2.end)) {
      end = item1.end;
    } else {
      end = item2.end;
    }

    return new GuitarStringItem(eventTypeCount, start, end);
  }

  private createSections(): Date[] {
    const sections: Date[] = [];
    let current = this.startDateWindow.clone();

    while ( current < this.endDateWindow ) {
      sections.push(current.toDate());
      current = current.add(1, 'days');
    }

    return sections;
  }
}

export interface GuitarStringItemAccum {
  items: GuitarStringItem[];
  count: number;
  currentItem: GuitarStringItem;
}

export class BucketSizeControl {
  availableBucketSizesInHours = [1, 2, 3, 4, 6, 8, 12, 24];
  indexSelected = 0;

  bucketSizeInHours(): number {
    return this.availableBucketSizesInHours[this.indexSelected];
  }

  numberOfBucketsPerDay(): number {
    return 24 / this.bucketSizeInHours();
  }

  isSmallestBucketSize(): boolean {
    return this.indexSelected === 0;
  }

  isLargestBucketSize(): boolean {
    return this.indexSelected === (this.availableBucketSizesInHours.length - 1);
  }

  decreaseBucketSize(): void {
    if ( this.indexSelected > 0 ) {
      this.indexSelected--;
    }
  }

  increaseBucketSize(): void {
    if (this.indexSelected < this.availableBucketSizesInHours.length - 1) {
      this.indexSelected++;
    }
  }
}

export class GraphicProportions {
  spaceWidthBetweenItems: number;
  sectionWidth: number;

  bucketSizeControl: BucketSizeControl = new BucketSizeControl();

  // constants
  spaceBetweenStrings = 30;
  firstStringY = 25;
  minSpaceWidthBetweenItems = 1.5;
  minComponentWidth = 535;
  componentPadding = 15;
  iconRadius = 10;
  iconWidth = this.iconRadius * 2;
  maxSpaceWidthBetweenItems = this.iconRadius;

  constructor() {}

  update(componentWidth: number, numberOfDaysInView: number): void {
    if (numberOfDaysInView > 0) {
      if (componentWidth < this.minComponentWidth) {
        componentWidth = this.minComponentWidth;
      }

      // The canvasWidth is the width of this component minus the components padding
      const canvasWidth = componentWidth;

      let spaceBetweenItems = this.calculateSpaceBetweenItems(
        canvasWidth, numberOfDaysInView *
        this.bucketSizeControl.numberOfBucketsPerDay());

      spaceBetweenItems = this.autoAdjustBucketSize(spaceBetweenItems,
        numberOfDaysInView, canvasWidth);

      // Update all these template variables at the same time.
      this.spaceWidthBetweenItems = spaceBetweenItems;
      this.sectionWidth = canvasWidth / numberOfDaysInView;
    }
  }

  getBucketSize(): number {
    return this.bucketSizeControl.bucketSizeInHours();
  }

  private autoAdjustBucketSize(spaceBetweenItems: number,
    numberOfDaysInView: number, canvasWidth: number): number {
    spaceBetweenItems = this.decreaseBucketSize(spaceBetweenItems,
      numberOfDaysInView, canvasWidth);

    // The increase check should be last, because the main concern is that
    // the items are not overlapping.
    return this.increaseBucketSize(spaceBetweenItems,
      numberOfDaysInView, canvasWidth);
  }

  private increaseBucketSize(spaceBetweenItems: number,
    numberOfDaysInView: number, canvasWidth: number): number {

    while (spaceBetweenItems < this.minSpaceWidthBetweenItems &&
      !this.bucketSizeControl.isLargestBucketSize()) {

      this.bucketSizeControl.increaseBucketSize();

      spaceBetweenItems = this.calculateSpaceBetweenItems(
        canvasWidth,
        numberOfDaysInView * this.bucketSizeControl.numberOfBucketsPerDay());
    }

    return spaceBetweenItems;
  }

  private decreaseBucketSize(spaceWidthBetweenItems: number,
    numberOfDaysInView: number, canvasWidth: number): number {

    while (spaceWidthBetweenItems > this.maxSpaceWidthBetweenItems &&
      !this.bucketSizeControl.isSmallestBucketSize()) {
        this.bucketSizeControl.decreaseBucketSize();

        spaceWidthBetweenItems = this.calculateSpaceBetweenItems(
          canvasWidth,
          numberOfDaysInView * this.bucketSizeControl.numberOfBucketsPerDay());
    }

    return spaceWidthBetweenItems;
  }

  // Calculating the spacing width between each items
  //
  // Find the total amount of empty space by subtracting the total width of all the item
  // positions from the canvas width.
  //
  // The number of spaces between items is the number of items plus one. This is because there is
  // one space before each item and then one extra space after the last item.
  //
  // To find the space width between each item, divide the total amount of empty space
  // by the number of spaces between items.
  private calculateSpaceBetweenItems(canvasWidth: number, totalNumberOfItems: number): number {
    const numberOfSpacesBetweenItems = totalNumberOfItems + 1;
    const totalAmountOfEmptySpace = (canvasWidth - (totalNumberOfItems * this.iconWidth));

    return totalAmountOfEmptySpace / numberOfSpacesBetweenItems;
  }
}

@Component({
  selector: 'app-event-feed-guitar-strings',
  templateUrl: './event-feed-guitar-strings.component.html',
  styleUrls: ['./event-feed-guitar-strings.component.scss']
})
export class EventFeedGuitarStringsComponent implements OnInit, OnDestroy, OnChanges {
  private isDestroyed: Subject<boolean> = new Subject<boolean>();
  @Output() newDateRange: EventEmitter<DateRange> = new EventEmitter();
  @Input() guitarStringCollection: GuitarStringCollection = initialState.guitarStringCollection;

  graphicProportions: GraphicProportions = new GraphicProportions();
  guitarStringDataContainer: GuitarStringDataContainer = new GuitarStringDataContainer();

  buttonDates = this.guitarStringDataContainer.buttonDates;
  startSliderPosition = 0;
  endSliderPosition = 100;
  sliderGrid = 100 / this.buttonDates.length;
  sliderWidth: number;
  @ViewChild('startSlider') startSlider: ElementRef;
  @ViewChild('endSlider') endSlider: ElementRef;

  constructor(private el: ElementRef) {}

  ngOnInit() {
    this.updateGraphic();
    this.setupZoomControls();
  }

  ngOnChanges(changes: SimpleChanges) {
    if (changes['guitarStringCollection']) {
      this.updateGuitarStringCollection(changes['guitarStringCollection'].currentValue);
    }
  }

  dateSelected(dateSelected?: Date, index?: number): void {
    let start: moment.Moment;
    let end: moment.Moment;

    this.startSliderPosition = index * this.sliderGrid;
    this.endSliderPosition = (index + 1) * this.sliderGrid;

    if (this.guitarStringDataContainer.getNumberOfDaysInView() !== 1) {
      const date = moment(dateSelected);
      start = date.clone().startOf('day');
      end = date.clone().endOf('day');

      this.changeSliderPosition('start', this.startSliderPosition, index);
      this.changeSliderPosition('end', this.endSliderPosition, index);

      this.newDateRange.emit({
        start: start,
        end: end
      });
    } else {
      this.resetSliders();
    }
  }

  itemSelected(_item: GuitarStringItem): void {}

  ngOnDestroy(): void {
    this.isDestroyed.next(true);
    this.isDestroyed.complete();
  }

  @HostListener('window:resize', ['$event.target'])
  onResize() {
    this.updateGraphic();
  }

  @HostListener('window:keydown', ['$event']) onKeyDown(e) {
    if (e.shiftKey && e.key === 'R') {
      this.resetSliders();
    }
  }

  // Tooltips: Build the sentence that goes in the tooltips
  getTooltipText(item: GuitarStringItem, isMultiple: boolean, guitarString: GuitarString) {
    let text = '';
    const length = item.eventTypeCount.length;

    if (isMultiple) {
      // Loops through each event type if the item is multiple.
      item.eventTypeCount.forEach((type, index) => {
        // Adds a comma between each element if there is more than 2 event types
        // Only two items should not have a comma between the first and second item
        // ex: 30 nodes and 2 cookbooks were updated.
        //     30 nodes, 2 cookbooks, and 1 environment was updated.
        if (index > 0 && length > 2) {
          text += ', ';
        }
        // Adds a space before 'and ' if there are only two items
        if (index === 1 && length === 2) {
          text += ' ';
        }
        // Adds 'and' before the event type name if it is the last event type in the list
        if ( index !== 0 && length === (index + 1) ) {
          text += 'and ';
        }
        // Adds the event type count
        text += `<strong>${type.count} `;

        // Formats the event type name
        text += `${item.getEventTypeLabel(type.name, type.count)}</strong>`;
      });

      text += ' were ';
    } else {
      // Formats the event type name
      switch (item.getEventType()) {
        case 'organization':
        case 'environment':
          text += 'An ';
          break;
        default:
          text += 'A ';
      }

      // If the item is not multiple, display the event type
      text += `${item.getEventTypeLabel(
        item.eventTypeCount[0].name, item.eventTypeCount[0].count)} was `;
    }

    // Adds the action to the end of the sentence.
    text += `${guitarString.eventAction}d.`;

    return text;
  }

  tooltipOffset(index: number) {
    return index > 36 ? '-120 0' : '0 0';
  }

  // Zoom Controls
  setupZoomControls() {
    const svg = d3.select('#guitar-string-zoom-slider');

    svg.select('.zoom-slider-button.start')
      .call(d3.drag()
        .on('start', () => { this.dragStarted(); })
        .on('drag', () => { this.dragStartSlider(); })
        .on('end', () => { this.dragEnded('start'); }));
    svg.select('.zoom-slider-button.end')
      .call(d3.drag()
        .on('start', () => { this.dragStarted(); })
        .on('drag', () => { this.dragEndSlider(); })
        .on('end', () => { this.dragEnded('end'); }));
  }

  dragStarted() {
    this.sliderWidth = this.getComponentWidth();
  }

  dragStartSlider() {
    const positionPercentage = d3.event.x / this.sliderWidth * 100;

    // Checks to see if the start slider is about to bump into the end slider
    if (this.endSliderPosition - positionPercentage >= this.sliderGrid) {
      // Prevents the start slider from going outside of the bounds of the slider box
      this.startSliderPosition = positionPercentage >= 0 ? positionPercentage : 0;

      d3.select('.zoom-slider-button.start').attr('cx', this.startSliderPosition + '%');
      d3.select('.zoom-slider-icon.start').attr('x', this.startSliderPosition + '%');
      d3.select('.slider-overlay.start').attr('width', this.startSliderPosition + '%');
    }
  }

  dragEndSlider() {
    const positionPercentage = d3.event.x / this.sliderWidth * 100;

    // Checks to see if the end slider is about to bump into the start slider
    if (positionPercentage - this.sliderGrid >= this.startSliderPosition) {
      // Prevents the end slider from going outside of the bounds of the slider box
      this.endSliderPosition = positionPercentage <= 100 ? positionPercentage : 100;

      d3.select('.zoom-slider-button.end').attr('cx', this.endSliderPosition + '%');
      d3.select('.zoom-slider-icon.end').attr('x', this.endSliderPosition + '%');
      d3.select('.slider-overlay.end').attr('x', this.endSliderPosition + '%')
        .attr('width', 100 - this.endSliderPosition + '%');
    }
  }

  dragEnded(className: string) {
    let index, position;

    if (className === 'start') {
      index = this.snapToGrid(this.startSliderPosition).index;
      position = this.snapToGrid(this.startSliderPosition).percentage;
      this.startSliderPosition = position;
    } else if (className === 'end') {
      index = this.snapToGrid(this.endSliderPosition).index;
      position = this.snapToGrid(this.endSliderPosition).percentage;
      this.endSliderPosition = position;
    }

    this.changeSliderDateRange(className, index);
    this.changeSliderPosition(className, position, index);
  }


  changeSliderDateRange(className: string, index: number) {
    if (className === 'start') {
      // Prevents the API call if the old start date and new start date are the same
      if (moment(this.guitarStringDataContainer.buttonDates[index]).clone().format('MMM D') ===
        this.guitarStringDataContainer.startDateWindow.clone().format('MMM D')) {
          return;
      }

      this.newDateRange.emit({
        start: moment(this.guitarStringDataContainer.buttonDates[index]).clone().startOf('day'),
        end: this.guitarStringDataContainer.endDateWindow.clone().endOf('day')
      });
    } else if (className === 'end') {
      // Prevents the API call if the old end date and new end date are the same
      if (this.guitarStringDataContainer.endDateWindow.clone().format('MMM D') ===
        moment(this.guitarStringDataContainer.buttonDates[index - 1]).clone().format('MMM D')) {
          return;
      }

      this.newDateRange.emit({
        start: this.guitarStringDataContainer.startDateWindow.clone().startOf('day'),
        end: moment(this.guitarStringDataContainer.buttonDates[index - 1]).clone().endOf('day')
      });
    }
  }

  moveSlider(event: KeyboardEvent, className: string) {
    const key = event.key;

    if (key !== 'ArrowRight' && key !== 'ArrowLeft') {
      return;
    }

    if (key === 'ArrowRight') {
      if (className === 'start') { // moving start slider right
        // Checks to see if the start slider is about to bump into the end slider
        if (this.endSliderPosition - this.startSliderPosition >= this.sliderGrid) {
          // Prevents the start slider from going outside of the bounds of the slider box
          this.startSliderPosition = this.startSliderPosition + this.sliderGrid;
        }
      } else if (className === 'end') { // moving end slider right
        this.endSliderPosition = this.endSliderPosition + this.sliderGrid;
        this.endSliderPosition = this.endSliderPosition <= 100 ? this.endSliderPosition : 100;
      }
    } else if (key === 'ArrowLeft') {
      if (className === 'start') { // moving start slider left
        this.startSliderPosition = this.startSliderPosition - this.sliderGrid;
        this.startSliderPosition = this.startSliderPosition >= 0 ? this.startSliderPosition : 0;
      } else if (className === 'end') { // moving end slider left
        if (this.endSliderPosition - this.sliderGrid >= this.startSliderPosition) {
          // Prevents the end slider from going outside of the bounds of the slider box
          this.endSliderPosition = this.endSliderPosition - this.sliderGrid;
        }
      }
    }

    if (className === 'start') {
      this.changeSliderPosition(
        className, this.startSliderPosition, this.snapToGrid(this.startSliderPosition).index
      );
      this.changeSliderDateRange(className, this.snapToGrid(this.startSliderPosition).index);
    } else if (className === 'end') {
      this.changeSliderPosition(
        className, this.endSliderPosition, this.snapToGrid(this.endSliderPosition).index
      );
      this.changeSliderDateRange(className, this.snapToGrid(this.endSliderPosition).index);
    }
  }

  changeSliderPosition(className: string, position: number, index: number) {
    d3.select(`.zoom-slider-button.${className}`)
      .transition()
      .attr('cx', position + '%')
      .attr('aria-valuenow', index)
      .attr('value', index)
      .attr('aria-valuetext',
        moment(this.guitarStringDataContainer.buttonDates[index]).clone().format('ddd, MMM D'));
    d3.select(`.zoom-slider-icon.${className}`)
      .transition()
      .attr('x', position + '%');
    d3.selectAll('.slider-overlay')
      .attr('fill', 'url(#slider-overlay-default)');

    if (className === 'start') {
      d3.select(`.slider-overlay.${className}`)
        .transition()
        .attr('width', position + '%');
    } else if (className === 'end') {
      d3.select(`.slider-overlay.${className}`)
        .transition()
        .attr('x', position + '%')
        .attr('width', 100 - position + '%');
    }
  }

  snapToGrid(position: number) {
    return {
      percentage: Math.round(position / this.sliderGrid) * this.sliderGrid,
      index: Math.round(position / this.sliderGrid)
    };
  }

  resetSliders() {
    this.startSliderPosition = 0;
    this.endSliderPosition = 100;

    d3.select('.zoom-slider-button.start')
      .transition().attr('cx', '0%');
    d3.select('.zoom-slider-icon.start')
      .transition().attr('x', '0%');
    d3.select('.slider-overlay.start')
      .transition().attr('width', '0%');
    d3.select('.zoom-slider-button.end')
      .transition().attr('cx', '100%');
    d3.select('.zoom-slider-icon.end')
      .transition().attr('x', '100%');
    d3.select('.slider-overlay.end')
      .transition().attr('x', '100%').attr('width', '0%');

    this.newDateRange.emit({
      start: initialState.filters.startDate,
      end: initialState.filters.endDate
    });
  }

  zoomButtonWidth(_length: number) {
    return this.sliderGrid + '%';
  }

  zoomButtonPosition(index: number) {
    return this.sliderGrid * index + '%';
  }

  zoomTextPosition(index: number) {
    return (this.sliderGrid * index) + (this.sliderGrid / 2) + '%';
  }

  private updateGraphic(): void {
    this.graphicProportions.update(this.getComponentWidth(),
      this.guitarStringDataContainer.getNumberOfDaysInView());
    this.guitarStringDataContainer.setHourlyBucketSize(this.graphicProportions.getBucketSize());
  }

  private updateGuitarStringCollection(guitarStringCollection: GuitarStringCollection): void {
    this.guitarStringDataContainer.updateGuitarStringCollection(guitarStringCollection);
    this.updateGraphic();
  }

  private getComponentWidth(): number {
    return this.el.nativeElement.getBoundingClientRect().width;
  }
}
