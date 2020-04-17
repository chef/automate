import { Component, OnInit, Input } from '@angular/core';
import * as d3 from 'd3';

@Component({
  selector: 'app-line-graph',
  templateUrl: './line-graph.component.html'
})
export class LineGraphComponent implements OnInit {

  constructor() {}

  // @Input() data = [];
  data: any[] = [ 1, 2, 3, 4, 5, 27, 22, 11, 23, 13, 11, 7, 9, 6, 4 ];

  // sizing the chart
  @Input() vbWidth = 900;
  @Input() vbHeight = 300;


  // get viewBox() {
  //   return `0 0 ${this.vbWidth} ${this.vbHeight}`;
  // }

  ngOnInit() {
    this.print();
  }

  get scaleX() {
    return d3.scaleLinear()
        .domain([0, this.data.length]) // incoming data [min, max]
        .range([0, 500]); // specifies pixel value used to draw the graph
  }

  get scaleY() {
    return d3.scaleLinear()
        .domain([Math.min(...this.data), Math.max(...this.data)]) // incoming data [min, max]
        .range([0, 500]); // specifies pixel value used to draw the graph
  }

  // this function generates the svg line path format
  // i.e. this.generator( pass in tuples here )
  get generator() {
    return d3.line();
  }

  // this function is turning out data into tuples that can be plotted across
  // an x and y axis of width/height that we determined in our sizing
  tuples() {
    return this.data
      .map((data, index) => [index, data])
      .map(([x, y]) => [this.scaleX(x), this.scaleY(y)]);
                        // scaleX and y need to gets functions in order to pass in
  }

  print() {
    console.log(this.generator(this.tuples()));
  }

}


