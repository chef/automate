import { Component, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import {
  Node
} from '../../entities/client-runs/client-runs.model';

@Component({
  selector: 'app-node-noruns-details',
  templateUrl: './node-noruns-details.component.html',
  styleUrls: ['./node-noruns-details.component.scss']
})
export class NodeNoRunsDetailsComponent implements OnInit {
  nodeId: string;
  node: Node;

  constructor(
    private route: ActivatedRoute
  ) {}

  ngOnInit() {
    this.nodeId = this.getRouteParam('node-id');
    this.route.data.subscribe((data: { node: Node }) => {
        this.node = data.node;
    });
  }

  private getRouteParam(paramName: string): string | undefined {
    return this.route.snapshot.params[paramName];
  }
}
