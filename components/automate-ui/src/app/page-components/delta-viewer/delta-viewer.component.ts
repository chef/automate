import { Component, Input } from '@angular/core';
import { DomSanitizer, SafeHtml } from '@angular/platform-browser';
import { Diff2Html } from 'diff2html';

@Component({
  selector: 'app-delta-viewer',
  templateUrl: './delta-viewer.component.html',
  styleUrls: ['./delta-viewer.component.scss']
})
export class DeltaViewerComponent {
  @Input() delta: string;

  get diffHtml(): SafeHtml {
    const str = this.delta.split('\\n').join('\n');
    const html = Diff2Html.getPrettyHtml(str);
    return this.sanitizer.bypassSecurityTrustHtml(html);
  }

  constructor(private sanitizer: DomSanitizer) {}
}
