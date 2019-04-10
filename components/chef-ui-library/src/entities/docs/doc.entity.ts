import { Entity } from '../entities';
import {
  filterTags,
  findTag,
  getComponentMetaData,
  getComponentProps,
  getType
} from './doc.helpers';

interface Property {
  name: string;
  description?: string;
  defaultValue?: string;
}

export class DocEntity implements Entity {
  id: string;
  name: string;
  description: string;
  tag: string;
  styleUrl: string;
  shadow: boolean;
  properties: Property[];
  examples: string[];
  docType: string;

  constructor(jsonDoc = {}) {
    const { tag, styleUrl, shadow } = getComponentMetaData(jsonDoc);

    this.id = tag;
    this.name = jsonDoc['name'];
    this.description = findTag('description', jsonDoc);
    this.tag = tag;
    this.styleUrl = styleUrl || '';
    this.shadow = shadow || false;
    this.properties = getComponentProps(jsonDoc['children']);
    this.examples = filterTags('example', jsonDoc);
    this.docType = getType(jsonDoc);
  }

  static create(json) {
    return new DocEntity(json);
  }
}
