
import { endsWith, replace } from 'lodash/fp';

export class EventHelper {
    public static getEventIcon(eventType: string): string {
        switch (eventType) {
          case 'policy': return 'add_to_photos';
          case 'user': return 'person';
          case 'group': return 'people';
          case 'permission': return 'lock_open';
          case 'organization': return 'layers';
          case 'node': return 'storage';
          case 'cookbook': return 'chrome_reader_mode';
          case 'version': return 'chrome_reader_mode';
          case 'cookbook_artifact_version': return 'chrome_reader_mode';
          case 'item': return 'business_center';
          case 'bag': return 'business_center';
          case 'environment': return 'public';
          case 'role': return 'book';
          case 'profile': return 'library_books';
          case 'scanjobs': return 'wifi_tethering';
          case 'client': return 'assignment_ind';
          case 'multiple': return 'more_horiz';
          case 'local_user': return 'person_outline';
          default: return 'help';
        }
    }

    public static getEventTaskClass(task): string {
        switch (task) {
            case 'delete':
                return 'delete';
            case 'edit':
                return 'edit';
            case 'update':
                return 'edit';
            case 'create':
                return 'create';
            default:
                return '';
        }
    }

    public static getEventTypeLabel(name: string, multiple: boolean) {
        let label;

        switch (name) {
          case 'item':
            label = 'data bag item';
            break;
          case 'version':
            label = 'cookbook';
            break;
          case 'bag':
            label = 'data bag';
            break;
          case 'scanjobs':
            label = 'scan job';
            break;
          case 'local_user':
            label = 'local user';
            break;
          case 'cookbook_artifact_version':
            label = 'cookbook';
            break;
          default:
            label = name;
        }

        // Pluralize the event label type if necessary
        if (multiple) {
          label = EventHelper.pluralize(label);
        }

        return label;
      }

      public static pluralize(str: string): string {
        if (str === 'key') {
          return 'keys';
        }
        if (endsWith('y', str)) {
          return replace(/y$/, 'ies', str);
        }

        return str + 's';
      }
}
