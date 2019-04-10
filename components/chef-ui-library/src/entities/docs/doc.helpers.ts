import filter from 'lodash/fp/filter';
import find from 'lodash/fp/find';
import get from 'lodash/fp/get';
import getOr from 'lodash/fp/getOr';
import map from 'lodash/fp/map';
import pipe from 'lodash/fp/pipe';
import replace from 'lodash/fp/replace';
import trim from 'lodash/fp/trim';
import split from 'lodash/fp/split';
import first from 'lodash/fp/first';

const getTagList: (json: object) => object[] =
  getOr([], ['comment', 'tags']);

const getTagText: (tag: object) => string =
  pipe(get('text'), trim);

/**
 * @param name  The tag name to search for.
 * @param json  The json object to search through.
 * @returns     The text for the first matching tag.
 */
export const findTag: (name: string, json: object) => string =
  (name, json) => pipe(getTagList,
                       find(['tag', name]),
                       getTagText)(json);

/**
 * @param name  The tag name to search for.
 * @param json  The json object to search through.
 * @returns     A list containing the text for each matching tag.
 */
export const filterTags: (name: string, json: object) => string[] =
  (name, json) => pipe(getTagList,
                       filter(['tag', name]),
                       map(getTagText))(json);

const getDecorators =
  getOr([], 'decorators');

const parseObjectString =
  pipe(replace(/(\w+):/ig, '"$1":'),
       replace(/'/ig, '"'),
       (s) => s ? JSON.parse(s) : {});

/**
 * @param json The json object to extract metadata from.
 * @returns    An object containing the components metadata.
 */
export const getComponentMetaData: (json: object) => {tag: string, styleUrl: string, shadow: boolean} =
  pipe(getDecorators,
       find(['name', 'Component']),
       getOr('', ['arguments', 'opts']),
       parseObjectString);

export function getComponentProps(children: object[]) {
  const filterer = (child) => {
    return child['kind'] === 1024 &&
      child['decorators'] &&
      child['decorators'][0]['name'] === 'Prop';
  };

  const properties = filter(filterer, children);
  return map((prop) => {
    return {
      name: prop['name'],
      description: getOr('', ['comment', 'shortText'], prop),
      defaultValue: prop['defaultValue']
    };
  }, properties);
}

/**
 * The type of the component is derived from it's parent directory.
 * It can be one of 'atoms', 'molecules', 'charts' or 'templates'.
 * @param json The json object to extract the type from.
 * @returns    The string representing the component type.
 */
export const getType: (json: object) => string =
  pipe(get(['sources', 0, 'fileName']),
       split('/'),
       first);
