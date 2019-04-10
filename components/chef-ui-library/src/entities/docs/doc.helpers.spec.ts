import {
  filterTags,
  findTag,
  getComponentMetaData,
  getComponentProps
} from './doc.helpers';

describe('find and filter', () => {

  const data = {
    'name': 'TestAtom',
    'comment': {
      'tags': [
        { 'tag': 'description',
          'text': 'This is a description' },
        { 'tag': 'example',
          'text': '<h1>This is an example</h1>' },
        { 'tag': 'example',
          'text': '<h1>This is another example</h1>' }
      ]
    }
  };

  describe('findTag', () => {

    it('returns a tags text.', () => {
      expect(findTag('description', data)).toBe(data['comment']['tags'][0]['text']);
    });

    it('returns the text of the first tag it finds.', () => {
      expect(findTag('example', data)).toBe(data['comment']['tags'][1]['text']);
    });

    it('returns an empty string(\'\') when an empty object is passed.', () => {
      expect(findTag('description', {})).toBe('');
    });

    it('returns an empty string(\'\') when an non-existing key is passed.', () => {
      expect(findTag('foobar', data)).toBe('');
    });

  });

  describe('filterTags', () => {

    it('returns a list containing the text for each matching tag.', () => {
      const expected = [
        data['comment']['tags'][1]['text'],
        data['comment']['tags'][2]['text'],
      ];

      expect(filterTags('example', data)).toEqual(expected);
    });

    it('returns a list even if there is only a single result', () => {
      expect(filterTags('description', data)).toEqual([data['comment']['tags'][0]['text']]);
    });

    it('returns an empty list when an empty object is passed.', () => {
      expect(filterTags('description', {})).toEqual([]);
    });

    it('returns an empty list when an non-existing key is passed.', () => {
      expect(filterTags('foobar', data)).toEqual([]);
    });

  });

});

describe('getComponentMetaData', () => {

  const data = {
    'decorators': [
      { 'name': 'Component',
        'arguments': {
          'opts': '{\n  tag: \'test-atom\',\n  styleUrl: \'test-atom.scss\',\n  shadow: true}' }
      }
    ]
  };

  it('returns the metadata for a component', () => {
    const expected = {
      tag: 'test-atom',
      styleUrl: 'test-atom.scss',
      shadow: true
    };

    expect(getComponentMetaData(data)).toEqual(expected);
  });

});

describe('getComponentProps', () => {
  const data = [
    { 'kind': 1024,
      'name': 'Property1',
      'comment': {
        'shortText': 'Description of property.'
      },
      'decorators': [
        { 'name': 'Prop' }
      ]
    },
    { 'kind': 1024,
      'name': 'Property2',
      'comment': {
        'shortText': 'Description of another property.'
      },
      'decorators': [
        { 'name': 'Prop' }
      ],
      'defaultValue': '"2"'
    }
  ];

  it('returns a list of properties', () => {
    const expected = [
      { name: 'Property1',
        description: data[0]['comment']['shortText'],
        defaultValue: undefined
      },
      { name: 'Property2',
        description: data[1]['comment']['shortText'],
        defaultValue: '"2"'
      }
    ];

    expect(getComponentProps(data)).toEqual(expected);
  });
});
