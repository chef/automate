import { SearchableNode } from '../models';
import { ChildLists } from './models';

export const mockSearchableTree: SearchableNode<ChildLists> = {
  id: '1',
  value: {
    name: 'run_list1',
    version: '0.1',
    type: 'role'
  },
  children: [
    {
      id: '11',
      value: {
        name: 'run_list1',
        version: '0.1',
        type: 'recipe'
      },
      children: []
    },
    {
      id: '12',
      value: {
        name: 'run_list1',
        version: '0.1',
        type: 'recipe'
      },
      children: []
    },
    {
      id: '13',
      value: {
        name: 'run_list12',
        version: '0.1',
        type: 'recipe'
      },
      children: [
        {
          id: '131',
          value: {
            name: 'run_list13',
            version: '0.1',
            type: 'recipe'
          },
          children: []
        },
        {
          id: '132',
          value: {
            name: 'run_list14',
            version: '0.1',
            type: 'recipe'
          },
          children: [
            {
              id: '1321',
              value: {
                name: 'run_list15',
                version: '0.1',
                type: 'recipe'
              },
              children: []
            },
            {
              id: '1322',
              value: {
                name: 'run_list16',
                version: '0.1',
                type: 'recipe'
              },
              children: []
            }
          ]
        }
      ]
    }
  ]
};
