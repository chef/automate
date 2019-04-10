import { StatusSelectorPipe } from './status-selector.pipe';
import {
  NodeHistory,
  SelectedStatus
} from '../types/types';

describe('StatusSelectorPipe', () => {
  let pipe: StatusSelectorPipe;
  const historyLists = [
    new NodeHistory({'_source': {'status': 'success', 'id': '1'}}),
    new NodeHistory({'_source': {'status': 'success', 'id': '2'}}),
    new NodeHistory({'_source': {'status': 'failure', 'id': '3'}})
  ];

  beforeEach(() => {
    pipe = new StatusSelectorPipe();
  });

  describe('historyLists', () => {

    it('returns an empty array when given an empty array and null', () => {
      const items = [];
      const result = pipe.transform(items, null);
      expect(result).toEqual(items);
    });

    it('returns the array unchanged when given null as an arg', () => {
      const result = pipe.transform(historyLists, null);
      expect(result).toEqual(historyLists);
    });

    it('returns items with a status of success when given SelectedStatus.Success', () => {
      const result = pipe.transform(historyLists, SelectedStatus.Success);
      expect(result).toEqual([
        historyLists[0],
        historyLists[1]
      ]);
    });

    it('returns items with a status of failure when given SelectedStatus.Failure', () => {
      const result = pipe.transform(historyLists, SelectedStatus.Failure);
      expect(result).toEqual([
        historyLists[2]
      ]);
    });
  });
});
