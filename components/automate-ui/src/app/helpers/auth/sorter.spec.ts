


describe('normalSort', () => {

    const inputObject: {} = [
        { checked: false, id: '123abc', name: '123abc', status: 'NO_RULES', type: 'CUSTOM'},
        { checked: false, id: 'project-6', name: 'project-6', status: 'NO_RULES', type: 'CUSTOM'},
        { checked: false, id: '12345zero', name: '12345zero', status: 'NO_RULES', type: 'CUSTOM'},
        { checked: false, id: '123xyz', name: '123xyz', status: 'NO_RULES', type: 'CUSTOM'},
        { checked: false, id: 'abc123', name: 'abc123', status: 'NO_RULES', type: 'CUSTOM'}
      ];

    const outputByName: {} = [
        
    ]

    it('returns a normally sorted array by chosen property', () => {
        expect(ChefSorters.normalSort(inputObject)).toBe(someOutput);
    });
});
