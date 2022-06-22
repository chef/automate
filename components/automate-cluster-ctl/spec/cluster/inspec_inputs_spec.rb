require 'spec_helper'
require 'cluster/inspec_inputs'

describe AutomateCluster::Logger do
  let(:tmpfile) { double('Tempfile', path: '/test/inspec-inputs.yml', truncate: true, fsync: true) }
  let(:inputs) { AutomateCluster::InspecInputs.new }

  before do
    allow(inputs).to receive(:input_file).and_return(tmpfile)
  end

  it 'should have a path' do
    expect(inputs.save).to eq '/test/inspec-inputs.yml'
  end

  it 'should add a value' do
    inputs.add('test', 'value')
    expect(tmpfile).to receive(:<<).with("---\ntest: value\n").and_return(true)
    inputs.save
  end

  it 'should try to delete the file' do
    expect(tmpfile).to receive(:close).and_return(true)
    expect(tmpfile).to receive(:unlink).and_return(true)

    inputs.delete
  end
end
