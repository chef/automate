require 'spec_helper'
require 'cluster/secrets'
require 'tempfile'

describe AutomateCluster::Secrets do
  let(:secrets) { AutomateCluster::Secrets.instance }

  before do
    AutomateCluster.logger
    secrets.set('some_password', 'rabbid_wombats')
  end

  it 'should fetch secrets' do
    expect(secrets.fetch('some_password')).to eq 'rabbid_wombats'
    expect(secrets.data['some_password']).to_not eq 'rabbid_wombats'
  end

  it 'should set setcrets with array method' do
    secrets['another_password'] = 'moar_passwords'
    expect(secrets.fetch('another_password')).to eq 'moar_passwords'
  end

  it 'should fetch with array method' do
    expect(secrets['some_password']).to eq 'rabbid_wombats'
  end

  it 'should return nil for there is no secret' do
    expect(secrets.fetch('bad_password')).to be_nil
  end


  it 'should be able to access secrets after rotating keys' do
    old_data = secrets.data.dup
    secrets.rotate!

    expect(secrets.fetch('some_password')).to eq 'rabbid_wombats'
    expect(old_data).to_not eq secrets.data
  end

  it 'should clear the secrets' do
    secrets.clear!

    expect(secrets.fetch('some_password')).to be_nil
  end

  it 'should load json output' do
    json = secrets.to_json
    secrets.clear!

    secrets.load_json(json)

    expect(secrets.fetch('some_password')).to eq 'rabbid_wombats'
  end

  it 'should create an instance when loading from file' do
    expect(File).to receive(:exist?).with('a_secret_file.key').and_return(true)
    expect(File).to receive(:exist?).with('secret_store.json').and_return(true)

    expect(File).to receive(:read).with('a_secret_file.key').and_return(secrets.secret_key)
    expect(File).to receive(:read).with('secret_store.json').and_return(secrets.to_json)

    store = AutomateCluster::Secrets.from_file('a_secret_file.key', 'secret_store.json')

    expect(store.fetch('some_password')).to eq 'rabbid_wombats'
  end

  it 'should return an array of secret names' do
    expect(secrets.names).to include('some_password')
  end

  it 'should delete the secret from the store' do
    secrets.delete('some_password')

    expect(secrets['some_password']).to be_nil
  end

  it 'should show a warning if no secret file is found' do
    expect(File).to receive(:exist?).with('invalid_file.key').and_return(false)
    expect(File).to receive(:exist?).with('storage.json').and_return(false)
    expect(AutomateCluster.logger).to receive(:warn).and_return(true)
    expect(AutomateCluster::Secrets).to receive(:generate_key).and_return("SOMEKEY")
    expect(AutomateCluster::Secrets.instance).to receive(:load_key).with("SOMEKEY").and_return(true)
    AutomateCluster::Secrets.from_file('invalid_file.key', 'storage.json')
  end
end
